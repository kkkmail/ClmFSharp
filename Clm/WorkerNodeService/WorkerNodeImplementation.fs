namespace WorkerNodeService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ClmSys.Registry
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open Clm.CommandLine
open System.IO

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<LocalProcessId, RemoteProcessId>
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                runningWorkers = Map.empty
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            messagingService : IMessagingService
            msgClientProxy : MessagingClientProxy
            workerNodeProxy : WorkerNodeProxy
            logger : Logger
            exeName : string
            minUsefulEe : MinUsefulEe
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.workerNodeAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo
                messagingService = this.messagingService
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type WorkerNodeMessage =
        | Start
        | Register
        | Unregister
        | UpdateProgress of LocalProgressUpdateInfo
        | GetMessages
        | RunModel of WorkerNodeRunModelData
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of WorkerNodeConfigParam


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let className = "WorkerNodeRunner"
        let getMethodName n = className + "." + n
        let onRunModelName = getMethodName "onRunModel"
        let onSaveResultName = getMethodName "onSaveResult"
        let onSaveChartsName = getMethodName "onSaveCharts"
        let onStartName = getMethodName "onStart"
        let onRegisterName = getMethodName "onRegister"
        let onUnregisterName = getMethodName "onUnregister"
        let onUpdateProgressName = getMethodName "onUpdateProgress"
        let onProcessMessageName = getMethodName "onProcessMessage"
        let onGetMessagesName = getMethodName "onGetMessages"
        let onConfigureWorkerName = getMethodName "onConfigureWorker"

        let messagingClient = MessagingClient i.messagingClientData
        do messagingClient.start()

        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage m = messagingClient.sendMessage m
        let logErr = i.logger.logErr
        let proxy = i.workerNodeProxy


        let getSolverRunnerAccessInfo ee =
            {
                wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
                minUsefulEe = ee
            }
            |> WorkerNodeSvcAccessInfo


        let onRunModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
            printfn "%s: d = %A." onRunModelName d

            let a =
                {
                    exeName = i.exeName
                    commandLineParam =
                        {
                            taskParam = d.taskParam
                            serviceAccessInfo = getSolverRunnerAccessInfo d.minUsefulEe
                        }

                    callBackInfo = d.runningProcessData
                }

            match proxy.runModel a with
            | Some result ->
                printfn "%s: Number of running models = %A." onRunModelName (s.runningWorkers.Count + 1)
                proxy.saveWorkerNodeRunModelData { d with localProcessId = Some result.localProcessId }
                { s with runningWorkers = s.runningWorkers.Add(result.localProcessId, d.remoteProcessId) }
            | None ->
                proxy.saveWorkerNodeRunModelData { d with localProcessId = None }
                s


        let onSaveResult (d : ResultDataId) =
            printfn "%s: d = %A." onSaveResultName d
            match proxy.tryLoadResultData d with
            | Some r ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = r |> SaveResultPrtMsg
                }.getMessageInfo()
                |> sendMessage

                proxy.tryDeleteResultData d |> ignore
            | None -> logErr (sprintf "%s: Unable to find result with resultDataId: %A" onSaveResultName d)


        let onSaveCharts (d : ResultDataId) =
            printfn "%s: d = %A." onSaveChartsName d
            match proxy.tryLoadChartInfo d with
            | Some c ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = c |> SaveChartsPrtMsg
                        //{
                        //    c with charts = c.charts |> List.map (fun e -> { e with chartName = Path.GetFileNameWithoutExtension e.chartName })
                        //} |> SaveChartsPrtMsg
                }.getMessageInfo()
                |> sendMessage

                try
                    c.charts
                    |> List.map (fun e -> if File.Exists e.chartName then File.Delete e.chartName)
                    |> ignore
                with
                | ex ->
                    i.logger.logExn onSaveChartsName ex

                proxy.tryDeleteChartInfo d |> ignore
            | None -> logErr (sprintf "%s: Unable to find charts with resultDataId: %A" onSaveChartsName d)


        let onStart s =
            printfn "%s" onStartName
            let m = proxy.loadAllWorkerNodeRunModelData()
            let r = proxy.loadAllResultData()

            let runIfNoResult g w =
                let d = w.remoteProcessId.toResultDataId()

                match r |> List.tryFind (fun e -> e.resultDataId = d) with
                | Some _ ->
                    {
                        partitionerRecipient = partitioner
                        deliveryType = GuaranteedDelivery
                        messageData =
                            {
                                remoteProcessId = w.remoteProcessId
                                runningProcessData = w.runningProcessData
                                progress = Completed
                            }
                            |> UpdateProgressPrtMsg
                    }.getMessageInfo()
                    |> sendMessage

                    proxy.tryDeleteWorkerNodeRunModelData w.remoteProcessId |> ignore
                    proxy.tryDeleteModelData w.runningProcessData.modelDataId |> ignore
                    onSaveResult d
                    onSaveCharts d
                    g
                | None -> onRunModel g w

            let tryRunModel g w =
                match w.localProcessId with
                | Some v ->
                    match tryGetProcessById v with
                    | Some p ->
                        match tryGetProcessName p with
                        | Some n when n = SolverRunnerName ->
                            { g with runningWorkers = s.runningWorkers.Add(v, w.remoteProcessId) }
                        | _ -> runIfNoResult g w
                    | None -> runIfNoResult g w
                | None -> onRunModel g w

            m |> List.fold (fun acc e -> tryRunModel acc e) s


        let onRegister s =
            printfn "%s" onRegisterName
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> sendMessage

            s


        let onUnregister s =
            printfn "%s" onUnregisterName
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> sendMessage

            s


        let onUpdateProgress s (p : LocalProgressUpdateInfo) =
            printfn "%s: p = %A." onUpdateProgressName p
            let updateProgress t c =
                match s.runningWorkers.TryFind p.localProcessId with
                | Some r ->
                    let q =
                        {
                            remoteProcessId = r
                            runningProcessData = p.runningProcessData
                            progress = p.progress
                        }
                    {
                        partitionerRecipient = partitioner
                        deliveryType = t
                        messageData = UpdateProgressPrtMsg q
                    }.getMessageInfo()
                    |> sendMessage

                    if c
                    then
                        printfn "%s: Calling tryDeleteWorkerNodeRunModelData and tryDeleteModelData..." onUpdateProgressName
                        proxy.tryDeleteWorkerNodeRunModelData r |> ignore
                        proxy.tryDeleteModelData p.runningProcessData.modelDataId |> ignore
                        p.runningProcessData.toResultDataId() |> onSaveResult
                        p.runningProcessData.toResultDataId() |> onSaveCharts
                | None -> logErr (sprintf "%s: Unable to find mapping from local process %A." onUpdateProgressName p.localProcessId)

                if c
                then { s with runningWorkers = s.runningWorkers.tryRemove p.localProcessId }
                else s


            let (t, c) =
                match p.progress with
                | NotStarted -> (NonGuaranteedDelivery, false)
                | InProgress _ -> (NonGuaranteedDelivery, false)
                | Completed -> (GuaranteedDelivery, true)

            updateProgress t c


        let tryFindRunningModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
            s.runningWorkers
            |> Map.toList
            |> List.map (fun (_, v) -> v)
            |> List.tryFind (fun e -> e = d.remoteProcessId)


        let onProcessMessage s (m : Message) =
            printfn "%s: m.messageId = %A." onProcessMessageName m.messageDataInfo.messageId
            match m.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg (d, m) ->
                    proxy.saveModelData m

                    match tryFindRunningModel s d with
                    | None -> onRunModel s d
                    | Some r ->
                        logErr (sprintf "%s: !!! ERROR !!! - found running model for remoteProcessId = %A" onProcessMessageName r.value)
                        s
            | _ ->
                logErr (sprintf "%s: Invalid message type: %A." onProcessMessageName m.messageData)
                s


        let onGetMessages s =
            async {
                printfn "%s" onGetMessagesName
                printfn "%s: WorkerNodeRunnerState: %A" onGetMessagesName s
                return! List.foldWhileSomeAsync (fun x () -> messagingClient.tryProcessMessage x onProcessMessage) WorkerNodeRunnerState.maxMessages s
            }


        let onConfigureWorker s d =
            match d with
            | WorkerNumberOfSores c ->
                printfn "%s" onConfigureWorkerName
                let cores = max 0 (min c Environment.ProcessorCount)
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = { i.workerNodeAccessInfo.workerNodeInfo with noOfCores = cores} |> RegisterWorkerNodePrtMsg
                }.getMessageInfo()
                |> sendMessage

            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! timed onStartName onStart s |> loop
                            | Register -> return! timed onRegisterName onRegister s |> loop
                            | Unregister -> return! timed onUnregisterName onUnregister s |> loop
                            | UpdateProgress p -> return! timed onUpdateProgressName onUpdateProgress s p |> loop
                            | GetMessages ->
                                let! ns = onGetMessages s
                                return! ns |> loop
                            | RunModel d -> return! timed onRunModelName onRunModel s d |> loop
                            | GetState w -> w.Reply s
                            | ConfigureWorker d -> return! timed onConfigureWorkerName onConfigureWorker s d |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.unregister() = Unregister |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        member __.getMessages() = GetMessages |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState
        member __.configure d = d |> ConfigureWorker |> messageLoop.Post


    let createServiceImpl i =
        i.logger.logInfo "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeAccessInfo.isInactive with
        | false ->
            i.logger.logInfo "createServiceImpl: Registering..."
            do w.register()
            do w.start()
            let h = new EventHandler(EventHandlerInfo.defaultValue (i.logger.logExn "WorkerNodeRunner") w.getMessages)
            do h.start()
            Some w
        | true ->
            i.logger.logInfo "createServiceImpl: Unregistering..."
            do w.unregister()
            do w.getState() |> ignore
            None


    type WorkerNodeService () =
        inherit MarshalByRefObject()
        let logger = Logger.log4net
        let className = "WorkerNodeService"

        let w =
            let h = MsgResponseHandler (serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo)
            logger.logInfo (sprintf "%s: Created MsgResponseHandler: %A" className h)

            {
                workerNodeAccessInfo = serviceAccessInfo
                messagingService = h
                msgClientProxy = MessagingClientProxy { messagingClientName = workerNodeServiceName }
                workerNodeProxy = WorkerNodeProxy WorkerNodeProxyInfo.defaultValue
                logger = logger
                exeName = SolverRunnerName
                minUsefulEe = MinUsefulEe.defaultValue

            }
            |> createServiceImpl

        let initService () = ()
        do initService ()


        let updateLocalProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "%s: Failed to update progress: %A" className p)


        let configureImpl d =
            match w with
            | Some r -> r.configure d
            | None -> logger.logErr (sprintf "%s: Failed to configure service: %A" className d)


        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = ignore()
            member __.configure d = configureImpl d

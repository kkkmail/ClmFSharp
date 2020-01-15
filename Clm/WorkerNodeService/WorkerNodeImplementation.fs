namespace WorkerNodeService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open ClmSys
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
open ServiceProxy.MsgProcessorProxy
open ClmSys.MessagingData

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeRunnerState
        with

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                runningWorkers = Map.empty
                numberOfCores = 0
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            workerNodeProxy : WorkerNodeProxy
            messageProcessorProxy : MessageProcessorProxy
            exeName : string
            minUsefulEe : MinUsefulEe
        }


    type WorkerNodeMessage =
        | Start
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | UpdateProgress of LocalProgressUpdateInfo
        | GetMessages
        //| RunModel of WorkerNodeRunModelData
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of WorkerNodeConfigParam


    type OnSaveResultProxy =
        {
            partitionerId : PartitionerId
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId>
            tryDeleteResultData : ResultDataId -> UnitResult
            sendMessage : MessageInfo -> UnitResult
        }


    type OnSaveChartsProxy =
        {
            partitionerId : PartitionerId
            tryLoadChartInfo : ResultDataId -> ClmResult<ChartInfo>
            tryDeleteChartInfo : ResultDataId -> UnitResult
            sendMessage : MessageInfo -> UnitResult
        }


    type OnRegisterProxy =
        {
            partitionerId : PartitionerId
            workerNodeInfo : WorkerNodeInfo
            sendMessage : MessageInfo -> UnitResult
        }


    let private toError e = e |> WorkerNodeErr |> Error


    let getSolverRunnerAccessInfo (i : WorkerNodeRunnerData) ee =
        {
            wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
            minUsefulEe = ee
        }
        |> WorkerNodeSvcAccessInfo


    let onSaveResult (proxy : OnSaveResultProxy) (d : ResultDataId) =
        let toError f e = ((f |> OnSaveResultErr |> WorkerNodeErr) + e) |> Error

        match proxy.tryLoadResultData d with
        | Ok r ->
            let send() =
                {
                    partitionerRecipient = proxy.partitionerId
                    deliveryType = GuaranteedDelivery
                    messageData = r |> SaveResultPrtMsg
                }.getMessageInfo()
                |> proxy.sendMessage

            match send() with
            | Ok() ->
                match proxy.tryDeleteResultData d with
                | Ok() -> Ok()
                | Error e -> toError (DeleteResultDataError d.value) e
            | Error e -> toError (SendResultMessageError (proxy.partitionerId.messagingClientId.value, d.value)) e
        | Error e -> toError (LoadResultDataError d.value) e


    let onSaveCharts (proxy : OnSaveChartsProxy) (d : ResultDataId) =
        let toError f e = ((f |> OnSaveChartsErr |> WorkerNodeErr) + e) |> Error

        match proxy.tryLoadChartInfo d with
        | Ok c ->
            let send() =
                {
                    partitionerRecipient = proxy.partitionerId
                    deliveryType = GuaranteedDelivery
                    messageData = c |> SaveChartsPrtMsg
                }.getMessageInfo()
                |> proxy.sendMessage

            match send() with
            | Ok() ->
                let r() =
                    try
                        c.charts
                        |> List.map (fun e -> if File.Exists e.chartName then File.Delete e.chartName)
                        |> ignore
                        Ok()
                    with
                    | ex -> ex |> DeleteChartError |> OnSaveChartsErr |> WorkerNodeErr |> Error

                match (r(), proxy.tryDeleteChartInfo d) ||> combineUnitResults with
                | Ok() -> Ok()
                | Error e -> toError (DeleteChartInfoError d.value) e
            | Error e -> toError (SendChartMessageError (proxy.partitionerId.messagingClientId.value, d.value)) e
        | Error e -> toError (LoadChartInfoError d.value) e


    let onRegister (proxy : OnRegisterProxy) s (r : AsyncReplyChannel<UnitResult>) =
        let result =
            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage

        r.Reply result
        s


    let onUnregister (proxy : OnRegisterProxy) s (r : AsyncReplyChannel<UnitResult>) =
        let result =
            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage

        r.Reply result
        s


    type OnUpdateProgressProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
            onSaveResult : ResultDataId -> UnitResult
            onSaveCharts : ResultDataId -> UnitResult
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult
        }


    let toDeliveryType progress =
        match progress with
        | NotStarted -> (NonGuaranteedDelivery, false)
        | InProgress _ -> (NonGuaranteedDelivery, false)
        | Completed -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)


    let getUpdateProgressResult (send : unit -> UnitResult) (proxy : OnUpdateProgressProxy) (p : LocalProgressUpdateInfo) (r : RunnerState) c =
        [
            send()

            if c
            then
                proxy.tryDeleteWorkerNodeRunModelData r.runnerRemoteProcessId
                proxy.tryDeleteModelData p.runningProcessData.modelDataId
                p.runningProcessData.toResultDataId() |> proxy.onSaveResult
                p.runningProcessData.toResultDataId() |> proxy.onSaveCharts
        ]
        |> foldUnitResults


    let onUpdateProgress (proxy : OnUpdateProgressProxy) s (p : LocalProgressUpdateInfo) =
        let updateProgress t c =
            let rso, result =
                match s.runningWorkers |> Map.tryFind p.localProcessId with
                | Some r ->
                    let send() =
                        {
                            partitionerRecipient = proxy.partitionerId
                            deliveryType = t
                            messageData = UpdateProgressPrtMsg { remoteProcessId = r.runnerRemoteProcessId; runningProcessData = p.runningProcessData; progress = p.progress }
                        }.getMessageInfo()
                        |> proxy.sendMessage

                    let result = getUpdateProgressResult send proxy p r c
                    Some { r with progress = p.progress; lastUpdated = DateTime.Now }, result
                | None -> None, p.localProcessId.value |> UnableToFindMappingError |> OnUpdateProgressErr |> WorkerNodeErr |> Error

            if c
            then { s with runningWorkers = s.runningWorkers.tryRemove p.localProcessId }, result
            else
                match rso with
                | Some rs -> { s with runningWorkers = s.runningWorkers.Add(p.localProcessId, rs) }, result
                | None -> s, result


        let (t, c) = toDeliveryType p.progress
        let w, result = updateProgress t c
        w


    let getRunModelParam (d : WorkerNodeRunModelData) =
        {
            exeName = i.exeName
            commandLineParam =
                {
                    taskParam = d.taskParam
                    serviceAccessInfo = getSolverRunnerAccessInfo d.minUsefulEe
                }

            callBackInfo = { d.runningProcessData with workerNodeId = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId }
        }


    let onRunModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        printfn "%s: d = %A." onRunModelName d
        let a = getRunModelParam d

        if s.numberOfCores > s.runningWorkers.Count
        then
            match proxy.runModel a with
            | Some result ->
                printfn "%s: Number of running models = %A." onRunModelName (s.runningWorkers.Count + 1)
                proxy.saveWorkerNodeRunModelData { d with localProcessId = Some result.localProcessId; commandLine = proxy.getCommandLine a }
        
                let rs =
                    {
                        runnerRemoteProcessId = d.remoteProcessId
                        progress = TaskProgress.NotStarted
                        started = DateTime.Now
                        lastUpdated = DateTime.Now
                    }

                { s with runningWorkers = s.runningWorkers.Add(result.localProcessId, rs) }
            | None ->
                proxy.saveWorkerNodeRunModelData { d with localProcessId = None; commandLine = proxy.getCommandLine a }
                s
        else
            let q =
                {
                    remoteProcessId = a.callBackInfo.runQueueId.toRemoteProcessId()
                    runningProcessData = a.callBackInfo
                    progress = Failed (sprintf "Worker node: %A exceeded number of available cores." i.workerNodeAccessInfo.workerNodeInfo.workerNodeId)
                }
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = UpdateProgressPrtMsg q
            }.getMessageInfo()
            |> sendMessage

            s


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
                    printfn "%s: Found process with id: %A" onStartName v

                    match tryGetProcessName p with
                    | Some n when n = SolverRunnerProcessName ->
                        printfn "%s: Found process with name: %s" onStartName n

                        let rs =
                            {
                                runnerRemoteProcessId = w.remoteProcessId
                                progress = TaskProgress.NotStarted
                                started = DateTime.Now
                                lastUpdated = DateTime.Now
                            }

                        { g with runningWorkers = s.runningWorkers.Add(v, rs) }
                    | m ->
                        printfn "%s: CANNOT find process with name: %s, but found %A" onStartName SolverRunnerProcessName m
                        runIfNoResult g w
                | None -> runIfNoResult g w
            | None -> onRunModel g w

        m |> List.fold (fun acc e -> tryRunModel acc e) { s with numberOfCores = i.workerNodeAccessInfo.workerNodeInfo.noOfCores }


    let tryFindRunningModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        s.runningWorkers
        |> Map.toList
        |> List.map (fun (_, v) -> v)
        |> List.tryFind (fun e -> e.runnerRemoteProcessId = d.remoteProcessId)


    let onProcessMessage s (m : Message) =
        printfn "%s: m.messageId = %A." onProcessMessageName m.messageDataInfo.messageId
        match m.messageData with
        | WorkerNodeMsg x ->
            match x with
            | RunModelWrkMsg (d, m) ->
                match tryFindRunningModel s d with
                | None ->
                    proxy.saveModelData m
                    onRunModel s d
                | Some r ->
                    logErr (sprintf "%s: !!! ERROR !!! - found running model for remoteProcessId = %A" onProcessMessageName r.runnerRemoteProcessId.value)
                    s
        | _ ->
            logErr (sprintf "%s: Invalid message type: %A." onProcessMessageName m.messageData)
            s


    let onGetMessages s =
        printfn "%s" onGetMessagesName
        printfn "%s: WorkerNodeRunnerState: %A" onGetMessagesName s
        List.foldWhileSome (fun x () -> tryProcessMessage x onProcessMessage) WorkerNodeRunnerState.maxMessages s


    let onGetState s (r : AsyncReplyChannel<WorkerNodeRunnerState>) =
        r.Reply s
        s


    let onConfigureWorker (s : WorkerNodeRunnerState) d =
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

            { s with numberOfCores = cores }





    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage = i.messageProcessorProxy.sendMessage
        let tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
        //let logErr = i.logger.logErr
        //let logExn = i.logger.logExn
        let proxy = i.workerNodeProxy


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
                            | GetMessages -> return! onGetMessages s |> loop
                            | GetState r -> return! timed onGetStateName onGetState s r |> loop
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
            let messagingClientAccessInfo = serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo
            let h = MsgResponseHandler messagingClientAccessInfo
            logger.logInfo (sprintf "%s: Created MsgResponseHandler: %A" className h)

            let messagingClientData =
                {
                    msgAccessInfo = messagingClientAccessInfo
                    messagingService = h
                    msgClientProxy = MessagingClientProxy.create { messagingClientName = workerNodeServiceName }
                    logger = logger
                }

            let messagingClient = MessagingClient messagingClientData
            do messagingClient.start()

            {
                workerNodeAccessInfo = serviceAccessInfo
                workerNodeProxy = WorkerNodeProxy.create WorkerNodeProxyInfo.defaultValue
                messageProcessorProxy = messagingClient.messageProcessorProxy
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


        let monitorImpl p =
            match w with
            | Some r ->
                let s = r.getState()
                WrkNodeState s
            | None ->
                logger.logErr (sprintf "%s: Failed to monitor service: %A" className p)
                CannotAccessWrkNode


        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = ignore()
            member __.configure d = configureImpl d
            member __.monitor p = monitorImpl p

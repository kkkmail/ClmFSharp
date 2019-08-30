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
            running : Map<LocalProcessId, RemoteProcessId>
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                running = Map.empty
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            workerNodeProxy : WorkerNodeProxy
            logger : Logger
            exeName : string
            minUsefulEe : MinUsefulEe
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.workerNodeAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo
                msgResponseHandler = this.msgResponseHandler
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type WorkerNodeMessage =
        | Start of WorkerNodeRunner
        | Register
        | Unregister
        | UpdateProgress of WorkerNodeRunner * LocalProgressUpdateInfo
        | SaveResult of ResultDataId
        | SaveCharts of ResultDataId
        | GetMessages of WorkerNodeRunner
        | RunModel of WorkerNodeRunModelData
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>


    and WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let messagingClient = MessagingClient i.messagingClientData
        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage m = messagingClient.sendMessage m
        let logErr = i.logger.logErr


        let getSolverRunnerAccessInfo ee =
            {
                wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
                minUsefulEe = ee
            }
            |> WorkerNodeSvcAccessInfo


        let onStart s (r : WorkerNodeRunner) =
            printfn "WorkerNodeRunner.onStart"
            i.workerNodeProxy.loadAllWorkerNodeRunModelData()
            |> List.map (fun e -> r.runModel e)
            |> ignore

            s


        let onRegister s =
            printfn "WorkerNodeRunner.onRegister"
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onUnregister s =
            printfn "WorkerNodeRunner.onUnregister"
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onUpdateProgress s (w : WorkerNodeRunner) (p : LocalProgressUpdateInfo) =
            printfn "WorkerNodeRunner.onUpdateProgress: p = %A." p
            let updateProgress t c =
                match s.running.TryFind p.updatedLocalProcessId with
                | Some r ->
                    let q =
                        {
                            updatedRemoteProcessId = r
                            updateModelId = p.updateModelId
                            progress = p.progress
                            resultDataId = p.resultDataId
                        }
                    {
                        partitionerRecipient = partitioner
                        deliveryType = t
                        messageData = UpdateProgressPrtMsg q
                    }.messageInfo
                    |> sendMessage

                    if c
                    then
                        printfn "WorkerNodeRunner.onUpdateProgress: Calling tryDeleteWorkerNodeRunModelData and tryDeleteModelData..."
                        //onCompleted
                        i.workerNodeProxy.tryDeleteWorkerNodeRunModelData r |> ignore
                        i.workerNodeProxy.tryDeleteModelData p.updateModelId |> ignore
                        w.saveResult p.resultDataId
                        w.saveCharts p.resultDataId
                | None -> logErr (sprintf "Unable to find mapping from local process %A." p.updatedLocalProcessId)

                if c
                then { s with running = s.running.tryRemove p.updatedLocalProcessId }
                else s


            let (t, c) =
                match p.progress with
                | NotStarted -> (NonGuaranteedDelivery, false)
                | InProgress _ -> (NonGuaranteedDelivery, false)
                | Completed -> (GuaranteedDelivery, true)

            updateProgress t c


        let onSaveResult s (d : ResultDataId) =
            printfn "WorkerNodeRunner.onSaveResult: d = %A." d
            match i.workerNodeProxy.tryLoadResultData d with
            | Some r ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = r |> SaveResultPrtMsg
                }.messageInfo
                |> sendMessage

                i.workerNodeProxy.tryDeleteResultData d |> ignore
            | None -> logErr (sprintf "Unable to find result with resultDataId: %A" d)

            s


        let onSaveCharts s (d : ResultDataId) =
            printfn "WorkerNodeRunner.onSaveCharts: d = %A." d
            match i.workerNodeProxy.tryLoadChartInfo d with
            | Some c ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = c |> SaveChartsPrtMsg
                        //{
                        //    c with charts = c.charts |> List.map (fun e -> { e with chartName = Path.GetFileNameWithoutExtension e.chartName })
                        //} |> SaveChartsPrtMsg
                }.messageInfo
                |> sendMessage

                try
                    c.charts
                    |> List.map (fun e -> if File.Exists e.chartName then File.Delete e.chartName)
                    |> ignore
                with
                    | ex ->
                        i.logger.logExn "onSaveCharts - Exception occurred:" ex

                i.workerNodeProxy.tryDeleteChartInfo d |> ignore
            | None -> logErr (sprintf "Unable to find charts with resultDataId: %A" d)

            s


        let onProcessMessage (w : WorkerNodeRunner) s (m : Message) =
            printfn "WorkerNodeRunner.onProcessMessage: m.messageId = %A." m.messageId
            match m.messageInfo.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg (d, m) ->
                    i.workerNodeProxy.saveModelData m
                    w.runModel d
            | _ -> i.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)

            s


        let onGetMessages s (w : WorkerNodeRunner) =
            printfn "WorkerNodeRunner.onGetMessages"
            printfn "WorkerNodeRunnerState: %A" s
            List.foldWhileSome (fun x () -> messagingClient.tryProcessMessage x (onProcessMessage w)) WorkerNodeRunnerState.maxMessages s


        let onRunModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
            printfn "WorkerNodeRunner.onRunModel: d = %A." d
            i.workerNodeProxy.saveWorkerNodeRunModelData d

            let a =
                {
                    exeName = i.exeName
                    commandLineParam =
                        {
                            taskParam = d.taskParam
                            serviceAccessInfo = getSolverRunnerAccessInfo d.minUsefulEe
                        }

                    callBackInfo =
                        {
                            modelDataId = d.modelDataId
                            resultDataId = d.resultDataId
                            runQueueId = d.runQueueId
                        }
                }

            match i.workerNodeProxy.runModel a with
            | Some result -> { s with running = s.running.Add(result.localProcessId, d.remoteProcessId) }
            | None -> s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! onStart s w |> loop
                            | Register -> return! onRegister s |> loop
                            | Unregister -> return! onUnregister s |> loop
                            | UpdateProgress (w, p) -> return! onUpdateProgress s w p |> loop
                            | SaveResult r -> return! onSaveResult s r |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | RunModel d -> return! onRunModel s d |> loop
                            | GetState w -> w.Reply s
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member this.start() = Start this |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.unregister() = Unregister |> messageLoop.Post
        member this.updateProgress p = UpdateProgress (this, p) |> messageLoop.Post
        member __.saveResult r = SaveResult r |> messageLoop.Post
        member __.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member private __.runModel d = RunModel d |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeAccessInfo.isInactive with
        | false ->
            printfn "createServiceImpl: Registering..."
            do w.register()
            do w.start()
            let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
            do h.start()
            Some w
        | true ->
            printfn "createServiceImpl: Unregistering..."
            do w.unregister()
            do w.getState() |> ignore
            None


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w =
            match MsgResponseHandler.tryCreate serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo with
            | Some h ->
                printfn "WorkerNodeService: Created MsgResponseHandler: %A" h
                {
                    workerNodeAccessInfo = serviceAccessInfo
                    msgResponseHandler = h
                    msgClientProxy = MessagingClientProxy { messagingClientName = workerNodeServiceName }
                    workerNodeProxy = WorkerNodeProxy WorkerNodeProxyInfo.defaultValue
                    logger = logger
                    exeName = SolverRunnerName
                    minUsefulEe = MinUsefulEe.defaultValue

                }
                |> createServiceImpl
            | None ->
                printfn "WorkerNodeService: Cannot create MsgResponseHandler."
                None

        let initService () = ()
        do initService ()

        let updateLocalProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "Failed to update progress: %A" p)

        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = ignore()

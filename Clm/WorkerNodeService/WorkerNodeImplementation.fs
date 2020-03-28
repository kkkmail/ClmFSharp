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
open System.IO
open ServiceProxy.MsgProcessorProxy
open Clm.CalculationData
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives
open ClmSys.MessagingPrimitives
open ServiceProxy.SolverRunner
open NoSql.FileSystemTypes
open ClmSys.Rop
open System.Threading
open SolverRunner.SolverRunnerTasks

module ServiceImplementation =

    let private toError g f = f |> g |> WorkerNodeErr |> Error
    let private addError g f e = ((f |> g |> WorkerNodeErr) + e) |> Error


    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessageResult = MessageProcessorResult<WorkerNodeRunnerState * UnitResult>


    type WorkerNodeRunnerState
        with
    
        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]
    
        static member defaultValue =
            {
                runningWorkers = Map.empty
                numberOfWorkerCores = 0
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            workerNodeProxy : WorkerNodeProxy
            messageProcessorProxy : MessageProcessorProxy
            minUsefulEe : MinUsefulEe
        }


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
        }


    let onSaveResult (proxy : SendMessageProxy) r =
        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> bindError (addError OnSaveResultErr (SendResultMessageError (proxy.partitionerId.messagingClientId, r.resultDataId)))


    let onSaveCharts (proxy : SendMessageProxy) r =
        match r with
        | GeneratedCharts c ->
            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage
            |> bindError (addError OnSaveChartsErr (SendChartMessageError (proxy.partitionerId.messagingClientId, c.resultDataId)))
        | NotGeneratedCharts -> Ok()


    let onRegister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    let onUnregister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    let toDeliveryType progress =
        match progress with
        | NotStarted -> (NonGuaranteedDelivery, false)
        | InProgress _ -> (NonGuaranteedDelivery, false)
        | Completed c -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)


    let onUpdateProgress (proxy : OnUpdateProgressProxy) s (p : ProgressUpdateInfo) =
        let updateProgress t completed =
            let rso, result =
                match s.runningWorkers |> Map.tryFind p.runQueueId with
                | Some rs ->
                    let result =
                        {
                            partitionerRecipient = proxy.sendMessageProxy.partitionerId
                            deliveryType = t
                            messageData = UpdateProgressPrtMsg p
                        }.getMessageInfo()
                        |> proxy.sendMessageProxy.sendMessage
                        |> bindError (addError OnUpdateProgressErr (UnableToSendProgressMsgErr p.runQueueId))

                    Some { rs with progress = p.progress; lastUpdated = DateTime.Now }, result
                | None -> None, p.runQueueId |> UnableToFindMappingError |> OnUpdateProgressErr |> WorkerNodeErr |> Error

            if completed
            then
                let r2 = proxy.tryDeleteWorkerNodeRunModelData p.runQueueId
                { s with runningWorkers = s.runningWorkers.tryRemove p.runQueueId }, combineUnitResults result r2
            else
                match rso with
                | Some rs -> { s with runningWorkers = s.runningWorkers.Add(p.runQueueId, rs) }, result
                | None -> s, result

        let (t, completed) = toDeliveryType p.progress
        let w, result = updateProgress t completed
        w, result


    type OnRunModelProxy =
        {
            workerNodeId : WorkerNodeId
            runModel : WorkerNodeRunModelData -> unit
            sendMessageProxy : SendMessageProxy
        }


    let onRunModel (proxy : OnRunModelProxy) (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        let w, result =
            match s.numberOfWorkerCores > s.runningWorkers.Count with
            | true ->
                let c = new CancellationTokenSource()
                let m = async { proxy.runModel d }
                Async.Start (m, c.Token)

                let rs =
                    {
                        progress = TaskProgress.NotStarted
                        started = DateTime.Now
                        lastUpdated = DateTime.Now
                        cancellationTokenSource = c
                    }

                { s with runningWorkers = s.runningWorkers.Add(d.runningProcessData.runQueueId, rs) }, Ok()
            | false ->
                let res =
                    {
                        partitionerRecipient = proxy.sendMessageProxy.partitionerId
                        deliveryType = GuaranteedDelivery
                        messageData = UpdateProgressPrtMsg { runQueueId = d.runningProcessData.runQueueId; progress = Failed "" }
                    }.getMessageInfo()
                    |> proxy.sendMessageProxy.sendMessage

                s, res

        w, result |> bindError (addError OnRunModelErr CannotRunModel)


    type OnStartProxy =
        {
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }


    let onStart (proxy : OnStartProxy) s =
        let doStart mi =
            let m, mf = mi |> Rop.unzip

            match foldErrors mf with
            | None -> (m, s) ||> Rop.foldWhileOk proxy.onRunModel
            | Some e -> s, Error e

        let g, result =
            match proxy.loadAllWorkerNodeRunModelData() with
            | Ok mi -> doStart mi
            | Error e -> s, Error e

        g, result


    type OnProcessMessageProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }


    let onProcessMessage (proxy : OnProcessMessageProxy) s (m : Message) =
        let addError = addError OnProcessMessageErr
        let toError = toError OnProcessMessageErr

        let w, result =
            match m.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg d ->
                    match s.runningWorkers |> Map.tryFind d.runningProcessData.runQueueId with
                    | None ->
                        match proxy.saveWorkerNodeRunModelData d with
                        | Ok() ->
                            let w1, r1 = proxy.onRunModel s d
                            w1, r1
                        | Error e -> s, addError CannotSaveModelData e
                    | Some _ -> s, d.runningProcessData.runQueueId |> ModelAlreadyRunning |> toError
            | _ -> s, (m.messageDataInfo.messageId, m.messageData.getInfo()) |> InvalidMessage |> toError

        w, result


    type OnProcessMessageType = OnProcessMessageType<WorkerNodeRunnerState>
    type OnGetMessagesProxy = OnGetMessagesProxy<WorkerNodeRunnerState>
    let onGetMessages = onGetMessages<WorkerNodeRunnerState>
    let onGetState s = s, s


    type OnConfigureWorkerProxy = OnRegisterProxy


    let onConfigureWorker (proxy : OnConfigureWorkerProxy) (s : WorkerNodeRunnerState) d =
        match d with
        | WorkerNumberOfSores c ->
            let cores = max 0 (min c Environment.ProcessorCount)

            let send() =
                {
                    partitionerRecipient = proxy.sendMessageProxy.partitionerId
                    deliveryType = GuaranteedDelivery
                    messageData = { proxy.workerNodeInfo with nodeInfo = { proxy.workerNodeInfo.nodeInfo with noOfCores = cores } } |> RegisterWorkerNodePrtMsg
                }.getMessageInfo()
                |> proxy.sendMessageProxy.sendMessage

            let w, result =
                match send() with
                | Ok() -> { s with numberOfWorkerCores = cores }, Ok()
                | Error e -> s, Error e

            w, result


    let sendMessageProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onRunModelProxy i p =
        {
            workerNodeId = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId
            runModel = runSolver p
            sendMessageProxy = sendMessageProxy i
        }


    //let onStartProxy i p =
    //    {
    //        loadAllWorkerNodeRunModelData = i.workerNodeProxy.loadAllWorkerNodeRunModelData
    //        onRunModel = onRunModel (onRunModelProxy i p)
    //    }


    let onRegisterProxy i : OnRegisterProxy =
        {
            workerNodeInfo = i.workerNodeAccessInfo.workerNodeInfo
            sendMessageProxy = sendMessageProxy i
        }


    let onUpdateProgressProxy i =
        {
            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
            sendMessageProxy = sendMessageProxy i
        }


    let onProcessMessageProxy i p =
        {
            saveWorkerNodeRunModelData = i.workerNodeProxy.saveWorkerNodeRunModelData
            onRunModel = onRunModel (onRunModelProxy i p)
        }


    //let onGetMessagesProxy i p =
    //    {
    //        tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
    //        onProcessMessage = onProcessMessage (onProcessMessageProxy i p)
    //        maxMessages = WorkerNodeRunnerState.maxMessages
    //        onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
    //    }


    type WorkerNodeMessage =
        | Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | UpdateProgress of AsyncReplyChannel<UnitResult> * ProgressUpdateInfo
        | SaveResult of AsyncReplyChannel<UnitResult> * ResultDataWithId
        | SaveCharts of AsyncReplyChannel<UnitResult> * ChartGenerationResult
        | GetMessages of OnGetMessagesProxy * AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of AsyncReplyChannel<UnitResult> * WorkerNodeConfigParam


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        //let onStartProxy = onStartProxy i
        let onRegisterProxy = onRegisterProxy i
        let onUpdateProgressProxy = onUpdateProgressProxy i
        //let onGetMessagesProxy = onGetMessagesProxy i
        let onConfigureWorkerProxy = onRegisterProxy
        let sendMessageProxy = sendMessageProxy i

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (p, r) -> return! onStart p s |> (withReply r) |> loop
                            | Register r -> return! onRegister onRegisterProxy s |> (withReply r) |> loop
                            | Unregister r -> return! onUnregister onRegisterProxy s |> (withReply r) |> loop
                            | UpdateProgress (r, p) -> return! onUpdateProgress onUpdateProgressProxy s p |> (withReply r) |> loop
                            | SaveResult (r, p) -> return! (s, onSaveResult sendMessageProxy p) |> (withReply r) |> loop
                            | SaveCharts (r, c) -> return! (s, onSaveCharts sendMessageProxy c) |> (withReply r) |> loop
                            | GetMessages (p, r) -> return! onGetMessages p s |> (withReply r) |> loop
                            | GetState r -> return! onGetState s |> (withReply r) |> loop
                            | ConfigureWorker (r, d) -> return! onConfigureWorker onConfigureWorkerProxy s d |> (withReply r) |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member w.start() = messageLoop.PostAndReply (fun reply -> Start (w.onStartProxy, reply))
        member _.register() = messageLoop.PostAndReply Register
        member _.unregister() = messageLoop.PostAndReply Unregister
        member _.updateProgress p = messageLoop.PostAndReply (fun r -> UpdateProgress (r, p))
        member _.saveResult p = messageLoop.PostAndReply (fun r -> SaveResult (r, p))
        member _.saveCharts p = messageLoop.PostAndReply (fun r -> SaveCharts (r, p))
        member w.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages (w.onGetMessagesProxy, reply))
        member _.getState () = messageLoop.PostAndReply GetState
        member _.configure d = messageLoop.PostAndReply (fun r -> ConfigureWorker (r, d))

        member w.solverRunnerProxy =
            {
                updateProgress = w.updateProgress
                saveResult = w.saveResult
                saveCharts = w.saveCharts
                logCrit = i.workerNodeProxy.logCrit
            }

        member w.onStartProxy =
            {
                loadAllWorkerNodeRunModelData = i.workerNodeProxy.loadAllWorkerNodeRunModelData
                onRunModel = onRunModel (onRunModelProxy i w.solverRunnerProxy)
            }

        member w.onGetMessagesProxy =
            {
                tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
                onProcessMessage = onProcessMessage (onProcessMessageProxy i w.solverRunnerProxy)
                maxMessages = WorkerNodeRunnerState.maxMessages
                onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
            }





    let createServiceImpl (logger : Logger) (i : WorkerNodeRunnerData) =
        logger.logInfoString "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeAccessInfo.isInactive with
        | false ->
            logger.logInfoString "createServiceImpl: Registering..."
            match w.register >-> w.start |> evaluate with
            | Ok() ->
                let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger w.getMessages "WorkerNodeRunner - getMessages")
                do h.start()
                Ok (Some w)
            | Error e -> Error e
        | true ->
            logger.logInfoString "createServiceImpl: Unregistering..."
            match w.unregister() with
            | Ok() -> Ok None
            | Error e -> Error e


    let getErrName (RunQueueId r) = "SolverRunnerErr\\" + r.ToString() |> MessagingClientName


    let createSolverRunnerProxy (w : IWorkerNodeService) r =
        {
            updateProgress = w.updateProgress
            saveResult = w.saveResult
            saveCharts = w.saveCharts
            logCrit = saveSolverRunnerErrFs (getErrName r)
        }


    type WorkerNodeService () =
        inherit MarshalByRefObject()
        let logger = Logger.log4net
        let className = "WorkerNodeService"
        let toError e = e |> WorkerNodeServiceErr |> Error
        let addError f e = ((f |> WorkerNodeServiceErr) + e) |> Error

        let w =
            let messagingClientAccessInfo = serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo
            let h = MsgResponseHandler messagingClientAccessInfo
            logger.logInfoString (sprintf "%s: Created MsgResponseHandler: %A" className h)

            let messagingClientData =
                {
                    msgAccessInfo = messagingClientAccessInfo
                    messagingService = h
                    msgClientProxy = MessagingClientProxy.create { messagingClientName = workerNodeServiceName }
                }

            let messagingClient = MessagingClient messagingClientData

            match messagingClient.start() with
            | Ok() ->
                let n =
                    {
                        workerNodeAccessInfo = serviceAccessInfo
                        workerNodeProxy = WorkerNodeProxy.create WorkerNodeProxyData.defaultValue
                        messageProcessorProxy = messagingClient.messageProcessorProxy
                        minUsefulEe = MinUsefulEe.defaultValue
                    }
                    |> createServiceImpl logger

                match n with
                | Ok (Some v) ->
                    createMessagingClientEventHandlers logger messagingClient
                    Ok v
                | Ok None -> toError UnableToCreateWorkerNodeServiceError
                | Error e -> addError UnableToCreateWorkerNodeServiceError e
            | Error e -> addError UnableToStartMessagingClientError e

        let initService () = ()
        do initService ()


        let updateProgressImpl p =
            match w with
            | Ok r -> r.updateProgress p
            | Error e -> addError (UpdateLocalProgressError (sprintf "Failed to update progress: %A" p)) e


        //let saveResultImpl r =
        //    match w with
        //    | Ok r -> r.configure d
        //    | Error e -> addError (ConfigureServiceError (sprintf "Failed to configure service: %A" d)) e


        //let saveChartsImpl c =
        //    match w with
        //    | Ok r -> r.getState d
        //    | Error e -> addError (ConfigureServiceError (sprintf "Failed to configure service: %A" d)) e


        let configureImpl d =
            match w with
            | Ok r -> r.configure d
            | Error e -> addError (ConfigureServiceError (sprintf "Failed to configure service: %A" d)) e


        let monitorImpl _ =
            match w with
            | Ok (Some r) -> r.getState() |> WrkNodeState
            | Ok None -> CannotAccessWrkNode
            | Error e -> ErrorOccurred e


        interface IWorkerNodeService with
            member _.updateProgress p = updateProgressImpl p
            //member _.saveResult r = saveResultImpl r
            //member _.saveCharts c = saveChartsImpl c
            member _.ping() = Ok()
            member _.configure d = configureImpl d
            member _.monitor p = monitorImpl p

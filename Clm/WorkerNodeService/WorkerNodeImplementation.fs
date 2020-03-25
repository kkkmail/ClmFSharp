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


    type WorkerNodeMessage =
        | Start of AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | UpdateProgress of AsyncReplyChannel<UnitResult> * ProgressUpdateInfo
        | GetMessages of AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of AsyncReplyChannel<UnitResult> * WorkerNodeConfigParam


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnRegisterProxy =
        {
            partitionerId : PartitionerId
            workerNodeInfo : WorkerNodeInfo
            sendMessage : MessageInfo -> UnitResult
        }


    type OnUpdateProgressProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
            onSaveResult : ResultDataId -> UnitResult
            onSaveCharts : ResultDataId -> UnitResult
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult
        }


    let onSaveResult (proxy : SendMessageProxy) r =
        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> bindError (addError OnSaveResultErr (SendResultMessageError (proxy.partitionerId.messagingClientId, r.resultDataId)))


    let onSaveCharts (proxy : SendMessageProxy) c =
        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = c |> SaveChartsPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> bindError (addError OnSaveChartsErr (SendChartMessageError (proxy.partitionerId.messagingClientId, c.resultDataId)))


    let onRegister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage

        s, result


    let onUnregister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage

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
                            partitionerRecipient = proxy.partitionerId
                            deliveryType = t
                            messageData = UpdateProgressPrtMsg p
                        }.getMessageInfo()
                        |> proxy.sendMessage
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
            partitionerId : PartitionerId
            workerNodeId : WorkerNodeId
            runModel : SolverRunnerProxy -> WorkerNodeRunModelData -> unit
            sendMessage : MessageInfo -> UnitResult
        }


    let onRunModel (proxy : OnRunModelProxy) (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        let addError = addError OnRunModelErr

        let w, result =
            let a = proxy.getRunModelParam d

            match s.numberOfWorkerCores > s.runningWorkers.Count with
            | true ->
                match proxy.runModel a with
                | Ok lpsi ->
                    let res =
                        match proxy.saveWorkerNodeRunModelData { d with localProcessId = Some lpsi.localProcessId; commandLine = proxy.getCommandLine a } with
                        | Ok() -> Ok()
                        | Error e -> addError (proxy.getCommandLine a |> CannotSaveWorkerNodeRunModelData) e

                    let rs =
                        {
                            runnerRemoteProcessId = d.remoteProcessId
                            progress = TaskProgress.NotStarted
                            started = DateTime.Now
                            lastUpdated = DateTime.Now
                        }

                    { s with runningWorkers = s.runningWorkers.Add(lpsi.localProcessId, rs) }, res
                | Error e ->
                    let err = addError (proxy.getCommandLine a |> CannotRunModel) (ProcessStartedErr e)
                    s, err
            | false ->
                let r = a.callBackInfo.runQueueId.toRemoteProcessId()

                let q =
                    {
                        remoteProcessId = r
                        runningProcessData = a.callBackInfo
                        progress = Failed (proxy.workerNodeId, r)
                    }

                let res =
                    {
                        partitionerRecipient = proxy.partitionerId
                        deliveryType = GuaranteedDelivery
                        messageData = UpdateProgressPrtMsg q
                    }.getMessageInfo()
                    |> proxy.sendMessage

                s, res

        w, result


    type OnStartProxy =
        {
            partitionerId : PartitionerId
            noOfCores : int
            workerNodeId : WorkerNodeId
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            sendMessage : MessageInfo -> UnitResult
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }


    let runIfNoResult (proxy : OnStartProxy) (r : list<ResultDataWithId>) g w =
        let d = w.remoteProcessId.toResultDataId()

        match r |> List.tryFind (fun e -> e.resultDataId = d) with
        | Some _ ->
            let result =
                [
                    {
                        partitionerRecipient = proxy.partitionerId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            {
                                remoteProcessId = w.remoteProcessId
                                runningProcessData = w.runningProcessData
                                progress = Completed NotGeneratedCharts
                            }
                            |> UpdateProgressPrtMsg
                    }.getMessageInfo()
                    |> proxy.sendMessage

                    proxy.tryDeleteWorkerNodeRunModelData w.remoteProcessId
                    proxy.tryDeleteModelData w.runningProcessData.modelDataId
                    proxy.onSaveResult d
                    proxy.onSaveCharts d
                ]
                |> foldUnitResults
            g, result
        | None -> proxy.onRunModel g w


    let onStart (proxy : OnStartProxy) s =
        let doStart mi ri =
            let m, mf = mi |> Rop.unzip
            let (r : list<ResultDataWithId>), rf = ri |> Rop.unzip

            match (mf @ rf) |> foldErrors with
            | None ->
                let runIfNoResult = runIfNoResult proxy r

                let tryRunModel g w =
                    match w.localProcessId with
                    | Some v ->
                        match tryGetProcessById v |> Option.bind tryGetProcessName with
                        | Some n when n = SolverRunnerProcessName ->
                            let rs = RunnerState.defaultValue w.remoteProcessId
                            { g with runningWorkers = s.runningWorkers.Add(v, rs) }, Ok()
                        | _ -> runIfNoResult g w
                    | None -> proxy.onRunModel g w

                let run g e f =
                    let (x, z) = tryRunModel g e
                    x, combineUnitResults f z

                let retVal = m |> List.fold (fun (g, f) e -> run g e f) ({ s with numberOfWorkerCores = proxy.noOfCores }, Ok())
                retVal
            | Some e -> s, Error e

        let g, result =
            match proxy.loadAllWorkerNodeRunModelData(), proxy.loadAllResultData() with
            | Ok mi, Ok ri -> doStart mi ri
            | Ok _, Error e -> s, Error e
            | Error e, Ok _ -> s, Error e
            | Error e1, Error e2 -> s, Error (e1 + e2)

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
                    partitionerRecipient = proxy.partitionerId
                    deliveryType = GuaranteedDelivery
                    messageData = { proxy.workerNodeInfo with nodeInfo = { proxy.workerNodeInfo.nodeInfo with noOfCores = cores } } |> RegisterWorkerNodePrtMsg
                }.getMessageInfo()
                |> proxy.sendMessage

            let w, result =
                match send() with
                | Ok() -> { s with numberOfWorkerCores = cores }, Ok()
                | Error e -> s, Error e

            w, result


    let onSaveResultProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            //loadResultData = i.workerNodeProxy.loadResultData
            //tryDeleteResultData = i.workerNodeProxy.tryDeleteResultData
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onSaveChartsProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            loadChartInfo = i.workerNodeProxy.loadChartInfo
            tryDeleteChartInfo = i.workerNodeProxy.tryDeleteChartInfo
            sendMessage = i.messageProcessorProxy.sendMessage
        }

    let onRunModelProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            workerNodeId = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId
            getRunModelParam = getRunModelParam i
            runModel = i.workerNodeProxy.runModel
            getCommandLine = i.workerNodeProxy.getCommandLine
            saveWorkerNodeRunModelData = i.workerNodeProxy.saveWorkerNodeRunModelData
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onStartProxy i =
        let proxy = i.workerNodeProxy

        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            noOfCores = i.workerNodeAccessInfo.nodeInfo.noOfCores
            workerNodeId = i.workerNodeAccessInfo.workNodeMsgAccessInfo.workerNodeId
            loadAllWorkerNodeRunModelData = proxy.loadAllWorkerNodeRunModelData
            loadAllResultData = proxy.loadAllResultData
            sendMessage = i.messageProcessorProxy.sendMessage
            tryDeleteWorkerNodeRunModelData = proxy.tryDeleteWorkerNodeRunModelData
            tryDeleteModelData = proxy.tryDeleteModelData
            onSaveResult = onSaveResult (onSaveResultProxy i)
            onSaveCharts = onSaveCharts (onSaveChartsProxy i)
            onRunModel = onRunModel (onRunModelProxy i)
        }


    let onRegisterProxy i : OnRegisterProxy =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            workerNodeInfo = i.workerNodeAccessInfo.workerNodeInfo
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onUpdateProgressProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            sendMessage = i.messageProcessorProxy.sendMessage
            onSaveResult = onSaveResult (onSaveResultProxy i)
            onSaveCharts = onSaveCharts (onSaveChartsProxy i)
            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
            tryDeleteModelData = i.workerNodeProxy.tryDeleteModelData
        }


    let onProcessMessageProxy i =
        {
            saveModelData = i.workerNodeProxy.saveModelData
            onRunModel = onRunModel (onRunModelProxy i)
            tryFindRunningModel = tryFindRunningModel
        }


    let onGetMessagesProxy i =
        {
            tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
            onProcessMessage = onProcessMessage (onProcessMessageProxy i)
            maxMessages = WorkerNodeRunnerState.maxMessages
            onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
        }


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let onStartProxy = onStartProxy i
        let onRegisterProxy = onRegisterProxy i
        let onUpdateProgressProxy = onUpdateProgressProxy i
        let onGetMessagesProxy = onGetMessagesProxy i
        let onConfigureWorkerProxy = onRegisterProxy

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start r -> return! onStart onStartProxy s |> (withReply r) |> loop
                            | Register r -> return! onRegister onRegisterProxy s |> (withReply r) |> loop
                            | Unregister r -> return! onUnregister onRegisterProxy s |> (withReply r) |> loop
                            | UpdateProgress (r, p) -> return! onUpdateProgress onUpdateProgressProxy s p |> (withReply r) |> loop
                            | GetMessages r -> return! onGetMessages onGetMessagesProxy s |> (withReply r) |> loop
                            | GetState r -> return! onGetState s |> (withReply r) |> loop
                            | ConfigureWorker (r, d) -> return! onConfigureWorker onConfigureWorkerProxy s d |> (withReply r) |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member _.start() = messageLoop.PostAndReply Start
        member _.register() = messageLoop.PostAndReply Register
        member _.unregister() = messageLoop.PostAndReply Unregister
        member _.updateProgress p = messageLoop.PostAndReply (fun r -> UpdateProgress (r, p))
        member _.getMessages() = messageLoop.PostAndReply GetMessages
        member _.getState () = messageLoop.PostAndReply GetState
        member _.configure d = messageLoop.PostAndReply (fun r -> ConfigureWorker (r, d))


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
                    Ok (Some v)
                | Ok None -> toError UnableToCreateWorkerNodeServiceError
                | Error e -> addError UnableToCreateWorkerNodeServiceError e
            | Error e -> addError UnableToStartMessagingClientError e

        let initService () = ()
        do initService ()


        let updateProgressImpl p =
            match w with
            | Ok (Some r) -> r.updateProgress p
            | Ok None -> toError ServiceUnavailable
            | Error e -> addError (UpdateLocalProgressError (sprintf "Failed to update progress: %A" p)) e


        let configureImpl d =
            match w with
            | Ok (Some r) -> r.configure d
            | Ok None -> toError ServiceUnavailable
            | Error e -> addError (ConfigureServiceError (sprintf "Failed to configure service: %A" d)) e


        let monitorImpl _ =
            match w with
            | Ok (Some r) -> r.getState() |> WrkNodeState
            | Ok None -> CannotAccessWrkNode
            | Error e -> ErrorOccurred e


        interface IWorkerNodeService with
            member _.updateProgress p = updateProgressImpl p
            member _.saveResult r = 0
            member _.saveCharts c = 0
            member _.ping() = Ok()
            member _.configure d = configureImpl d
            member _.monitor p = monitorImpl p

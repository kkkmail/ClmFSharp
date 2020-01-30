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
open Clm.CalculationData
open ClmSys.ClmErrors
open ClmSys.ContGenData
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerData
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives

module ServiceImplementation =

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
        | Start of AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | UpdateProgress of AsyncReplyChannel<UnitResult> * LocalProgressUpdateInfo
        | GetMessages of AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of AsyncReplyChannel<UnitResult> * WorkerNodeConfigParam


    type OnSaveResultProxy =
        {
            partitionerId : PartitionerId
            loadResultData : ResultDataId -> ClmResult<ResultDataWithId>
            tryDeleteResultData : ResultDataId -> UnitResult
            sendMessage : MessageInfo -> UnitResult
        }


    type OnSaveChartsProxy =
        {
            partitionerId : PartitionerId
            loadChartInfo : ResultDataId -> ClmResult<ChartInfo>
            tryDeleteChartInfo : ResultDataId -> UnitResult
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
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult
        }


    let getSolverRunnerAccessInfo (i : WorkerNodeRunnerData) =
        {
            wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
            minUsefulEe = i.minUsefulEe
        }
        |> WorkerNodeSvcAccessInfo


    let onSaveResult (proxy : OnSaveResultProxy) (d : ResultDataId) =
        let addError f e = ((f |> OnSaveResultErr |> WorkerNodeErr) + e) |> Error

        match proxy.loadResultData d with
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
                | Error e -> addError (DeleteResultDataError d.value) e
            | Error e -> addError (SendResultMessageError (proxy.partitionerId.messagingClientId.value, d.value)) e
        | Error e -> addError (LoadResultDataErr d.value) e


    let onSaveCharts (proxy : OnSaveChartsProxy) (d : ResultDataId) =
        let addError f e = ((f |> OnSaveChartsErr |> WorkerNodeErr) + e) |> Error

        match proxy.loadChartInfo d with
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
                | Error e -> addError (DeleteChartInfoError d.value) e
            | Error e -> addError (SendChartMessageError (proxy.partitionerId.messagingClientId.value, d.value)) e
        | Error e -> addError (LoadChartInfoError d.value) e


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
        | Completed -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)


    let getUpdateProgressResult (send : unit -> UnitResult) (proxy : OnUpdateProgressProxy) (p : LocalProgressUpdateInfo) rid c =
        [
            send()

            if c
            then
                proxy.tryDeleteWorkerNodeRunModelData rid
                proxy.tryDeleteModelData p.runningProcessData.modelDataId
                p.runningProcessData.toResultDataId() |> proxy.onSaveResult
                p.runningProcessData.toResultDataId() |> proxy.onSaveCharts
        ]
        |> foldUnitResults


    let onUpdateProgress (proxy : OnUpdateProgressProxy) s (p : LocalProgressUpdateInfo) =
        let updateProgress t c =
            let rso, result =
                match s.runningWorkers |> Map.tryFind p.localProcessId with
                | Some rs ->
                    let send() =
                        {
                            partitionerRecipient = proxy.partitionerId
                            deliveryType = t
                            messageData = UpdateProgressPrtMsg { remoteProcessId = rs.runnerRemoteProcessId; runningProcessData = p.runningProcessData; progress = p.progress }
                        }.getMessageInfo()
                        |> proxy.sendMessage

                    let result = getUpdateProgressResult send proxy p rs.runnerRemoteProcessId c
                    Some { rs with progress = p.progress; lastUpdated = DateTime.Now }, result
                | None -> None, p.localProcessId.value |> UnableToFindMappingError |> OnUpdateProgressErr |> WorkerNodeErr |> Error

            if c
            then { s with runningWorkers = s.runningWorkers.tryRemove p.localProcessId }, result
            else
                match rso with
                | Some rs -> { s with runningWorkers = s.runningWorkers.Add(p.localProcessId, rs) }, result
                | None -> s, result

        let (t, c) = toDeliveryType p.progress
        let w, result = updateProgress t c
        w, result


    let getRunModelParam (i : WorkerNodeRunnerData) (d : WorkerNodeRunModelData) =
        {
            exeName = i.exeName
            commandLineParam =
                {
                    taskParam = d.taskParam
                    serviceAccessInfo = getSolverRunnerAccessInfo i
                }

            callBackInfo = { d.runningProcessData with workerNodeId = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId }
        }


    type OnRunModelProxy =
        {
            partitionerId : PartitionerId
            workerNodeId : WorkerNodeId
            getRunModelParam : WorkerNodeRunModelData -> RunModelParam
            runModel : RunModelParam ->  Result<LocalProcessStartedInfo, ProcessStartedError>
            getCommandLine : RunModelParam -> string
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            sendMessage : MessageInfo -> UnitResult
        }


    let onRunModel (proxy : OnRunModelProxy) (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        let addError f e = ((f |> OnRunModelErr |> WorkerNodeErr) + e) |> Error

        let w, result =
            let a = proxy.getRunModelParam d

            match s.numberOfCores > s.runningWorkers.Count with
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
                    //proxy.saveWorkerNodeRunModelData { d with localProcessId = None; commandLine = proxy.getCommandLine a }
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
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            loadAllResultData : unit -> ListResult<ResultDataWithId>
            sendMessage : MessageInfo -> UnitResult
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult
            onSaveResult : ResultDataId -> UnitResult
            onSaveCharts : ResultDataId -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }


    let onStart (proxy : OnStartProxy) s =
        let doStart mi ri =
            let m, mf = mi |> Rop.unzip
            let (r : list<ResultDataWithId>), rf = ri |> Rop.unzip

            match (mf @ rf) |> foldErrors with
            | None ->
                let runIfNoResult g w =
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
                                            progress = Completed
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

                let tryRunModel g w =
                    match w.localProcessId with
                    | Some v ->
                        match tryGetProcessById v with
                        | Some p ->
                            match tryGetProcessName p with
                            | Some n when n = SolverRunnerProcessName ->
                                let rs =
                                    {
                                        runnerRemoteProcessId = w.remoteProcessId
                                        progress = TaskProgress.NotStarted
                                        started = DateTime.Now
                                        lastUpdated = DateTime.Now
                                    }

                                { g with runningWorkers = s.runningWorkers.Add(v, rs) }, Ok()
                            | _ -> runIfNoResult g w
                        | None -> runIfNoResult g w
                    | None -> proxy.onRunModel g w

                let run g e f =
                    let (x, z) = tryRunModel g e
                    x, combineUnitResults f z

                let retVal = m |> List.fold (fun (g, f) e -> run g e f) ({ s with numberOfCores = proxy.noOfCores }, Ok())
                retVal
            | Some e -> s, Error e

        let g, result =
            match proxy.loadAllWorkerNodeRunModelData(), proxy.loadAllResultData() with
            | Ok mi, Ok ri -> doStart mi ri
            | Ok _, Error e -> s, Error e
            | Error e, Ok _ -> s, Error e
            | Error e1, Error e2 -> s, Error (e1 + e2)

        g, result


    let tryFindRunningModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        s.runningWorkers
        |> Map.toList
        |> List.map (fun (_, v) -> v)
        |> List.tryFind (fun e -> e.runnerRemoteProcessId = d.remoteProcessId)


    type OnProcessMessageProxy =
        {
            saveModelData : ModelData -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
            tryFindRunningModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> RunnerState option
        }


    let onProcessMessage (proxy : OnProcessMessageProxy) s (m : Message) =
        let addError f e = ((f |> OnProcessMessageErr |> WorkerNodeErr) + e) |> Error
        let toError e = e |> OnProcessMessageErr |> WorkerNodeErr |> Error

        let w, result =
            match m.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg (d, m) ->
                    match proxy.tryFindRunningModel s d with
                    | None ->
                        match proxy.saveModelData m with
                        | Ok() -> proxy.onRunModel s d
                        | Error e -> s, addError CannotSaveModelData e
                    | Some r -> s, r.runnerRemoteProcessId.value |> ModelAlreadyRunning |> toError
            | _ -> s, m.messageData.getInfo() |> InvalidMessage |> toError

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
                    messageData = { proxy.workerNodeInfo with noOfCores = cores} |> RegisterWorkerNodePrtMsg
                }.getMessageInfo()
                |> proxy.sendMessage

            let w, result =
                match send() with
                | Ok() -> { s with numberOfCores = cores }, Ok()
                | Error e -> s, Error e

            w, result


    let onSaveResultProxy i =
        {
            partitionerId = i.workerNodeAccessInfo.partitionerId
            loadResultData = i.workerNodeProxy.loadResultData
            tryDeleteResultData = i.workerNodeProxy.tryDeleteResultData
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
            noOfCores = i.workerNodeAccessInfo.noOfCores
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
                let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger.logError w.getMessages)
                do h.start()
                Ok (Some w)
            | Error e -> Error e
        | true ->
            logger.logInfoString "createServiceImpl: Unregistering..."
            match w.unregister() with
            | Ok() -> Ok None
            | Error e -> Error e


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
                        workerNodeProxy = WorkerNodeProxy.create WorkerNodeProxyInfo.defaultValue
                        messageProcessorProxy = messagingClient.messageProcessorProxy
                        exeName = SolverRunnerName
                        minUsefulEe = MinUsefulEe.defaultValue
                    }
                    |> createServiceImpl logger

                match n with
                | Ok (Some v) ->
                    createMessagingClientEventHandlers logger.logError messagingClient
                    Ok (Some v)
                | Ok None -> toError UnableToCreateWorkerNodeServiceError
                | Error e -> addError UnableToCreateWorkerNodeServiceError e
            | Error e -> addError UnableToStartMessagingClientError e

        let initService () = ()
        do initService ()


        let updateLocalProgressImpl p =
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
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = Ok()
            member __.configure d = configureImpl d
            member __.monitor p = monitorImpl p

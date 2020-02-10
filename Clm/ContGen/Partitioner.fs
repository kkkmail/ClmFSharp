namespace ContGen

open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.Logging
open ContGenServiceInfo.ServiceInfo
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open ClmSys.WorkerNodeData
open ServiceProxy.PartitionerProxy
open PartitionerServiceInfo.ServiceInfo
open ClmSys.TimerEvents
open Clm.CalculationData
open ServiceProxy.MsgProcessorProxy
open ClmSys
open ClmSys.ClmErrors
open ClmSys.PartitionerData
open ClmSys.WorkerNodePrimitives
open ClmSys.SolverRunnerData
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerErrors
open ClmSys.ContGenData

module Partitioner =

    type PartitionerCallBackInfo =
        {
            onUpdateProgress : ProgressUpdateInfo -> UnitResult
            setRunLimit : int -> UnitResult
        }

        static member defaultValue =
            {
                onUpdateProgress = fun _ -> Ok()
                setRunLimit = fun _ -> Ok()
            }


    type PartitionerRunnerData =
        {
            partitionerMsgAccessInfo : PartitionerMsgAccessInfo
            partitionerProxy : PartitionerProxy
            messageProcessorProxy : MessageProcessorProxy
            //logger : Logger
        }

    //type PartitionerRunnerState =
    //    {
    //        //workerNodes : Map<WorkerNodeId, WorkerNodeState>
    //        //partitionerCallBackInfo : PartitionerCallBackInfo
    //        dummy : int
    //    }
    //
    //    static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]
    //
    //    static member defaultValue =
    //        {
    //            //workerNodes = Map.empty
    //            //partitionerCallBackInfo = PartitionerCallBackInfo.defaultValue
    //            dummy = 0
    //        }



    //type PartitionerRunnerResult = StateWithResult<PartitionerRunnerState>
    //type PartitionerRunnerProcessStartedResult = PartitionerRunnerState * ProcessStartedResult


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo * AsyncReplyChannel<UnitResult>
        | RunModel of RunModelParam * AsyncReplyChannel<ProcessStartedResult>
        | GetMessages of AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    //type OnRegisterProxy =
    //    {
    //        upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
    //    }
    //
    //
    //let onRegister (proxy : OnRegisterProxy) (r : WorkerNodeInfo) = proxy.upsertWorkerNodeInfo r


    //type OnCompletedProxy =
    //    {
    //        tryDeleteRunModelParamWithRemoteId : RemoteProcessId -> UnitResult
    //    }
    //
    //
    //let onCompleted (proxy : OnCompletedProxy) r =
    //    let result = proxy.tryDeleteRunModelParamWithRemoteId r
    //    result


    //type SendRunModelMessageProxy =
    //    {
    //        sendMessage : MessageInfo -> UnitResult
    //    }
    //
    //
    //let sendRunModelMessage (proxy : SendRunModelMessageProxy) (e : RunModelParamWithRemoteId) workerNodeId m =
    //    {
    //        workerNodeRecipient = workerNodeId
    //        deliveryType = GuaranteedDelivery
    //        messageData =
    //            (
    //                {
    //                    remoteProcessId = e.remoteProcessId
    //                    localProcessId = None
    //                    runningProcessData = { e.runModelParam.callBackInfo with workerNodeId = workerNodeId }
    //                    taskParam = e.runModelParam.commandLineParam.taskParam
    //                    minUsefulEe = e.runModelParam.commandLineParam.serviceAccessInfo.minUsefulEe
    //                    commandLine = EmptyString
    //                },
    //                m
    //            )
    //            |> RunModelWrkMsg
    //    }.getMessageInfo()
    //    |> proxy.sendMessage


    type OnTryRunModelWithRemoteIdProxy =
        {
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId option>
            tryGetNode : unit -> ClmResult<WorkerNodeId option>
            loadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            sendRunModelMessage : RunModelParamWithRemoteId -> WorkerNodeId -> ModelData -> UnitResult
            onCompleted : RemoteProcessId -> UnitResult
        }


    let onTryRunModelWithRemoteId (proxy : OnTryRunModelWithRemoteIdProxy) (i : RunModelParamWithRemoteId) =
        let addError f e = ((f |> OnTryRunModelWithRemoteIdErr |> PartitionerErr) + e) |> Error
        let toClmError e = e |> OnTryRunModelWithRemoteIdErr |> PartitionerErr
        let toError e = e |> toClmError |> Error
        let modelDataId = i.runModelParam.callBackInfo.modelDataId

        let result =
            match proxy.tryLoadResultData (i.runModelParam.callBackInfo.runQueueId.toResultDataId()) with
            | Ok None ->
                match proxy.tryGetNode() with
                | Ok (Some workerNodeId) ->
                    match proxy.loadModelData i.runModelParam.commandLineParam.serviceAccessInfo modelDataId with
                    | Ok m ->
                        match proxy.sendRunModelMessage i workerNodeId m with
                        | Ok() ->
                            let x =
                                {
                                    processId = i.remoteProcessId |> RemoteProcess
                                    runningProcessData = i.runModelParam.callBackInfo
                                }

                            let r = (x, None) |> StartedSuccessfully |> Ok
                            r
                        | Error e -> addError (UnableToSendRunModelMessage modelDataId.value) e
                    | Error e -> addError (UnableToLoadModelData modelDataId.value) e
                | Ok None -> Ok()
                | Error e -> addError (UnableToGetWorkerNode modelDataId.value) e
            | Ok (Some _) ->
                let r = proxy.onCompleted i.remoteProcessId
                let result = toErrorOption toClmError (OnCompletedErr modelDataId.value) r
                result |> AlreadyCompleted |> Ok
            | Error e -> addError (UnableToLoadModelData i.runModelParam.callBackInfo.modelDataId.value) e

        result


    //type OnStartProxy =
    //    {
    //        loadAllWorkerNodeState : unit -> ListResult<WorkerNodeState>
    //        setRunLimit : Map<WorkerNodeId, WorkerNodeState> -> UnitResult
    //    }
    //
    //
    //let onStart (proxy : OnStartProxy) s =
    //    let onStartRun g r = { g with workerNodes = g.workerNodes.Add (r.workerNodeInfo.workerNodeId, r) }
    //
    //    match proxy.loadAllWorkerNodeState() with
    //    | Ok w ->
    //        let workers, e = w |> Rop.unzip
    //        match e |> foldToUnitResult with
    //        | Ok() ->
    //            let w = workers |> List.fold (fun acc r -> onStartRun acc r) s
    //            let result = proxy.setRunLimit w.workerNodes
    //            w, result
    //        | Error e -> s, Error e
    //    | Error e -> s, Error e


    //type OnUpdateProgressProxy =
    //    {
    //        onUpdateProgress : ProgressUpdateInfo -> UnitResult
    //        onCompleted : RemoteProcessId -> UnitResult
    //    }
    //
    //
    //let onUpdateProgress (proxy : OnUpdateProgressProxy) (i : RemoteProgressUpdateInfo) =
    //    let u = i.toProgressUpdateInfo() |> proxy.onUpdateProgress
    //
    //    let r =
    //        match i.progress with
    //        | NotStarted | InProgress _ -> Ok()
    //        | Completed -> proxy.onCompleted i.remoteProcessId
    //        | Failed _ -> proxy.onCompleted i.remoteProcessId
    //
    //    combineUnitResults r u


    //type OnFailedProxy =
    //    {
    //        loadRunModelParamWithRemoteId : RemoteProcessId -> ClmResult<RunModelParamWithRemoteId>
    //        onUpdateProgress : RemoteProgressUpdateInfo -> UnitResult
    //    }
    //
    //
    //let onFailed (proxy : OnFailedProxy) r (i : RemoteProcessId) =
    //    let result =
    //        match proxy.loadRunModelParamWithRemoteId i with
    //        | Ok m ->
    //            let i = Failed (r, i) |> m.toRemoteProgressUpdateInfo
    //            let r = proxy.onUpdateProgress i
    //            r
    //        | Error e -> Error e
    //
    //    result


    //type OnUnregisterProxy =
    //    {
    //        loadWorkerNodeInfo : WorkerNodeId -> ClmResult<WorkerNodeInfo>
    //        upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
    //    }
    //
    //
    //let onUnregister (proxy : OnUnregisterProxy) (r : WorkerNodeId) =
    //    let addError f e = ((f |> OnUnregisterErr |> PartitionerErr) + e) |> Error
    //    let toError e = e |> OnUnregisterErr |> PartitionerErr |> Error
    //
    //    match proxy.loadWorkerNodeInfo r with
    //    | Ok w ->
    //        match proxy.upsertWorkerNodeInfo w with
    //        | Ok() -> Ok()
    //        | Error e -> addError CannotUpsertWorkerNodeInfo e
    //    | Error e -> addError CannotLoadWorkerNodeInfo e


    type OnSaveResultProxy =
        {
            saveResultData : ResultDataWithId -> UnitResult
        }


    let onSaveResult (proxy : OnSaveResultProxy) r =
        let result = proxy.saveResultData r
        result


    type OnSaveChartsProxy =
        {
            saveCharts : ChartInfo -> UnitResult
        }


    let onSaveCharts (proxy : OnSaveChartsProxy) (c : ChartInfo) =
        let result = proxy.saveCharts c
        result


    type OnProcessMessageProxy =
        {
            onUpdateProgress : RemoteProgressUpdateInfo -> UnitResult
            onSaveResult : ResultDataWithId -> UnitResult
            onSaveCharts : ChartInfo -> UnitResult
            onRegister : WorkerNodeInfo -> UnitResult
            onUnregister : WorkerNodeId -> UnitResult
        }


    let onProcessMessage (proxy : OnProcessMessageProxy) (m : Message) =
        let addError f e = ((f |> OnProcessPartitionerMessageErr |> PartitionerErr) + e) |> Error
        let toClmError e = e |> OnProcessPartitionerMessageErr |> PartitionerErr
        let toError e = e |> toClmError |> Error

        let result =
            match m.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i -> proxy.onUpdateProgress i
                | SaveResultPrtMsg r -> proxy.onSaveResult r
                | SaveChartsPrtMsg c -> proxy.onSaveCharts c
                | RegisterWorkerNodePrtMsg r -> proxy.onRegister r
                | UnregisterWorkerNodePrtMsg r -> proxy.onUnregister r
            | _ -> InvalidMessageTypeErr m.messageDataInfo.messageId.value |> toError

        result


    type OnProcessMessageType = OnProcessMessageType<unit>
    type OnGetMessagesProxy = OnGetMessagesProxy<unit>
    let onGetMessages = onGetMessages<unit>


    type OnRunModelProxy =
        {
            tryGetRunner : RunQueueId -> RemoteProcessId option
            saveRunModelParamWithRemoteId : RunModelParamWithRemoteId -> UnitResult
            onTryRunModelWithRemoteId : RunModelParamWithRemoteId -> ProcessStartedResult
        }


    let onRunModel (proxy : OnRunModelProxy) (a: RunModelParam) : ProcessStartedResult =
        let w, result =
            match tryGetRunner s a.callBackInfo.runQueueId with
            | Some q -> s, ({ processId = RemoteProcess q; runningProcessData = a.callBackInfo }, None) |> StartedSuccessfully |> Ok
            | None ->
                let i = { remoteProcessId = a.callBackInfo.runQueueId.toRemoteProcessId(); runModelParam = a }

                match proxy.saveRunModelParamWithRemoteId i with
                | Ok() -> proxy.onTryRunModelWithRemoteId s i
                | Error e -> s, Error e

        w, result


    let onGetState s = s, s


    let onRegisterProxy i c =
        {
            setRunLimit = setRunLimit (setRunLimitProxy c)
            saveWorkerNodeState = i.partitionerProxy.saveWorkerNodeState
        }


    let onCompletedProxy i =
        {
            tryDeleteRunModelParamWithRemoteId = i.partitionerProxy.tryDeleteRunModelParamWithRemoteId
            saveWorkerNodeState = i.partitionerProxy.saveWorkerNodeState
        }


    let sendRunModelMessageProxy i =
        {
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onTryRunModelWithRemoteIdProxy i =
        {
            tryLoadResultData = i.partitionerProxy.tryLoadResultData
            tryGetNode = tryGetNode
            loadModelData = i.partitionerProxy.loadModelData
            saveWorkerNodeState = i.partitionerProxy.saveWorkerNodeState
            sendRunModelMessage = sendRunModelMessage (sendRunModelMessageProxy i)
            onCompleted = onCompleted (onCompletedProxy i)
        }


    let onStartProxy i c =
        {
            loadAllWorkerNodeState = i.partitionerProxy.loadAllWorkerNodeState
            setRunLimit = setRunLimit (setRunLimitProxy c)
        }


    let onUpdateProgressProxy i (c : PartitionerCallBackInfo) =
        {
            onUpdateProgress = c.onUpdateProgress
            onCompleted = onCompleted (onCompletedProxy i)
        }


    let onFailedProxy i c =
        {
            loadRunModelParamWithRemoteId = i.partitionerProxy.loadRunModelParamWithRemoteId
            onUpdateProgress = onUpdateProgress (onUpdateProgressProxy i c)
        }


    let onUnregisterProxy i c =
        {
            tryDeleteWorkerNodeState = i.partitionerProxy.tryDeleteWorkerNodeState
            onFailed = onFailed (onFailedProxy i c)
            setRunLimit = setRunLimit (setRunLimitProxy c)
        }


    let onSaveResultProxy i =
        {
            saveResultData = i.partitionerProxy.saveResultData
        }


    let onSaveChartsProxy i =
        {
            saveCharts = i.partitionerProxy.saveCharts
        }


    let onRequestWorkProxy i =
        {
            x = 0
        }


    let onProcessMessageProxy i c =
        {
            onUpdateProgress = onUpdateProgress (onUpdateProgressProxy i c)
            onSaveResult = onSaveResult (onSaveResultProxy i)
            onSaveCharts = onSaveCharts (onSaveChartsProxy i)
            onRegister = onRegister (onRegisterProxy i c)
            onUnregister = onUnregister (onUnregisterProxy i c)
        }


    let onGetMessagesProxy i c =
        {
            tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
            onProcessMessage = onProcessMessage (onProcessMessageProxy i c)
            maxMessages = PartitionerRunnerState.maxMessages
            onError = fun f -> f |> OnGetMessagesPartitionerErr |> PartitionerErr
        }


    let onRunModelProxy i =
        {
            tryGetRunner = tryGetRunner
            saveRunModelParamWithRemoteId = i.partitionerProxy.saveRunModelParamWithRemoteId
            onTryRunModelWithRemoteId = onTryRunModelWithRemoteId (onTryRunModelWithRemoteIdProxy i)
        }


    type PartitionerRunner(i : PartitionerRunnerData) =
        let onStartProxy = onStartProxy i
        let onRunModelProxy = onRunModelProxy i
        let onGetMessagesProxy = onGetMessagesProxy i

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : PartitionerRunnerState, c : PartitionerCallBackInfo) =
                    async
                        {
                            match! u.Receive() with
                            | Start (q, r) -> return! (onStart (onStartProxy c) s |> (withReply r), q) |> loop
                            | RunModel (p, r) -> return! (onRunModel onRunModelProxy s p |> (withReply r), c) |> loop
                            | GetMessages r ->return! (onGetMessages (onGetMessagesProxy c) s |> (withReply r), c) |> loop
                            | GetState r -> return! (onGetState s|> (withReply r), c) |> loop
                        }

                (PartitionerRunnerState.defaultValue, PartitionerCallBackInfo.defaultValue) |> loop
                )


        member __.start q = messageLoop.PostAndReply (fun reply -> Start (q, reply))
        member __.runModel p = messageLoop.PostAndReply (fun reply -> RunModel (p, reply))
        member __.getMessages () = messageLoop.PostAndReply GetMessages
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl (logger : Logger) (i : PartitionerRunnerData) =
        logger.logInfoString "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger.logError w.getMessages)
        //do h.start()
        w, h

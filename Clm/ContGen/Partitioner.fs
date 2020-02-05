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


    type PartitionerRunnerState =
        {
            //workerNodes : Map<WorkerNodeId, WorkerNodeState>
            //partitionerCallBackInfo : PartitionerCallBackInfo
            dummy : int
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                //workerNodes = Map.empty
                //partitionerCallBackInfo = PartitionerCallBackInfo.defaultValue
                dummy = 0
            }


    type PartitionerRunnerResult = StateWithResult<PartitionerRunnerState>
    type PartitionerRunnerProcessStartedResult = PartitionerRunnerState * ProcessStartedResult


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo * AsyncReplyChannel<UnitResult>
        | RunModel of RunModelParam * AsyncReplyChannel<ProcessStartedResult>
        | GetMessages of AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    type SetRunLimitProxy =
        {
            setRunLimit : int -> UnitResult
        }


    let setRunLimit (proxy : SetRunLimitProxy) workerNodes =
        let c = workerNodes |> Map.fold (fun acc _ r -> r.workerNodeInfo.noOfCores + acc) 0
        let result = proxy.setRunLimit c
        result


    type OnRegisterProxy =
        {
            setRunLimit : Map<WorkerNodeId, WorkerNodeState> -> UnitResult
            saveWorkerNodeState : WorkerNodeState -> UnitResult
        }


    let onRegister (proxy : OnRegisterProxy) s (r : WorkerNodeInfo) =
        let updated q =
            let newState = { workerNodeInfo = r; runningProcesses = q }
            let result = proxy.saveWorkerNodeState newState
            { s with workerNodes = s.workerNodes.Add (r.workerNodeId, newState) }, result

        let w, result =
            match s.workerNodes.TryFind r.workerNodeId with
            | Some n -> n.runningProcesses
            | None -> Map.empty
            |> updated

        w, proxy.setRunLimit w.workerNodes |> combineUnitResults result


    let tryGetNode (workerNodes : Map<WorkerNodeId, WorkerNodeState>) =
        workerNodes
            |> Map.toList
            |> List.map (fun (_, v) -> v)
            |> List.sortBy (fun e -> e.priority)
            |> List.tryFind (fun e -> e.runningProcesses.Count < e.workerNodeInfo.noOfCores)


    let tryFindRunningNode (workerNodes : Map<WorkerNodeId, WorkerNodeState>) r =
        workerNodes |> Map.tryPick (fun a b -> b.runningProcesses |> Map.tryFind r |> Option.bind (fun _ -> Some (a, b)))


    type OnCompletedProxy =
        {
            tryDeleteRunModelParamWithRemoteId : RemoteProcessId -> UnitResult
            saveWorkerNodeState : WorkerNodeState -> UnitResult
        }


    let onCompleted (proxy : OnCompletedProxy) (s : PartitionerRunnerState) r =
        proxy.tryDeleteRunModelParamWithRemoteId r |> ignore

        match tryFindRunningNode s.workerNodes r with
        | Some (w, n) ->
            let newNodeState = { n with runningProcesses = n.runningProcesses.tryRemove r }
            let result = proxy.saveWorkerNodeState newNodeState
            { s with workerNodes = s.workerNodes.Add (w, newNodeState) }, result
        | None -> s, Ok()


    type SendRunModelMessageProxy =
        {
            sendMessage : MessageInfo -> UnitResult
        }


    let sendRunModelMessage (proxy : SendRunModelMessageProxy) (e : RunModelParamWithRemoteId) workerNodeId m =
        {
            workerNodeRecipient = workerNodeId
            deliveryType = GuaranteedDelivery
            messageData =
                (
                    {
                        remoteProcessId = e.remoteProcessId
                        localProcessId = None
                        runningProcessData = { e.runModelParam.callBackInfo with workerNodeId = workerNodeId }
                        taskParam = e.runModelParam.commandLineParam.taskParam
                        minUsefulEe = e.runModelParam.commandLineParam.serviceAccessInfo.minUsefulEe
                        commandLine = EmptyString
                    },
                    m
                )
                |> RunModelWrkMsg
        }.getMessageInfo()
        |> proxy.sendMessage


    type OnTryRunModelWithRemoteIdProxy =
        {
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId option>
            tryGetNode : Map<WorkerNodeId, WorkerNodeState> -> WorkerNodeState option
            loadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            saveWorkerNodeState : WorkerNodeState -> UnitResult
            sendRunModelMessage : RunModelParamWithRemoteId -> WorkerNodeId -> ModelData -> UnitResult
            onCompleted : PartitionerRunnerState -> RemoteProcessId -> PartitionerRunnerResult
        }


    let onTryRunModelWithRemoteId (proxy : OnTryRunModelWithRemoteIdProxy) s (i : RunModelParamWithRemoteId) : PartitionerRunnerProcessStartedResult =
        let addError f e = ((f |> OnTryRunModelWithRemoteIdErr |> PartitionerErr) + e) |> Error
        let toClmError e = e |> OnTryRunModelWithRemoteIdErr |> PartitionerErr
        let toError e = e |> toClmError |> Error
        let modelDataId = i.runModelParam.callBackInfo.modelDataId

        let w, result =
            match proxy.tryLoadResultData (i.runModelParam.callBackInfo.runQueueId.toResultDataId()) with
            | Ok None ->
                match proxy.tryGetNode s.workerNodes with
                | Some n ->
                    match proxy.loadModelData i.runModelParam.commandLineParam.serviceAccessInfo modelDataId with
                    | Ok m ->
                        match proxy.sendRunModelMessage i n.workerNodeInfo.workerNodeId m with
                        | Ok() ->
                            let newNodeState = { n with runningProcesses = n.runningProcesses.Add(i.remoteProcessId, i.runModelParam.callBackInfo.runQueueId) }
                            let b = proxy.saveWorkerNodeState newNodeState

                            let x =
                                {
                                    processId = i.remoteProcessId |> RemoteProcess
                                    runningProcessData = i.runModelParam.callBackInfo
                                }

                            let r = (x, b |> toErrorOption toClmError (TryRunModelWithRemoteIdErr modelDataId.value)) |> StartedSuccessfully |> Ok
                            { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }, r
                        | Error e -> s, addError (UnableToSendRunModelMessage modelDataId.value) e
                    | Error e -> s, addError (UnableToLoadModelData modelDataId.value) e
                | None -> s, UnableToGetWorkerNode modelDataId.value |> toError
            | Ok (Some _) ->
                let w, r = proxy.onCompleted s i.remoteProcessId
                let result = toErrorOption toClmError (OnCompletedErr modelDataId.value) r
                w, result |> AlreadyCompleted |> Ok
            | Error e -> s,addError (UnableToLoadModelData i.runModelParam.callBackInfo.modelDataId.value) e

        w, result


    type OnStartProxy =
        {
            loadAllWorkerNodeState : unit -> ListResult<WorkerNodeState>
            setRunLimit : Map<WorkerNodeId, WorkerNodeState> -> UnitResult
        }


    let onStart (proxy : OnStartProxy) s =
        let onStartRun g r = { g with workerNodes = g.workerNodes.Add (r.workerNodeInfo.workerNodeId, r) }

        match proxy.loadAllWorkerNodeState() with
        | Ok w ->
            let workers, e = w |> Rop.unzip
            match e |> foldToUnitResult with
            | Ok() ->
                let w = workers |> List.fold (fun acc r -> onStartRun acc r) s
                let result = proxy.setRunLimit w.workerNodes
                w, result
            | Error e -> s, Error e
        | Error e -> s, Error e


    type OnUpdateProgressProxy =
        {
            onUpdateProgress : ProgressUpdateInfo -> UnitResult
            onCompleted : PartitionerRunnerState -> RemoteProcessId -> PartitionerRunnerResult
        }


    let onUpdateProgress (proxy : OnUpdateProgressProxy) s (i : RemoteProgressUpdateInfo) =
        let u = i.toProgressUpdateInfo() |> proxy.onUpdateProgress

        let w, r =
            match i.progress with
            | NotStarted | InProgress _ -> s, Ok()
            | Completed -> proxy.onCompleted s i.remoteProcessId
            | Failed _ -> proxy.onCompleted s i.remoteProcessId

        w, combineUnitResults r u


    type OnFailedProxy =
        {
            loadRunModelParamWithRemoteId : RemoteProcessId -> ClmResult<RunModelParamWithRemoteId>
            onUpdateProgress : PartitionerRunnerState -> RemoteProgressUpdateInfo -> PartitionerRunnerResult
        }


    let onFailed (proxy : OnFailedProxy) s r (i : RemoteProcessId) =
        let w, result =
            match proxy.loadRunModelParamWithRemoteId i with
            | Ok m ->
                let i = Failed (r, i) |> m.toRemoteProgressUpdateInfo
                let w, r = proxy.onUpdateProgress s i
                w, r
            | Error e -> s, Error e

        w, result


    type OnUnregisterProxy =
        {
            tryDeleteWorkerNodeState : WorkerNodeId -> UnitResult
            onFailed : PartitionerRunnerState -> WorkerNodeId -> RemoteProcessId -> PartitionerRunnerResult
            setRunLimit : Map<WorkerNodeId, WorkerNodeState> -> UnitResult
        }


    let onUnregister (proxy : OnUnregisterProxy) s (r : WorkerNodeId) =
        let removeNode() =
            let wn = s.workerNodes.tryRemove r

            let result =
                [
                    proxy.tryDeleteWorkerNodeState r
                    proxy.setRunLimit wn
                ]
                |> foldUnitResults

            { s with workerNodes = wn }, [result]

        let failed w r e u =
            let v, q = proxy.onFailed w r e
            v, q :: u

        let w, results =
            match s.workerNodes.TryFind r with
            | Some n ->
                n.runningProcesses
                |> Map.toList
                |> List.map (fun (e, _) -> e)
                |> List.fold (fun (w, u) e -> failed w r e u) (removeNode())
            | None -> removeNode()

        let result = results |> foldUnitResults
        w, result


    type OnRequestWorkProxy =
        {
            x : int
        }


    let onRequestWork (proxy : OnRequestWorkProxy) s (w : WorkerNodeRequestInfo) =
        s, Ok()


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
            onUpdateProgress : PartitionerRunnerState -> RemoteProgressUpdateInfo -> PartitionerRunnerResult
            onSaveResult : ResultDataWithId -> UnitResult
            onSaveCharts : ChartInfo -> UnitResult
            onRegister : PartitionerRunnerState -> WorkerNodeInfo -> PartitionerRunnerResult
            onUnregister : PartitionerRunnerState -> WorkerNodeId -> PartitionerRunnerResult
            onRequestWork : PartitionerRunnerState -> WorkerNodeRequestInfo -> PartitionerRunnerResult
        }


    let onProcessMessage (proxy : OnProcessMessageProxy) (s : PartitionerRunnerState) (m : Message) =
        let addError f e = ((f |> OnProcessPartitionerMessageErr |> PartitionerErr) + e) |> Error
        let toClmError e = e |> OnProcessPartitionerMessageErr |> PartitionerErr
        let toError e = e |> toClmError |> Error

        let w, result =
            match m.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i -> proxy.onUpdateProgress s i
                | SaveResultPrtMsg r -> s, proxy.onSaveResult r
                | SaveChartsPrtMsg c -> s, proxy.onSaveCharts c
                | RegisterWorkerNodePrtMsg r -> proxy.onRegister s r
                | UnregisterWorkerNodePrtMsg r -> proxy.onUnregister s r
                | RequestWork w -> proxy.onRequestWork s w
            | _ -> s, InvalidMessageTypeErr m.messageDataInfo.messageId.value |> toError

        w, result


    type OnProcessMessageType = OnProcessMessageType<PartitionerRunnerState>
    type OnGetMessagesProxy = OnGetMessagesProxy<PartitionerRunnerState>
    let onGetMessages = onGetMessages<PartitionerRunnerState>


    let tryGetRunner s q =
        s.workerNodes
        |> Map.tryPick (fun _ b -> b.runningProcesses |> Map.tryPick (fun a b -> if b = q then Some a else None))


    type OnRunModelProxy =
        {
            tryGetRunner : PartitionerRunnerState -> RunQueueId -> RemoteProcessId option
            saveRunModelParamWithRemoteId : RunModelParamWithRemoteId -> UnitResult
            onTryRunModelWithRemoteId : PartitionerRunnerState -> RunModelParamWithRemoteId -> PartitionerRunnerProcessStartedResult
        }


    let onRunModel (proxy : OnRunModelProxy) s (a: RunModelParam) : PartitionerRunnerProcessStartedResult =
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


    let setRunLimitProxy (c : PartitionerCallBackInfo) =
        {
            setRunLimit = c.setRunLimit
        }


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
            onRequestWork = onRequestWork (onRequestWorkProxy i)
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

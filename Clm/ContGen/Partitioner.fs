namespace ContGen

open System
open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.Logging
open ContGenServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open ClmSys.WorkerNodeData
open ServiceProxy.PartitionerProxy
open PartitionerServiceInfo.ServiceInfo
open ClmSys.TimerEvents
open Clm.CalculationData
open ServiceProxy.MsgProcessorProxy
open ClmSys.GeneralErrors
open ClmSys

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
            workerNodes : Map<WorkerNodeId, WorkerNodeState>
            //partitionerCallBackInfo : PartitionerCallBackInfo
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                workerNodes = Map.empty
                //partitionerCallBackInfo = PartitionerCallBackInfo.defaultValue
            }


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo * AsyncReplyChannel<UnitResult>
        | RunModel of RunModelParam * AsyncReplyChannel<ProcessStartedResult>
        | GetMessages
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    //let private className = "PartitionerRunner"
    //let private getMethodName n = className + "." + n
    //let private sendMessageName = getMethodName "sendMessage"
    //let private setRunLimitName = getMethodName "setRunLimit"
    //let private onRegisterName = getMethodName "onRegister"
    //let private tryGetNodeName = getMethodName "tryGetNode"
    //let private onTryRunModelWithRemoteIdName = getMethodName "onTryRunModelWithRemoteId"
    //let private onUnregisterName = getMethodName "onUnregister"
    //let private onStartName = getMethodName "onStart"
    //let private onUpdateProgressName = getMethodName "onUpdateProgress"
    //let private onSaveResultName = getMethodName "onSaveResult"
    //let private onSaveChartsName = getMethodName "onSaveCharts"
    //let private onProcessMessageName = getMethodName "onProcessMessage"
    //let private onGetMessagesName = getMethodName "onGetMessages"
    //let private tryGetRunnerName = getMethodName "tryGetRunner"
    //let private onRunModelName = getMethodName "onRunModel"
    //let private onGetStateName = getMethodName "onGetState"


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


    let onCompleted (proxy : OnCompletedProxy) r (s : PartitionerRunnerState) =
        proxy.tryDeleteRunModelParamWithRemoteId r |> ignore

        match tryFindRunningNode s.workerNodes r with
        | Some (w, n) ->
            let newNodeState = { n with runningProcesses = n.runningProcesses.tryRemove r }
            let result = proxy.saveWorkerNodeState newNodeState
            { s with workerNodes = s.workerNodes.Add (w, newNodeState) }, result
        | None -> s, Ok()


    let sendRunModelMessage sendMessage (e : RunModelParamWithRemoteId) workerNodeId m =
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
        |> sendMessage


    type OnTryRunModelWithRemoteIdProxy =
        {
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId option>
            tryGetNode : Map<WorkerNodeId, WorkerNodeState> -> WorkerNodeState option
            loadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            saveWorkerNodeState : WorkerNodeState -> UnitResult
            sendRunModelMessage : RunModelParamWithRemoteId -> WorkerNodeId -> ModelData -> UnitResult
            onCompleted : RemoteProcessId -> PartitionerRunnerState -> PartitionerRunnerState * UnitResult
        }


    let onTryRunModelWithRemoteId (proxy : OnTryRunModelWithRemoteIdProxy) s (e : RunModelParamWithRemoteId) =
        let addError f e = ((f |> OnTryRunModelWithRemoteIdErr |> PartitionerErr) + e) |> Error
        let toClmError e = e |> OnTryRunModelWithRemoteIdErr |> PartitionerErr
        let toError e = e |> toClmError |> Error

        match proxy.tryLoadResultData (e.runModelParam.callBackInfo.runQueueId.toResultDataId()) with
        | Ok None ->
            match proxy.tryGetNode s.workerNodes with
            | Some n ->
                match proxy.loadModelData e.runModelParam.commandLineParam.serviceAccessInfo e.runModelParam.callBackInfo.modelDataId with
                | Ok m ->
                    let a = proxy.sendRunModelMessage e n.workerNodeInfo.workerNodeId m
                    let newNodeState = { n with runningProcesses = n.runningProcesses.Add(e.remoteProcessId, e.runModelParam.callBackInfo.runQueueId) }
                    let b = proxy.saveWorkerNodeState newNodeState

                    let x =
                        {
                            processId = e.remoteProcessId |> RemoteProcess
                            runningProcessData = e.runModelParam.callBackInfo
                        }

                    let result = (x, combineUnitResults a b |> toErrorOption (toClmError TryRunModelWithRemoteIdErr)) |> StartedSuccessfully |> Ok
                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }, result
                | Error e ->
                    //logger.logErr (sprintf "%s: Unable to load model with id: %A" onTryRunModelWithRemoteIdName e.runModelParam.callBackInfo.modelDataId)
                    s, Error e
            | None -> s, UnableToGetWorkerNode |> toError
        | Ok (Some _) -> proxy.onCompleted e.remoteProcessId s, Ok AlreadyCompleted
        | Error e -> s, Error e


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


    let onUpdateProgress proxy s (i : RemoteProgressUpdateInfo) =
        printfn "%s: i = %A." onUpdateProgressName i
        i.toProgressUpdateInfo() |> s.partitionerCallBackInfo.onUpdateProgress

        match i.progress with
        | NotStarted -> s
        | InProgress _ -> s
        | Completed -> onCompleted proxy i.remoteProcessId s
        | Failed _ -> onCompleted proxy i.remoteProcessId s


    let onFailed s r (e : RunModelParamWithRemoteId) =
        (e.toRemoteProgressUpdateInfo (Failed (sprintf "Remote process %A failed because worker node %A has been unregistered." e.remoteProcessId r))).toProgressUpdateInfo() |> s.partitionerCallBackInfo.onUpdateProgress
        s


    let onUnregister proxy s (r : WorkerNodeId) =
        printfn "%s: r = %A." onUnregisterName r

        let removeNode g =
            proxy.tryDeleteWorkerNodeState r |> ignore
            { g with workerNodes = g.workerNodes.tryRemove r } |> setRunLimit

        match s.workerNodes.TryFind r with
        | Some n ->
            n.runningProcesses
            |> Map.toList
            |> List.map (fun (e, _) -> e)
            |> List.map proxy.tryLoadRunModelParamWithRemoteId
            |> List.choose id
            |> List.fold (fun acc e -> onFailed acc r e) (removeNode s)
        | None -> removeNode s


    let onSaveResult proxy s r =
        printfn "%s: r = %A." onSaveResultName r
        proxy.saveResultData r
        s


    let onSaveCharts proxy s (c : ChartInfo) =
        printfn "%s: resultDataId = %A." onSaveChartsName c.resultDataId
        proxy.saveCharts c
        s


    let onProcessMessage proxy (s : PartitionerRunnerState) (m : Message) =
        printfn "%s: m.messageId = %A." onProcessMessageName m.messageDataInfo.messageId
        match m.messageData with
        | PartitionerMsg x ->
            match x with
            | UpdateProgressPrtMsg i -> onUpdateProgress proxy s i
            | SaveResultPrtMsg r -> onSaveResult proxy s r
            | SaveChartsPrtMsg c -> onSaveCharts proxy s c
            | RegisterWorkerNodePrtMsg r ->
                let x = onRegister proxy s r
                printfn "%s: RegisterWorkerNodePrtMsg completed, state = %A." onProcessMessageName x
                x
            | UnregisterWorkerNodePrtMsg r -> onUnregister proxy s r
        | _ ->
            //p.logger.logErr (sprintf "%s: Invalid message type: %A." onProcessMessageName m.messageData)
            s


    let onGetMessages tryProcessMessage proxy s =
        printfn "%s: state: %A" onGetMessagesName s
        let y = List.foldWhileSome (fun x () -> tryProcessMessage x (onProcessMessage proxy)) PartitionerRunnerState.maxMessages s
        printfn "%s: completed, y = %A" onGetMessagesName y
        y


    let tryGetRunner s q =
        s.workerNodes
        |> Map.tryPick (fun _ b -> b.runningProcesses |> Map.tryPick (fun a b -> if b = q then Some a else None))


    type OnRunModelProxy =
        {
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId option>
            tryGetRunner : PartitionerRunnerState -> RunQueueId -> RemoteProcessId option
        }


    let onRunModel (proxy : OnRunModelProxy) s (a: RunModelParam) = // (r : AsyncReplyChannel<ProcessStartedResult>) =
        let reply q =
            {
                processId = q |> RemoteProcess
                runningProcessData = a.callBackInfo
            }
            |> StartedSuccessfully
            |> r.Reply

        let tryGetResult() = proxy.tryLoadResultData (a.callBackInfo.runQueueId.toResultDataId())

        match tryGetRunner s a.callBackInfo.runQueueId, tryGetResult() with
        | Some w, None ->
            printfn "%s: | Some %A, None" onRunModelName w
            reply w
            s
        | None, None ->
            printfn "%s: | None, None" onRunModelName
            let q = a.callBackInfo.runQueueId.toRemoteProcessId()

            let e =
                {
                    remoteProcessId = q
                    runModelParam = a
                }

            proxy.saveRunModelParamWithRemoteId e
            let (g, x) = onTryRunModelWithRemoteId sendMessage proxy s e
            // TODO kk:20191227 - Shall we remove model if onTryRunModelWithRemoteId failed???
            r.Reply x
            g
        | None, Some d ->
            // This can happen when there are serveral unprocessed messages in different mailbox processors.
            // Since we already have the result, we don't start the remote calculation again.
            printfn "%s: | None, Some %A" onRunModelName d.resultDataId
            r.Reply AlreadyCompleted
            s
        | Some w, Some d ->
            // This should not happen because we have the result and that means that
            // the relevant message was processed and that that should've removed the runner from the list.
            // Nevertless, we just let the duplicate calculation run.
            printfn "%s: Error - found running model: %A and result: %A." onRunModelName a.callBackInfo.runQueueId d.resultDataId
            printfn "%s: | Some %A, Some %A" onRunModelName w d.resultDataId
            reply w
            s


    let onGetState s = s, s


    type PartitionerRunner(w : PartitionerRunnerData) =
        let proxy = w.partitionerProxy
        let tryProcessMessage = onTryProcessMessage w.messageProcessorProxy
        let onStartProxy : OnStartProxy = 0


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : PartitionerRunnerState, c : PartitionerCallBackInfo) =
                    async
                        {
                            match! u.Receive() with
                            | Start (q, r) -> return! (onStart onStartProxy s|> (withReply r), q) |> loop
                            | RunModel (p, r) -> return! onRunModel w.messageProcessorProxy.sendMessage proxy s p r |> loop
                            | GetMessages ->return! onGetMessages tryProcessMessage proxy s |> loop
                            | GetState r -> return! (onGetState s|> (withReply r), c) |> loop
                        }

                (PartitionerRunnerState.defaultValue, PartitionerCallBackInfo.defaultValue) |> loop
                )


        member __.start q = messageLoop.PostAndReply (fun reply -> Start (q, reply))
        member __.runModel p = messageLoop.PostAndReply (fun reply -> RunModel (p, reply))
        member __.getMessages () = GetMessages |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue (i.logger.logExn "PartitionerRunner.createServiceImpl") w.getMessages)
        do h.start()
        w

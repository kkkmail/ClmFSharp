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

module Partitioner =

    type PartitionerCallBackInfo =
        {
            onUpdateProgress : ProgressUpdateInfo -> unit
            setRunLimit : int -> unit
        }

        static member defaultValue =
            {
                onUpdateProgress = fun _ -> ignore()
                setRunLimit = fun _ -> ignore()
            }


    type PartitionerRunnerParam =
        {
            partitionerMsgAccessInfo : PartitionerMsgAccessInfo
            partitionerProxy : PartitionerProxy
            messagingClient : MessageProcessorProxy
            //messagingService : IMessagingService
            //msgClientProxy : MessagingClientProxy
            logger : Logger
        }

        //member this.messagingClientData =
        //    {
        //        msgAccessInfo = this.partitionerMsgAccessInfo.messagingClientAccessInfo
        //        messagingService = this.messagingService
        //        msgClientProxy = this.msgClientProxy
        //        logger = this.logger
        //    }


    type PartitionerRunnerState =
        {
            workerNodes : Map<WorkerNodeId, WorkerNodeState>
            partitionerCallBackInfo : PartitionerCallBackInfo
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                partitionerCallBackInfo = PartitionerCallBackInfo.defaultValue
                workerNodes = Map.empty
            }


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo
        | RunModel of RunModelParam * AsyncReplyChannel<ProcessStartedResult>
        | GetMessages
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    let private className = "PartitionerRunner"
    let private getMethodName n = className + "." + n
    let private sendMessageName = getMethodName "sendMessage"
    let private setRunLimitName = getMethodName "setRunLimit"
    let private onRegisterName = getMethodName "onRegister"
    let private tryGetNodeName = getMethodName "tryGetNode"
    let private onTryRunModelWithRemoteIdName = getMethodName "onTryRunModelWithRemoteId"
    let private onUnregisterName = getMethodName "onUnregister"
    let private onStartName = getMethodName "onStart"
    let private onUpdateProgressName = getMethodName "onUpdateProgress"
    let private onSaveResultName = getMethodName "onSaveResult"
    let private onSaveChartsName = getMethodName "onSaveCharts"
    let private onProcessMessageName = getMethodName "onProcessMessage"
    let private onGetMessagesName = getMethodName "onGetMessages"
    let private tryGetRunnerName = getMethodName "tryGetRunner"
    let private onRunModelName = getMethodName "onRunModel"
    let private onGetStateName = getMethodName "onGetState"


    let setRunLimit x =
        printfn "%s: x: %A" setRunLimitName x
        let c =
            x.workerNodes
            |> Map.toList
            |> List.map snd
            |> List.fold (fun acc r -> r.workerNodeInfo.noOfCores + acc) 0

        x.partitionerCallBackInfo.setRunLimit c
        printfn "%s: completed." setRunLimitName
        x


    let onRegister (proxy : PartitionerProxy) s (r : WorkerNodeInfo) =
        printfn "%s: r = %A." onRegisterName r

        let updated q =
            let newState = { workerNodeInfo = r; runningProcesses = q }
            proxy.saveWorkerNodeState newState |> ignore
            { s with workerNodes = s.workerNodes.Add (r.workerNodeId, newState) }


        match s.workerNodes.TryFind r.workerNodeId with
        | Some n -> n.runningProcesses
        | None -> Map.empty
        |> updated
        |> setRunLimit


    let tryGetNode s =
        printfn "%s." tryGetNodeName

        let x =
            s.workerNodes
                |> Map.toList
                |> List.map (fun (_, v) -> v)
                |> List.sortBy (fun e -> e.priority)
                |> List.tryFind (fun e -> e.runningProcesses.Count < e.workerNodeInfo.noOfCores)

        printfn "%s: retVal = %A" tryGetNodeName x
        x


    let tryFindRunningNode s r =
        s.workerNodes
        |> Map.tryPick (fun a b -> b.runningProcesses |> Map.tryFind r |> Option.bind (fun _ -> Some (a, b)))


    let onCompleted (proxy : PartitionerProxy) r s =
        proxy.tryDeleteRunModelParamWithRemoteId r |> ignore

        match tryFindRunningNode s r with
        | Some (w, n) ->
            let newNodeState = { n with runningProcesses = n.runningProcesses.tryRemove r }
            proxy.saveWorkerNodeState newNodeState |> ignore
            { s with workerNodes = s.workerNodes.Add (w, newNodeState) }
        | None -> s


    let sendRunModelMessage sendMessage (e : RunModelParamWithRemoteId) n m =
        {
            workerNodeRecipient = n.workerNodeInfo.workerNodeId
            deliveryType = GuaranteedDelivery
            messageData =
                (
                    {
                        remoteProcessId = e.remoteProcessId
                        localProcessId = None
                        runningProcessData = { e.runModelParam.callBackInfo with workerNodeId = n.workerNodeInfo.workerNodeId }
                        taskParam = e.runModelParam.commandLineParam.taskParam
                        minUsefulEe = e.runModelParam.commandLineParam.serviceAccessInfo.minUsefulEe
                        commandLine = EmptyString
                    },
                    m
                )
                |> RunModelWrkMsg
        }.getMessageInfo()
        |> sendMessage


    let onTryRunModelWithRemoteId sendMessage (proxy : PartitionerProxy) s (e : RunModelParamWithRemoteId) =
        printfn "%s: e = %A." onTryRunModelWithRemoteIdName e
        let tryGetResult() = proxy.tryLoadResultData (e.runModelParam.callBackInfo.runQueueId.toResultDataId())

        match tryGetResult() with
        | None ->
            match tryGetNode s with
            | Some n ->
                printfn "%s: Using Node: %A." onTryRunModelWithRemoteIdName n

                match proxy.tryLoadModelData e.runModelParam.commandLineParam.serviceAccessInfo e.runModelParam.callBackInfo.modelDataId with
                | Some m ->
                    printfn "%s: using modelDataId: %A." onTryRunModelWithRemoteIdName m.modelDataId
                    sendRunModelMessage sendMessage e n m
                    let newNodeState = { n with runningProcesses = n.runningProcesses.Add(e.remoteProcessId, e.runModelParam.callBackInfo.runQueueId) }
                    printfn "%s: newNodeState = %A" onTryRunModelWithRemoteIdName newNodeState
                    proxy.saveWorkerNodeState newNodeState |> ignore

                    let x =
                        {
                            processId = e.remoteProcessId |> RemoteProcess
                            runningProcessData = e.runModelParam.callBackInfo
                        }
                        |> StartedSuccessfully

                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }, x
                | None ->
                    printfn "%s: cannot find model." onTryRunModelWithRemoteIdName
                    logger.logErr (sprintf "%s: Unable to load model with id: %A" onTryRunModelWithRemoteIdName e.runModelParam.callBackInfo.modelDataId)
                    s, FailedToStart
            | None ->
                printfn "%s: cannot find node to run." onTryRunModelWithRemoteIdName
                s, FailedToStart
        | Some _ -> onCompleted proxy e.remoteProcessId s, AlreadyCompleted


    let onStart (proxy : PartitionerProxy) s q =
        printfn "%s" onStartName

        let onStartRun g r = { g with workerNodes = g.workerNodes.Add (r.workerNodeInfo.workerNodeId, r) }
        let g = { s with partitionerCallBackInfo = q }
        let workers = proxy.loadAllWorkerNodeState()
        printfn "%s: workers = %A" onStartName workers

        workers
        |> List.fold (fun acc r -> onStartRun acc r) g
        |> setRunLimit


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


    let onRunModel sendMessage proxy s (a: RunModelParam) (r : AsyncReplyChannel<ProcessStartedResult>) =
        printfn "%s" onRunModelName

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


    let onGetState s (r : AsyncReplyChannel<PartitionerRunnerState>) =
        printfn "%s" onGetStateName
        r.Reply s
        s


    type PartitionerRunner(w : PartitionerRunnerParam) =
        let proxy = w.partitionerProxy
        let tryProcessMessage = onTryProcessMessage w.messagingClient


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start q -> return! timed onStartName onStart proxy s q |> loop
                            | RunModel (p, r) -> return! timed onRunModelName onRunModel w.messagingClient.sendMessage proxy s p r |> loop
                            | GetMessages ->return! timed onGetMessagesName onGetMessages tryProcessMessage proxy s |> loop
                            | GetState r -> return! timed onGetStateName onGetState s r |> loop
                        }

                PartitionerRunnerState.defaultValue |> loop
                )


        member __.start q = Start q |> messageLoop.Post
        member __.runModel p = messageLoop.PostAndReply (fun reply -> RunModel (p, reply))
        member __.getMessages () = GetMessages |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue (i.logger.logExn "PartitionerRunner.createServiceImpl") w.getMessages)
        do h.start()
        w

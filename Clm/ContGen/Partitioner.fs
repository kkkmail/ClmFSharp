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
open Messaging.ServiceResponse
open ClmSys.WorkerNodeData
open ServiceProxy.PartitionerProxy
open PartitionerServiceInfo.ServiceInfo
open ClmSys.TimerEvents
open Clm.CalculationData

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
            messagingService : IMessagingService
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.partitionerMsgAccessInfo.messagingClientAccessInfo
                messagingService = this.messagingService
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type PartitionerRunnerState =
        {
            workerNodes : Map<WorkerNodeId, WorkerNodeState>
            partitionerQueue : list<PartitionerQueueElement>
            partitionerCallBackInfo : PartitionerCallBackInfo
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                partitionerCallBackInfo = PartitionerCallBackInfo.defaultValue
                workerNodes = Map.empty
                partitionerQueue = []
            }


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo
        | RunModel of RunModelParam * AsyncReplyChannel<ProcessStartedResult>
        | GetMessages
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    type PartitionerRunner(p : PartitionerRunnerParam) =
        let className = "PartitionerRunner"
        let getMethodName n = className + "." + n
        let sendMessageName = getMethodName "sendMessage"
        let setRunLimitName = getMethodName "setRunLimit"
        let onRegisterName = getMethodName "onRegister"
        let tryGetNodeName = getMethodName "tryGetNode"
        let onCannotRunName = getMethodName "onCannotRun"
        let onRunOrCompletedName = getMethodName "onRunOrCompleted"
        let onTryRunModelWithRemoteIdName = getMethodName "onTryRunModelWithRemoteId"
        let onUnregisterName = getMethodName "onUnregister"
        let onStartName = getMethodName "onStart"
        let onUpdateProgressName = getMethodName "onUpdateProgress"
        let onSaveResultName = getMethodName "onSaveResult"
        let onSaveChartsName = getMethodName "onSaveCharts"
        let onProcessMessageName = getMethodName "onProcessMessage"
        let onGetMessagesName = getMethodName "onGetMessages"
        let tryGetRunnerName = getMethodName "tryGetRunner"
        let onRunModelName = getMethodName "onRunModel"

        let messagingClient = MessagingClient p.messagingClientData
        do messagingClient.start()

        let tryLoadModelData = p.partitionerProxy.tryLoadModelData
        let logger = p.logger
        let logErr = logger.logErr
        let proxy = p.partitionerProxy


        let sendMessage (m : MessageInfo) =
            printfn "%s: recipient: %A" sendMessageName m.recipientInfo.recipient
            messagingClient.sendMessage m


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


        let onRegister s (r : WorkerNodeInfo) =
            printfn "%s: r = %A." onRegisterName r

            let updated q =
                let newState = { workerNodeInfo = r; runningProcesses = q }
                proxy.saveWorkerNodeState newState |> ignore
                { s with workerNodes = s.workerNodes.Add (r.workerNodeId, newState) }

            match s.workerNodes.TryFind r.workerNodeId with
            | Some n -> n.runningProcesses
            | None -> []
            |> updated
            |> setRunLimit


        let tryGetNode s =
            printfn "%s." tryGetNodeName

            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v)
                    |> List.sortBy (fun e -> e.priority)
                    |> List.tryFind (fun e -> e.runningProcesses.Length < e.workerNodeInfo.noOfCores)

            printfn "%s: retVal = %A" tryGetNodeName x
            x


        let tryFindRunningNode s r =
            s.workerNodes
            |> Map.toList
            |> List.tryPick (fun (a, b) -> if List.contains r (b.runningProcesses |> List.map (fun e -> e.remoteProcessId)) then Some a else None)


        let onCompleted r l s =
            proxy.tryDeleteRunModelParamWithRemoteId r |> ignore

            match tryFindRunningNode s r with
            | Some x ->
                match s.workerNodes.TryFind x with
                | Some n ->
                    let newNodeState = { n with runningProcesses = n.runningProcesses |> List.filter (fun e -> e.remoteProcessId <> r) }
                    proxy.saveWorkerNodeState newNodeState |> ignore
                    { s with workerNodes = s.workerNodes.Add (x, newNodeState) }
                | None -> s
                |> l
            | None -> s


        let onCannotRun (e : RunModelParamWithRemoteId) g =
            printfn "%s." onCannotRunName
            proxy.savePartitionerQueueElement e.queueElement
            { g with partitionerQueue = e.queueElement :: g.partitionerQueue |> List.distinctBy (fun e -> e.queuedRemoteProcessId) }


        let onRunOrCompleted (e : RunModelParamWithRemoteId) g =
            printfn "%s." onRunOrCompletedName

            match proxy.tryDeletePartitionerQueueElement e.remoteProcessId with
            | Some _ ->
                printfn "%s: Deleted queue element with id: %A" onRunOrCompletedName e.remoteProcessId
                ignore()
            | None ->
                // This is not an error if queue element is not found.
                printfn "%s: Cannot delete queue element with id: %A" onRunOrCompletedName e.remoteProcessId
                ignore()

            { g with partitionerQueue = g.partitionerQueue |> List.filter (fun a -> a.queuedRemoteProcessId <> e.remoteProcessId) }


        let sendRunModelMessage (e : RunModelParamWithRemoteId) n m =
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
                        },
                        m
                    )
                    |> RunModelWrkMsg
            }.getMessageInfo()
            |> sendMessage


        let onTryRunModelWithRemoteId s (e : RunModelParamWithRemoteId) =
            printfn "%s: e = %A." onTryRunModelWithRemoteIdName e
            let tryGetResult() = proxy.tryLoadResultData (e.runModelParam.callBackInfo.runQueueId.toResultDataId())
            let onCannotRun g = onCannotRun e g
            let onRunOrCompleted g = onRunOrCompleted e g
            let sendRunModelMessage n m = sendRunModelMessage e n m

            match tryGetResult() with
            | None ->
                match tryGetNode s with
                | Some n ->
                    printfn "%s: Using Node: %A." onTryRunModelWithRemoteIdName n

                    match tryLoadModelData e.runModelParam.commandLineParam.serviceAccessInfo e.runModelParam.callBackInfo.modelDataId with
                    | Some m ->
                        printfn "%s: using modelDataId: %A." onTryRunModelWithRemoteIdName m.modelDataId
                        sendRunModelMessage n m

                        let i =
                            {
                                remoteProcessId = e.remoteProcessId
                                runQueueId = e.runModelParam.callBackInfo.runQueueId
                            }

                        let newNodeState = { n with runningProcesses = i :: n.runningProcesses }
                        printfn "%s: newNodeState = %A" onTryRunModelWithRemoteIdName newNodeState
                        proxy.saveWorkerNodeState newNodeState |> ignore
                        { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }
                        |> onRunOrCompleted
                    | None ->
                        printfn "%s: cannot find model." onTryRunModelWithRemoteIdName
                        logger.logErr (sprintf "%s: Unable to load model with id: %A" onTryRunModelWithRemoteIdName e.runModelParam.callBackInfo.modelDataId)
                        onCannotRun s
                | None ->
                    printfn "%s: cannot find node to run." onTryRunModelWithRemoteIdName
                    onCannotRun s
            | Some _ -> onRunOrCompleted s |> onCompleted e.remoteProcessId id


        let onUnregister s (r : WorkerNodeId) =
            printfn "%s: r = %A." onUnregisterName r

            let removeNode g =
                proxy.tryDeleteWorkerNodeState r |> ignore
                { g with workerNodes = g.workerNodes.tryRemove r } |> setRunLimit

            match s.workerNodes.TryFind r with
            | Some n ->
                n.runningProcesses
                |> List.map (fun e -> e.remoteProcessId)
                |> List.map proxy.tryLoadRunModelParamWithRemoteId
                |> List.choose id
                |> List.fold (fun acc e -> onTryRunModelWithRemoteId acc e) (removeNode s)
            | None -> removeNode s


        let loadQueue s =
            proxy.loadAllPartitionerQueueElement()
            |> List.map (fun e -> proxy.tryLoadRunModelParamWithRemoteId e.queuedRemoteProcessId)
            |> List.choose id
            |> List.fold (fun acc e -> onTryRunModelWithRemoteId acc e) s


        let onStart s q =
            printfn "%s" onStartName

            let onStartRun g r = { g with workerNodes = g.workerNodes.Add (r.workerNodeInfo.workerNodeId, r) }

            let g = loadQueue { s with partitionerCallBackInfo = q }
            let workers = proxy.loadAllWorkerNodeState()
            printfn "%s: workers = %A" onStartName workers

            workers
            |> List.fold (fun acc r -> onStartRun acc r) g
            |> setRunLimit


        let onUpdateProgress s (i : RemoteProgressUpdateInfo) =
            printfn "%s: i = %A." onUpdateProgressName i
            i.toProgressUpdateInfo() |> s.partitionerCallBackInfo.onUpdateProgress

            match i.progress with
            | NotStarted -> s
            | InProgress _ -> s
            | Completed -> onCompleted i.remoteProcessId loadQueue s


        let onSaveResult s r =
            printfn "%s: r = %A." onSaveResultName r
            proxy.saveResultData r
            s


        let onSaveCharts s (c : ChartInfo) =
            printfn "%s: resultDataId = %A." onSaveChartsName c.resultDataId
            proxy.saveCharts c
            s


        let onProcessMessage (s : PartitionerRunnerState) (m : Message) =
            printfn "%s: m.messageId = %A." onProcessMessageName m.messageDataInfo.messageId
            match m.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i -> onUpdateProgress s i
                | SaveResultPrtMsg r -> onSaveResult s r
                | SaveChartsPrtMsg c -> onSaveCharts s c
                | RegisterWorkerNodePrtMsg r ->
                    let x = onRegister s r
                    printfn "%s: RegisterWorkerNodePrtMsg completed, state = %A." onProcessMessageName x
                    x
                | UnregisterWorkerNodePrtMsg r -> onUnregister s r
            | _ ->
                p.logger.logErr (sprintf "%s: Invalid message type: %A." onProcessMessageName m.messageData)
                s


        let onGetMessages s =
            printfn "%s: state: %A" onGetMessagesName s
            let y = List.foldWhileSome (fun x () -> messagingClient.tryProcessMessage x onProcessMessage) PartitionerRunnerState.maxMessages s
            printfn "%s: completed, y = %A" onGetMessagesName y
            y


        let tryGetRunner s q =
            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v.runningProcesses)
                    |> List.concat
            printfn "%s: q = %A, x = %A" tryGetRunnerName q x
            x
            |> List.tryFind (fun e -> e.runQueueId = q)


        let onRunModel s (a: RunModelParam) (r : AsyncReplyChannel<ProcessStartedResult>) =
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
                reply w.remoteProcessId
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
                reply q
                onTryRunModelWithRemoteId s e
            | None, Some d ->
                // This can happen when there are serveral unprocessed messages in different mailbox processors.
                // Since we already have the result, we don't start the remote calculation again.
                printfn "%s: | None, Some %A" onRunModelName d.resultDataId
                r.Reply AlreadyCompleted
                s
            | Some w, Some d ->
                // This should not happen because we have the result and that means that
                // the relevant message was processed and that that shoud've removed the runner from the list.
                // Nevertless, we just let the duplicate calculation run.
                printfn "%s: Error - found running model: %A and result: %A." onRunModelName w.runQueueId d.resultDataId
                printfn "%s: | Some %A, Some %A" onRunModelName w d.resultDataId
                reply w.remoteProcessId
                s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start q -> return! timed onStartName onStart s q |> loop
                            | RunModel (p, r) -> return! timed onRunModelName onRunModel s p r |> loop
                            | GetMessages ->return! timed onGetMessagesName onGetMessages s |> loop
                            | GetState r -> r.Reply s
                        }

                PartitionerRunnerState.defaultValue |> loop
                )


        member __.start q = Start q |> messageLoop.Post
        member __.runModel p = messageLoop.PostAndReply (fun reply -> RunModel (p, reply))
        member __.getMessages() = GetMessages |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue (i.logger.logExn "PartitionerRunner.createServiceImpl") w.getMessages)
        do h.start()
        w

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
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.partitionerMsgAccessInfo.messagingClientAccessInfo
                msgResponseHandler = this.msgResponseHandler
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


    and PartitionerRunner(p : PartitionerRunnerParam) =
        let messagingClient = MessagingClient p.messagingClientData
        let tryLoadModelData = p.partitionerProxy.tryLoadModelData
        let logger = p.logger
        let logErr = logger.logErr
        let proxy = p.partitionerProxy


        let sendMessage m =
            printfn "PartitionerRunner.sendMessage: recipient: %A" m.recipient
            messagingClient.sendMessage m


        let setRunLimit x =
            printfn "PartitionerRunner.setRunLimit: x: %A" x
            let c =
                x.workerNodes
                |> Map.toList
                |> List.map snd
                |> List.fold (fun acc r -> r.workerNodeInfo.noOfCores + acc) 0

            x.partitionerCallBackInfo.setRunLimit c
            printfn "PartitionerRunner.setRunLimit completed."
            x


        let onRegister s (r : WorkerNodeInfo) =
            printfn "PartitionerRunner.onRegister: r = %A." r

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
            printfn "PartitionerRunner.tryGetNode."

            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v)
                    |> List.sortBy (fun e -> e.priority)
                    |> List.tryFind (fun e -> e.runningProcesses.Length < e.workerNodeInfo.noOfCores)
            printfn "PartitionerRunner.tryGetNode: retVal = %A" x
            x


        let onRunModelWithRemoteId s (e : RunModelParamWithRemoteId) =
            printfn "PartitionerRunner.onRunModelWithRemoteId: e = %A." e

            let onCannotRun() =
                printfn "PartitionerRunner.onCannotRun"
                proxy.savePartitionerQueueElement e.queueElement
                { s with partitionerQueue = e.queueElement :: s.partitionerQueue }

            match tryGetNode s with
            | Some n ->
                printfn "PartitionerRunner.onRunModelWithRemoteId: Using Node: %A." n
                match tryLoadModelData e.runModelParam.commandLineParam.serviceAccessInfo e.runModelParam.callBackInfo.modelDataId with
                | Some m ->
                    printfn "PartitionerRunner.onRunModelWithRemoteId: using modelDataId: %A." m.modelDataId
                    {
                        workerNodeRecipient = n.workerNodeInfo.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            (
                                {
                                    remoteProcessId = e.remoteProcessId
                                    modelDataId = m.modelDataId
                                    taskParam = e.runModelParam.commandLineParam.taskParam
                                    runQueueId = e.runModelParam.callBackInfo.runQueueId
                                    minUsefulEe = e.runModelParam.commandLineParam.serviceAccessInfo.minUsefulEe
                                },
                                m
                            )
                            |> RunModelWrkMsg
                    }.messageInfo
                    |> sendMessage

                    let i =
                        {
                            remoteProcessId = e.remoteProcessId
                            runQueueId = e.runModelParam.callBackInfo.runQueueId
                        }

                    let newNodeState = { n with runningProcesses = i :: n.runningProcesses }
                    proxy.saveWorkerNodeState newNodeState |> ignore
                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }
                | None ->
                    printfn "PartitionerRunner.onRunModelWithRemoteId - cannot find model."
                    logger.logErr (sprintf "Unable to load model with id: %A" e.runModelParam.callBackInfo.modelDataId)
                    onCannotRun()
            | None ->
                printfn "PartitionerRunner.onRunModelWithRemoteId - cannot find node to run."
                onCannotRun()


        let onUnregister s (r : WorkerNodeId) =
            printfn "PartitionerRunner.onUnregister: r = %A." r

            let removeNode g =
                proxy.tryDeleteWorkerNodeState r |> ignore
                { g with workerNodes = g.workerNodes.tryRemove r } |> setRunLimit

            match s.workerNodes.TryFind r with
            | Some n ->
                n.runningProcesses
                |> List.map (fun e -> e.remoteProcessId)
                |> List.map proxy.tryLoadRunModelParamWithRemoteId
                |> List.choose id
                |> List.fold (fun acc e ->  onRunModelWithRemoteId acc e) s
            | None -> s
            |> removeNode


        /// TODO kk:20190825 - Refactor to make it better.
        let loadQueue s =
            let queue = proxy.loadAllPartitionerQueueElement()

            let g =
                queue
                |> List.map (fun e -> proxy.tryLoadRunModelParamWithRemoteId e.queuedRemoteProcessId)
                |> List.choose id
                |> List.fold (fun acc e ->  onRunModelWithRemoteId acc e) s

            queue |> List.map (fun e -> proxy.tryDeletePartitionerQueueElement e.queuedRemoteProcessId) |> ignore
            g


        let onStart s q =
            printfn "PartitionerRunner.onStart"
            let onStartRun g r = { g with workerNodes = g.workerNodes.Add (r.workerNodeInfo.workerNodeId, r) }
            let g = loadQueue { s with partitionerCallBackInfo = q }
            let workers = proxy.loadAllWorkerNodeState()
            printfn "PartitionerRunner.onStart: workers = %A" workers

            workers
            |> List.fold (fun acc r -> onStartRun acc r) g
            |> setRunLimit

        let tryFindRunningNode s r =
            s.workerNodes
            |> Map.toList
            |> List.tryPick (fun (a, b) -> if List.contains r (b.runningProcesses |> List.map (fun e -> e.remoteProcessId)) then Some a else None)


        let onUpdateProgress s (i : RemoteProgressUpdateInfo) =
            printfn "PartitionerRunner.onUpdateProgress: i = %A." i
            s.partitionerCallBackInfo.onUpdateProgress i.progressUpdateInfo

            match i.progress with
            | NotStarted -> s
            | InProgress _ -> s
            | Completed ->
                proxy.tryDeleteRunModelParamWithRemoteId i.updatedRemoteProcessId |> ignore

                match tryFindRunningNode s i.updatedRemoteProcessId with
                | Some x ->
                    let g = loadQueue s
                    match g.workerNodes.TryFind x with
                    | Some n ->
                        let newNodeState = { n with runningProcesses = n.runningProcesses |> List.filter (fun e -> e.remoteProcessId <> i.updatedRemoteProcessId) }
                        proxy.saveWorkerNodeState newNodeState |> ignore
                        { g with workerNodes = g.workerNodes.Add (x, newNodeState) }
                    | None -> g
                | None -> s


        let onSaveResult s r =
            printfn "PartitionerRunner.onSaveResult: r = %A." r
            proxy.saveResultData r
            s


        let onSaveCharts s (c : ChartInfo) =
            printfn "PartitionerRunner.onSaveCharts: resultDataId = %A." c.resultDataId
            proxy.saveCharts c
            s


        let onProcessMessage (s : PartitionerRunnerState) (m : Message) =
            printfn "PartitionerRunner.onProcessMessage: m.messageId = %A." m.messageId
            match m.messageInfo.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i ->
                    onUpdateProgress s i
                | SaveResultPrtMsg r -> onSaveResult s r
                | SaveChartsPrtMsg c -> onSaveCharts s c
                | RegisterWorkerNodePrtMsg r ->
                    let x = onRegister s r
                    printfn "PartitionerRunner.onProcessMessage.RegisterWorkerNodePrtMsg completed, state = %A." x
                    x
                | UnregisterWorkerNodePrtMsg r -> onUnregister s r
            | _ ->
                p.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)
                s


        let onGetMessages s =
            async {
                printfn "PartitionerRunner.onGetMessages, state: %A" s
                let! y = List.foldWhileSomeAsync (fun x () -> messagingClient.tryProcessMessage x onProcessMessage) PartitionerRunnerState.maxMessages s
                printfn "PartitionerRunner.onGetMessages completed, y = %A" y
                return y
            }


        let tryGetRunner s q =
            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v.runningProcesses)
                    |> List.concat
            printfn "PartitionerRunner.tryGetRunner: q = %A, x = %A" q x
            x
            |> List.tryFind (fun e -> e.runQueueId = q)


        let onRunModel s (a: RunModelParam) (r : AsyncReplyChannel<ProcessStartedResult>) =
            printfn "PartitionerRunner.onRunModel"

            let reply q =
                {
                    processId = q |> RemoteProcess
                    processToStartInfo =
                        {
                            modelDataId = a.callBackInfo.modelDataId
                            runQueueId = a.callBackInfo.runQueueId
                        }
                }
                |> StartedSuccessfully
                |> r.Reply

            let tryGetResult() = proxy.tryLoadResultData (a.callBackInfo.runQueueId.toResultDataId())

            match tryGetRunner s a.callBackInfo.runQueueId, tryGetResult() with
            | Some w, None ->
                printfn "PartitionerRunner.onRunModel - | Some %A, None" w
                reply w.remoteProcessId
                s
            | None, None ->
                printfn "PartitionerRunner.onRunModel - | None, None"
                let q = a.callBackInfo.runQueueId.toRemoteProcessId()

                let e =
                    {
                        remoteProcessId = q
                        runModelParam = a
                    }

                proxy.saveRunModelParamWithRemoteId e
                reply q
                onRunModelWithRemoteId s e
            | None, Some d ->
                // This can happen when there are serveral unprocessed messages in different mailbox processors.
                // Since we already have the result, we don't start the remote calculation again.
                printfn "PartitionerRunner.onRunModel - | None, Some %A" d.resultDataId
                r.Reply AlreadyCompleted
                s
            | Some w, Some d ->
                // This should not happen because we have the result and that means that
                // the relevant message was processed and that that shoud've removed the runner from the list.
                // Nevertless, we just let the duplicate calculation run.
                printfn "PartitionerRunner.onRunModel: Error - found running model: %A and result: %A." w.runQueueId d.resultDataId
                printfn "PartitionerRunner.onRunModel - | Some %A, Some %A" w d.resultDataId
                reply w.remoteProcessId
                s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start q -> return! onStart s q |> loop
                            | RunModel (p, r) -> return! onRunModel s p r |> loop
                            | GetMessages ->
                                let! ns =onGetMessages s 
                                return! ns |> loop
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
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w

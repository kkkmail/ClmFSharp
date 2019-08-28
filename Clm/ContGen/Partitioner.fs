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
        | Start of PartitionerRunner * PartitionerCallBackInfo
        | Register of WorkerNodeInfo
        | Unregister of PartitionerRunner * WorkerNodeId
        | UpdateProgress of PartitionerRunner * RemoteProgressUpdateInfo
        | RunModel of RunModelParam * RemoteProcessId
        | RunModelWithRemoteId of RunModelParamWithRemoteId
        | SaveCharts of ChartInfo
        | SaveResult of ResultDataWithId
        | GetMessages of PartitionerRunner
        | GetState of AsyncReplyChannel<PartitionerRunnerState>


    and PartitionerRunner(p : PartitionerRunnerParam) =
        let messagingClient = MessagingClient p.messagingClientData
        let tryLoadModelData = p.partitionerProxy.tryLoadModelData
        let logger = p.logger
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
                let newState = { workerNodeInfo = r; running = q }
                proxy.saveWorkerNodeState newState |> ignore
                { s with workerNodes = s.workerNodes.Add (r.workerNodeId, newState) }

            match s.workerNodes.TryFind r.workerNodeId with
            | Some n -> n.running
            | None -> []
            |> updated
            |> setRunLimit


        let onUnregister s (w : PartitionerRunner) (r : WorkerNodeId) =
            printfn "PartitionerRunner.onUnregister: r = %A." r

            match s.workerNodes.TryFind r with
            | Some n ->
                n.running
                |> List.map proxy.tryLoadRunModelParamWithRemoteId
                |> List.choose id
                |> List.map w.runModelWithRemoteId
                |> ignore
            | None -> ignore()

            proxy.tryDeleteWorkerNodeState r |> ignore
            { s with workerNodes = s.workerNodes.tryRemove r } |> setRunLimit


        /// TODO kk:20190825 - Refactor to make it better.
        let loadQueue (w : PartitionerRunner) =
            let queue = proxy.loadAllPartitionerQueueElement()
            
            queue
            |> List.map (fun e -> proxy.tryLoadRunModelParamWithRemoteId e.queuedRemoteProcessId)
            |> List.choose id
            |> List.map w.runModelWithRemoteId
            |> ignore

            queue |> List.map (fun e -> proxy.tryDeleteRunModelParamWithRemoteId e.queuedRemoteProcessId) |> ignore


        let onStart s w q =
            printfn "PartitionerRunner.onStart"
            loadQueue w
            let workers = proxy.loadAllWorkerNodeState()
            workers |> List.fold (fun acc r -> onRegister acc r.workerNodeInfo) { s with partitionerCallBackInfo = q }


        let tryFindRunningNode s r =
            s.workerNodes
            |> Map.toList
            |> List.tryPick (fun (a, b) -> if List.contains r b.running then Some a else None)


        let onUpdateProgress s w (i : RemoteProgressUpdateInfo) =
            printfn "PartitionerRunner.onUpdateProgress: i = %A." i
            s.partitionerCallBackInfo.onUpdateProgress i.progressUpdateInfo

            match i.progress with
            | NotStarted -> s
            | InProgress _ -> s
            | Completed ->
                proxy.tryDeleteRunModelParamWithRemoteId i.updatedRemoteProcessId |> ignore

                match tryFindRunningNode s i.updatedRemoteProcessId with
                | Some x ->
                    loadQueue w
                    match s.workerNodes.TryFind x with
                    | Some n ->
                        let newNodeState = { n with running = n.running |> List.filter (fun e -> e <> i.updatedRemoteProcessId) }
                        proxy.saveWorkerNodeState newNodeState |> ignore
                        { s with workerNodes = s.workerNodes.Add (x, newNodeState) }
                    | None -> s
                | None -> s


        let onSaveResult s r =
            printfn "PartitionerRunner.onSaveResult: r = %A." r
            proxy.saveResultData r
            s


        let onSaveCharts s (c : ChartInfo) =
            printfn "PartitionerRunner.onSaveCharts: resultDataId = %A." c.resultDataId
            proxy.saveCharts c
            s


        let onProcessMessage (w : PartitionerRunner) (s : PartitionerRunnerState) (m : Message) =
            printfn "PartitionerRunner.onProcessMessage: m.messageId = %A." m.messageId
            match m.messageInfo.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i ->
                    w.updateProgress i
                    s
                | SaveResultPrtMsg r -> onSaveResult s r
                | SaveChartsPrtMsg c -> onSaveCharts s c
                | RegisterWorkerNodePrtMsg r ->
                    let x = onRegister s r
                    printfn "PartitionerRunner.onProcessMessage.RegisterWorkerNodePrtMsg completed, state = %A." x
                    x
                | UnregisterWorkerNodePrtMsg r -> onUnregister s w r
            | _ ->
                p.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)
                s


        let onGetMessages s (w : PartitionerRunner) =
            printfn "PartitionerRunner.onGetMessages, state: %A" s
            let y = List.foldWhileSome (fun x () -> messagingClient.tryProcessMessage x (onProcessMessage w)) PartitionerRunnerState.maxMessages s
            printfn "PartitionerRunner.onGetMessages completed, y = %A" y
            y


        let tryGetNode s =
            printfn "PartitionerRunner.tryGetNode."

            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v)
                    |> List.sortBy (fun e -> (e.workerNodeInfo.nodePriority.value, (decimal e.running.Length) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))))
                    |> List.tryPick (fun e -> if e.running.Length < e.workerNodeInfo.noOfCores then Some e else None)
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

                    let newNodeState = { n with running = e.remoteProcessId :: n.running }
                    proxy.saveWorkerNodeState newNodeState |> ignore
                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, newNodeState) }
                | None ->
                    printfn "PartitionerRunner.onRunModelWithRemoteId - cannot find model."
                    logger.logErr (sprintf "Unable to load model with id: %A" e.runModelParam.callBackInfo.modelDataId)
                    onCannotRun()
            | None ->
                printfn "PartitionerRunner.onRunModelWithRemoteId - cannot find node to run."
                onCannotRun()


        let onRunModel s (a: RunModelParam) (q : RemoteProcessId) =
            printfn "PartitionerRunner.onRunModel: q = %A." q

            let e =
                {
                    remoteProcessId = q
                    runModelParam = a
                }

            proxy.saveRunModelParamWithRemoteId e
            onRunModelWithRemoteId s e


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (w, q) -> return! onStart s w q |> loop
                            | Register r -> return! onRegister s r |> loop
                            | Unregister (w, r) -> return! onUnregister s w r |> loop
                            | UpdateProgress (w, i) -> return! onUpdateProgress s w i |> loop
                            | RunModel (p, q) -> return! onRunModel s p q |> loop
                            | RunModelWithRemoteId e -> return! onRunModelWithRemoteId s e |> loop
                            | SaveResult r -> return! onSaveResult s r |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | GetState w -> w.Reply s
                        }

                PartitionerRunnerState.defaultValue |> loop
                )


        let runModelImpl p =
            let q = Guid.NewGuid() |> RemoteProcessId
            printfn "PartitionerRunner.runModelImpl: q = %A, p = %A." q p
            (p, q) |> RunModel |> messageLoop.Post

            {
                processId = q |> RemoteProcess
                processToStartInfo =
                    {
                        modelDataId = p.callBackInfo.modelDataId
                        runQueueId = p.callBackInfo.runQueueId
                    }
            }


        member this.start q = (this, q) |> Start |> messageLoop.Post
        member __.runModel p = runModelImpl p
        member private __.runModelWithRemoteId e = e |> RunModelWithRemoteId |> messageLoop.Post
        member private this.updateProgress i = (this, i) |> UpdateProgress |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w

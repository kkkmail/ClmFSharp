﻿namespace ContGen

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

    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            running : list<RemoteProcessId>
        }


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
        | UpdateProgress of PartitionerRunner * RemoteProgressUpdateInfo
        | RunModel of RunModelParam * RemoteProcessId
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
            proxy.saveWorkerNodeInfo r
            let updated q = { s with workerNodes = s.workerNodes.Add (r.workerNodeId, { workerNodeInfo = r; running = q }) }

            match s.workerNodes.TryFind r.workerNodeId with
            | Some n -> n.running
            | None -> []
            |> updated
            |> setRunLimit


        /// TODO kk:20190825 - Refactor to make it better.
        let loadQueue (w : PartitionerRunner) =
            let queue = proxy.loadAllPartitionerQueueElement()
            queue |> List.map (fun e -> w.runModel(e.runModelParam, e.remoteProcessId)) |> ignore
            queue |> List.map (fun e -> proxy.tryDeletePartitionerQueueElement e.remoteProcessId) |> ignore


        let onStart s w q =
            printfn "PartitionerRunner.onStart"
            loadQueue w
            let workers = proxy.loadAllWorkerNodeInfo()
            workers |> List.fold (fun acc r -> onRegister acc r) { s with partitionerCallBackInfo = q }


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
                match tryFindRunningNode s i.updatedRemoteProcessId with
                | Some x ->
                    loadQueue w
                    match s.workerNodes.TryFind x with
                    | Some n ->
                        { s with workerNodes = s.workerNodes.Add (x, { n with running = n.running |> List.filter (fun e -> e <> i.updatedRemoteProcessId) }) }
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

            | _ ->
                p.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)
                s


        let onGetMessages s (w : PartitionerRunner) =
            printfn "PartitionerRunner.onGetMessages"
            printfn "PartitionerRunnerState: %A" s
            let y = List.foldWhileSome (fun x () -> messagingClient.tryProcessMessage x (onProcessMessage w)) PartitionerRunnerState.maxMessages s
            printfn "PartitionerRunner.onGetMessages completed, y = %A" y
            y


        let tryGetNode s =
            printfn "PartitionerRunner.tryGetNode."

            let x =
                s.workerNodes
                    |> Map.toList
                    |> List.map (fun (_, v) -> v)
                    |> List.sortBy (fun e -> (e.workerNodeInfo.nodePriority, (decimal e.running.Length) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))))
                    |> List.tryPick (fun e -> if e.running.Length < e.workerNodeInfo.noOfCores then Some e else None)
            printfn "PartitionerRunner.tryGetNode: retVal = %A" x
            x

        let onRunModel s (a: RunModelParam) (q : RemoteProcessId) =
            printfn "PartitionerRunner.onRunModel: q = %A." q

            let onCannotRun() =
                printfn "PartitionerRunner.onCannotRun"
                let e =
                    {
                        remoteProcessId = q
                        runModelParam = a
                    }

                proxy.savePartitionerQueueElement e
                { s with partitionerQueue = e :: s.partitionerQueue }


            match tryGetNode s with
            | Some n ->
                printfn "PartitionerRunner.onRunModel: Using Node: %A." n
                match tryLoadModelData a.commandLineParam.serviceAccessInfo a.callBackInfo.modelDataId with
                | Some m ->
                    printfn "PartitionerRunner.onRunModel: using modelDataId: %A." m.modelDataId
                    {
                        workerNodeRecipient = n.workerNodeInfo.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            (
                                {
                                    remoteProcessId = q
                                    modelDataId = m.modelDataId
                                    taskParam = a.commandLineParam.taskParam
                                    runQueueId = a.callBackInfo.runQueueId
                                    minUsefulEe = a.commandLineParam.serviceAccessInfo.minUsefulEe
                                },
                                m
                            )
                            |> RunModelWrkMsg
                    }.messageInfo
                    |> sendMessage

                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, { n with running = q :: n.running }) }
                | None ->
                    printfn "PartitionerRunner.onRunModel - cannot find model."
                    logger.logErr (sprintf "Unable to load model with id: %A" a.callBackInfo.modelDataId)
                    onCannotRun()
            | None ->
                printfn "PartitionerRunner.onRunModel - cannot find node to run."
                onCannotRun()


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (w, q) -> return! onStart s w q |> loop
                            | Register r -> return! onRegister s r |> loop
                            | UpdateProgress (w, i) -> return! onUpdateProgress s w i |> loop
                            | RunModel (p, q) -> return! onRunModel s p q |> loop
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
        member private __.runModel(p, q) = (p, q) |> RunModel |> messageLoop.Post
        member private this.updateProgress i = (this, i) |> UpdateProgress |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w

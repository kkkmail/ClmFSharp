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
            //onStarted : ProcessStartInfo -> unit
        }

        static member defaultValue =
            {
                onUpdateProgress = fun _ -> ignore()
                //onStarted = fun _ -> ignore()
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
            partitionerCallBackInfo : PartitionerCallBackInfo
            workerNodes : Map<WorkerNodeId, WorkerNodeState>
            partitionerQueue : list<PartitionerQueueElement>
        }

        static member defaultValue =
            {
                partitionerCallBackInfo =
                    {
                        onUpdateProgress = fun _ -> ignore()
                        //onStarted = fun _ -> ignore()
                    }

                workerNodes = Map.empty
                partitionerQueue = []
            }


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo
        | Register of WorkerNodeInfo
        | UpdateProgress of RemoteProgressUpdateInfo
        | RunModel of RunModelParam * RemoteProcessId
        | SaveCharts of ChartInfo
        | SaveResult of ResultDataWithId
        | GetMessages of PartitionerRunner
        | ProcessMessage of PartitionerRunner * Message


    and PartitionerRunner(p : PartitionerRunnerParam) =
        let messagingClient = MessagingClient p.messagingClientData
        let sendMessage m = messagingClient.sendMessage m
        let tryLoadModelData = p.partitionerProxy.tryLoadModelData
        let logger = p.logger


        let onStart s q =
            printfn "PartitionerRunner.onStart"
            { s with partitionerCallBackInfo = q }


        let onRegister s (r : WorkerNodeInfo) =
            printfn "PartitionerRunner.onRegister: r = %A." r
            p.partitionerProxy.saveWorkerNodeInfo r
            let updated q = { s with workerNodes = s.workerNodes.Add (r.workerNodeId, { workerNodeInfo = r; running = q }) }

            match s.workerNodes.TryFind r.workerNodeId with
            | Some n -> n.running
            | None -> []
            |> updated


        let tryFindRunningNode s r =
            s.workerNodes
            |> Map.toList
            |> List.tryPick (fun (a, b) -> if List.contains r b.running then Some a else None)


        let onUpdateProgress s (i : RemoteProgressUpdateInfo) =
            printfn "PartitionerRunner.onUpdateProgress: i = %A." i
            s.partitionerCallBackInfo.onUpdateProgress i.progressUpdateInfo

            match i.progress with
            | NotStarted -> s
            | InProgress _ -> s
            | Completed ->
                match tryFindRunningNode s i.updatedRemoteProcessId with
                | Some x ->
                    match s.workerNodes.TryFind x with
                    | Some w ->
                        { s with workerNodes = s.workerNodes.Add (x, { w with running = w.running |> List.filter (fun e -> e <> i.updatedRemoteProcessId) }) }
                    | None -> s
                | None -> s


        let onSaveResult s r =
            printfn "PartitionerRunner.onSaveResult: r = %A." r
            p.partitionerProxy.saveResultData r
            s


        /// TODO kk:20190820 - Implement.
        let onSaveCharts s c =
            printfn "PartitionerRunner.onSaveCharts: c = %A." c
            s


        let onGetMessages s (w : PartitionerRunner) =
            printfn "PartitionerRunner.onGetMessages"
            let messages = messagingClient.getMessages()

            //messages
            //|> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            //|> List.map (fun e -> p.msgClientProxy.saveMessage { messageType = IncomingMessage; message = e })
            //|> ignore

            messages
            |> List.map (fun e -> w.processMessage e)
            |> ignore

            s


        let onProcessMessage (s : PartitionerRunnerState) (w : PartitionerRunner) (m : Message) =
            printfn "PartitionerRunner.onProcessMessage: m = %A." m
            match m.messageInfo.messageData with
            | PartitionerMsg x ->
                match x with
                | UpdateProgressPrtMsg i ->
                    w.updateProgress i
                    s
                | SaveResultPrtMsg r -> onSaveResult s r
                | SaveChartsPrtMsg c -> onSaveCharts s c
                | RegisterWorkerNodePrtMsg w -> onRegister s w

            | _ ->
                p.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)
                s

            //match m.messageInfo.deliveryType with
            //| GuaranteedDelivery -> p.msgClientProxy.deleteMessage m.messageId
            //| NonGuaranteedDelivery -> ignore()

            //s

        let tryGetNode s =
            s.workerNodes
            |> Map.toList
            |> List.map (fun (_, v) -> v)
            |> List.sortBy (fun e -> (e.workerNodeInfo.nodePriority, (decimal e.running.Length) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))))
            |> List.tryPick (fun e -> if e.running.Length < e.workerNodeInfo.noOfCores then Some e else None)


        let saveQueueElement q =
            printfn "saveQueueElement is not implemented yet."
            ignore()


        let onRunModel s (a: RunModelParam) (q : RemoteProcessId) =
            printfn "PartitionerRunner.onRunModel: q = %A." q
            let onCannotRun() =
                let e =
                    {
                        remoteProcessId = q
                        runModelParam = a
                    }

                saveQueueElement e
                { s with partitionerQueue = e :: s.partitionerQueue }


            match tryGetNode s with
            | Some n ->
                match tryLoadModelData a.commandLineParam.serviceAccessInfo a.callBackInfo.modelDataId with
                | Some m ->
                    {
                        workerNodeRecipient = n.workerNodeInfo.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            {
                                wrkModelData = m
                                taskParam = a.commandLineParam.taskParam
                                runQueueId = a.callBackInfo.runQueueId
                                minUsefulEe = a.commandLineParam.serviceAccessInfo.minUsefulEe
                                remoteProcessId = q
                            }
                            |> RunModelWrkMsg
                    }.messageInfo
                    |> sendMessage

                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, { n with running = q :: n.running }) }
                | None ->
                    logger.logErr (sprintf "Unable to load model with id: %A" a.callBackInfo.modelDataId)
                    onCannotRun()
            | None -> onCannotRun()


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start q -> return! onStart s q |> loop
                            | Register r -> return! onRegister s r |> loop
                            | UpdateProgress i -> return! onUpdateProgress s i |> loop
                            | RunModel (p, q) -> return! onRunModel s p q |> loop
                            | SaveResult r -> return! onSaveResult s r |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | ProcessMessage (w, m) -> return! onProcessMessage s w m |> loop
                        }

                onStart PartitionerRunnerState.defaultValue PartitionerCallBackInfo.defaultValue |> loop
                )


        let runModelImpl p =
            let q = Guid.NewGuid() |> RemoteProcessId
            (p, q) |> RunModel |> messageLoop.Post

            {
                processId = q |> RemoteProcess
                processToStartInfo =
                    {
                        modelDataId = p.callBackInfo.modelDataId
                        runQueueId = p.callBackInfo.runQueueId
                    }
            }


        member __.start q = q |> Start |> messageLoop.Post
        member __.runModel p = runModelImpl p
        member private __.updateProgress i = UpdateProgress i |> messageLoop.Post
        member private this.processMessage m = ProcessMessage (this, m) |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post


    let createServiceImpl i =
        printfn "createServiceImpl: Creating PartitionerRunner..."
        let w = PartitionerRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w

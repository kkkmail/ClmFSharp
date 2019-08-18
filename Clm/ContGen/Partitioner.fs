namespace ContGen

open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CommandLine
open Clm.CalculationData
open ClmSys.Logging
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner
open DbData.Configuration
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open ClmSys.WorkerNodeData
open ClmSys.PartitionerData
open ServiceProxy.PartitionerProxy
open NoSql.FileSystemTypes

module Partitioner =

    type PartitionerQueueElement =
        {
            remoteProcessId : RemoteProcessId
            runModelParam : RunModelParam
        }


    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            running : list<RemoteProcessId>
        }


    type PartitionerCallBackInfo =
        {
            onUpdateProgress : ProgressUpdateInfo -> unit
        }

        static member defaultValue =
            {
                onUpdateProgress = fun _ -> ignore()
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
            callBackInfo : PartitionerCallBackInfo
            workerNodes : Map<WorkerNodeId, WorkerNodeState>
            partitionerQueue : list<PartitionerQueueElement>
        }

        static member defaultValue =
            {
                callBackInfo =
                    {
                        onUpdateProgress = fun _ -> ignore()
                    }

                workerNodes = Map.empty
                partitionerQueue = []
            }


    type PartitionerMessage =
        | Start of PartitionerCallBackInfo
        //| Register of int
        | UpdateProgress of RemoteProgressUpdateInfo
        | RunModel of RunModelParam * RemoteProcessId
        //| SaveModelData of ModelData
        //| SaveCharts of ChartInfo
        //| GetMessages of PartitionerRunner
        //| ProcessMessage of Message


    and PartitionerRunner(p : PartitionerRunnerParam) =
        let messagingClient = MessagingClient p.messagingClientData
        let sendMessage m = messagingClient.sendMessage m
        let tryLoadModelData = p.partitionerProxy.tryLoadModelData
        let logger = p.logger


        let onStart s q =
            { s with callBackInfo = q }


        //let onRegister s =
        //    {
        //        partitionerRecipient = partitioner
        //        deliveryType = GuaranteedDelivery
        //        messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
        //    }.messageInfo
        //    |> sendMessage

        //    s

        let tryFindRunningNode s r =
            s.workerNodes
            |> Map.toList
            |> List.tryPick (fun (a, b) -> if List.contains r b.running then Some a else None)


        let onUpdateProgress s (i : RemoteProgressUpdateInfo) =
            //let notify() =
            //    match s.workerNodes.TryFind

            match i.progress with
            | NotStarted -> ignore()
            | InProgress _ -> ignore()
            | Completed ->
                //match tryFindRunningNode s i.runningProcessInfo.runningProcessId
                failwith "Start new model if it exists..."

            s.callBackInfo.onUpdateProgress i.progressUpdateInfo
            s


        //let onSaveModelData s x =
        //    {
        //        partitionerRecipient = partitioner
        //        deliveryType = GuaranteedDelivery
        //        messageData = x |> SaveModelDataPrtMsg
        //    }.messageInfo
        //    |> sendMessage

        //    s


        //let onSaveCharts s c =
        //    {
        //        partitionerRecipient = partitioner
        //        deliveryType = GuaranteedDelivery
        //        messageData = c |> SaveChartsPrtMsg
        //    }.messageInfo
        //    |> sendMessage

        //    s


        //let onGetMessages s (w : WorkerNodeRunner) =
        //    let messages = messagingClient.getMessages()

        //    messages
        //    |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
        //    |> List.map (fun e -> i.msgClientProxy.saveMessage IncomingMessage e)
        //    |> ignore

        //    messages
        //    |> List.map (fun e -> w.processMessage e)
        //    |> ignore

        //    s


        //let onRunModelMsg (m : ModelData) =
        //    failwith ""


        //let onProcessMessage s (m : Message) =
        //    match m.messageInfo.messageData with
        //    | WorkerNodeMsg x ->
        //        match x with
        //        | RunModelWrkMsg m -> onRunModelMsg m
        //    | _ -> i.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)

        //    match m.messageInfo.deliveryType with
        //    | GuaranteedDelivery -> i.msgClientProxy.deleteMessage m.messageId
        //    | NonGuaranteedDelivery -> ignore()

        //    s

        let tryGetNode s =
            s.workerNodes
            |> Map.toList
            |> List.map (fun (_, v) -> v)
            |> List.sortBy (fun e -> (e.workerNodeInfo.nodePriority, (decimal e.running.Length) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))))
            |> List.tryPick (fun e -> if e.running.Length < e.workerNodeInfo.noOfCores then Some e else None)


        let saveQueueElement q =
            printfn "saveQueueElement is not implemented yet."
            ignore()


        let onRunModel s (p: RunModelParam) (q : RemoteProcessId) =
            let onCannotRun() =
                let e =
                    {
                        remoteProcessId = q
                        runModelParam = p
                    }

                saveQueueElement e
                { s with partitionerQueue = e :: s.partitionerQueue }


            match tryGetNode s with
            | Some n ->
                match tryLoadModelData p.commandLineParam.serviceAccessInfo p.callBack.calledBackModelId with
                | Some m ->
                    {
                        workerNodeRecipient = n.workerNodeInfo.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData = m |> RunModelWrkMsg
                    }.messageInfo
                    |> sendMessage

                    {
                        processId = RemoteProcess q
                        modelDataId = p.callBack.calledBackModelId
                        runQueueId = p.callBack.runQueueId
                    }
                    |> p.callBack.notifyOnStarted

                    { s with workerNodes = s.workerNodes.Add(n.workerNodeInfo.workerNodeId, { n with running = q :: n.running }) }
                | None ->
                    logger.logErr (sprintf "Unable to load model with id: %A" p.callBack.calledBackModelId)
                    onCannotRun()
            | None -> onCannotRun()


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start q -> return! onStart s q |> loop
                            //| Register -> return! onRegister s |> loop
                            | UpdateProgress i -> return! onUpdateProgress s i |> loop
                            | RunModel (p, q) -> return! onRunModel s p q |> loop
                            //| SaveModelData m -> return! onSaveModelData s m |> loop
                            //| SaveCharts c -> return! onSaveCharts s c |> loop
                            //| GetMessages w -> return! onGetMessages s w |> loop
                            //| ProcessMessage m -> return! onProcessMessage s m |> loop
                        }

                onStart PartitionerRunnerState.defaultValue PartitionerCallBackInfo.defaultValue |> loop
                )


        let runModelImpl (p: RunModelParam) : ProcessStartInfo =
            let q = Guid.NewGuid() |> RemoteProcessId
            (p, q) |> RunModel |> messageLoop.Post

            {
                processId = q |> RemoteProcess
                modelDataId = p.callBack.calledBackModelId
                runQueueId = p.callBack.runQueueId
            }


        member __.start q = q |> Start |> messageLoop.Post
        member this.runModel p = runModelImpl p

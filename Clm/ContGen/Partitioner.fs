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

module Partitioner =


    type PartitionerRunnerParam =
        {
            partitionerMsgAccessInfo : PartitionerMsgAccessInfo
            logger : Logger
            //onUpdateProgress : ProgressUpdateInfo -> unit

            //msgResponseHandler : MsgResponseHandler
            //msgClientProxy : MessagingClientProxy
        }

        //member this.messagingClientData =
        //    {
        //        msgAccessInfo = failwith ""
        //        msgResponseHandler = this.msgResponseHandler
        //        msgClientProxy = this.msgClientProxy
        //        logger = this.logger
        //    }


    type PartitionerRunnerState =
        {
            onUpdateProgress : ProgressUpdateInfo -> unit
        }

        static member defaultValue =
            {
                onUpdateProgress = fun _ -> ignore()
            }


    type PartitionerMessage =
        | Start of (ProgressUpdateInfo -> unit)
        //| Register of int
        | UpdateProgress of ProgressUpdateInfo
        | RunModel of RunModelParam * RemoteProcessId
        //| SaveModelData of ModelData
        //| SaveCharts of ChartInfo
        //| GetMessages of PartitionerRunner
        //| ProcessMessage of Message


    and PartitionerRunner(p : PartitionerRunnerParam) =
        //let messagingClient = MessagingClient p.messagingClientData
        //let partitioner = p.partitionerId
        //let sendMessage m = messagingClient.sendMessage m


        //let remoteRunnerImpl =
        //    {
        //        connectionString = clmConnectionString
        //        runModel = runModelImpl
        //    }


        let onStart s q =
            { s with onUpdateProgress = q }


        //let onRegister s =
        //    {
        //        partitionerRecipient = partitioner
        //        deliveryType = GuaranteedDelivery
        //        messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
        //    }.messageInfo
        //    |> sendMessage

        //    s


        let onUpdateProgress s (i : ProgressUpdateInfo) =
            match i.progress with
            | NotStarted -> ignore()
            | InProgress _ -> ignore()
            | Completed -> failwith "Start new model if it exists..."

            s.onUpdateProgress i
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

        let onRunModel s (p: RunModelParam) (q : RemoteProcessId) =
            s


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

                onStart (PartitionerRunnerState.defaultValue) (fun _ -> ignore()) |> loop
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

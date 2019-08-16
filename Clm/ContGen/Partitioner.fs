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

    let runModelImpl (p : RunModelParam) : ProcessStartInfo =
        failwith ""


    type PartitionerRunnerParam =
        {
            partitionerId : PartitionerId
            msgSvcAccessInfo : ServiceAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            logger : Logger

            onUpdateProgress : ProgressUpdateInfo -> unit
        }

        member this.messagingClientData =
            {
                msgAccessInfo = failwith ""
                msgResponseHandler = this.msgResponseHandler
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type PartitionerRunnerState =
        {
            dummy : int
        }

        static member defaultValue =
            {
                dummy = 0
            }


    type PartitionerMessage =
        | Start
        //| Register of int
        | UpdateProgress of ProgressUpdateInfo
        //| SaveModelData of ModelData
        //| SaveCharts of ChartInfo
        //| GetMessages of PartitionerRunner
        //| ProcessMessage of Message


    and PartitionerRunner(p : PartitionerRunnerParam) =
        let messagingClient = MessagingClient p.messagingClientData
        let partitioner = p.partitionerId
        let sendMessage m = messagingClient.sendMessage m


        let remoteRunnerImpl =
            {
                connectionString = clmConnectionString
                runModel = runModelImpl
            }


        let onStart s =
            s


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

            p.onUpdateProgress i
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


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            //| Register -> return! onRegister s |> loop
                            | UpdateProgress i -> return! onUpdateProgress s i |> loop
                            //| SaveModelData m -> return! onSaveModelData s m |> loop
                            //| SaveCharts c -> return! onSaveCharts s c |> loop
                            //| GetMessages w -> return! onGetMessages s w |> loop
                            //| ProcessMessage m -> return! onProcessMessage s m |> loop
                        }

                onStart (PartitionerRunnerState.defaultValue) |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member this.remoteRunner = remoteRunnerImpl

﻿namespace MessagingServiceInfo

open System
open System.Runtime.Remoting.Channels.Tcp
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams

module ServiceInfo =

    let MessagingServiceName = "MessagingService" + " - " + versionNumberValue.value
    let MessagingProgramName = "MessagingService.exe"


    type MessagingWorkState =
        | MsgSvcNotStarted
        | CanTransmitMessages
        | ShuttingDown


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type MessageSize =
        | SmallSize
        | MediumSize
        | LargeSize


    type PartitionerMessage =
        | UpdateProgressPrtMsg of RemoteProgressUpdateInfo
        | SaveResultPrtMsg of ResultDataWithId
        | SaveChartsPrtMsg of ChartInfo
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo
        | UnregisterWorkerNodePrtMsg of WorkerNodeId

        member this.messageSize =
            match this with
            | UpdateProgressPrtMsg _ -> SmallSize
            | SaveResultPrtMsg _ -> SmallSize
            | SaveChartsPrtMsg _ -> MediumSize
            | RegisterWorkerNodePrtMsg _ -> SmallSize
            | UnregisterWorkerNodePrtMsg _ -> SmallSize


    type WorkerNodeRunModelData =
        {
            remoteProcessId : RemoteProcessId
            localProcessId : LocalProcessId option
            runningProcessData : RunningProcessData
            taskParam : ModelCommandLineTaskParam
            minUsefulEe : MinUsefulEe
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData * ModelData

        member this.messageSize =
            match this with
            | RunModelWrkMsg _ -> LargeSize


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | WorkerNodeMsg of WorkerNodeMessage

        member this.messageSize =
            match this with
            | TextData s ->
                if s.Length < 1_000 then SmallSize
                else if s.Length < 1_000_000 then MediumSize
                else LargeSize
            | PartitionerMsg m -> m.messageSize
            | WorkerNodeMsg m -> m.messageSize

        member this.keepInMemory =
            match this.messageSize with
            | SmallSize -> true
            | MediumSize -> false
            | LargeSize -> false


    type MessageRecipientInfo =
        {
            recipient : MessagingClientId
            deliveryType : MessageDeliveryType
        }


    type MessageInfo =
        {
            recipientInfo : MessageRecipientInfo
            messageData : MessageData
        }


    type PartitionerMessageInfo =
        {
            partitionerRecipient : PartitionerId
            deliveryType : MessageDeliveryType
            messageData : PartitionerMessage
        }

        member this.messageInfo =
            {
                recipientInfo =
                    {
                        recipient = this.partitionerRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> PartitionerMsg
            }


    type WorkerNodeMessageInfo =
        {
            workerNodeRecipient : WorkerNodeId
            deliveryType : MessageDeliveryType
            messageData : WorkerNodeMessage
        }

        member this.messageInfo =
            {
                recipientInfo =
                    {
                        recipient = this.workerNodeRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> WorkerNodeMsg
            }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    /// TODO kk:20190930 - The name is not good.
    type MessageDataInfo =
        {
            messageId : MessageId
            dataVersion : MessagingDataVersion
            sender : MessagingClientId
            recipientInfo : MessageRecipientInfo
            createdOn : DateTime
        }

        member this.isExpired(waitTime : TimeSpan) =
            match this.recipientInfo.deliveryType with
            | GuaranteedDelivery -> false
            | NonGuaranteedDelivery -> if this.createdOn.Add waitTime < DateTime.Now then true else false


    type Message =
        {
            messageDataInfo : MessageDataInfo
            messageData : MessageData
        }

        member this.isExpired = this.messageDataInfo.isExpired


    type MessageWithOptionalData =
        {
            messageDataInfo : MessageDataInfo
            messageDataOpt : MessageData option
        }

        member this.isExpired = this.messageDataInfo.isExpired

        member this.toMessasge() =
            match this.messageDataOpt with
            | Some m ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageData = m
                }
                |> Some
            | None -> None


    type Message
        with
        member this.toMessageWithOptionalData() =
            match this.messageData.keepInMemory with
            | true ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageDataOpt = Some this.messageData
                }
            | false ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageDataOpt = None
                }


    type MessageResult =
        | NoMessage
        | SmallMessage of Message
        | MediumMessage of Message
        | LargeMessage of Message


    type MessageWithType =
        {
            message : Message
            messageType : MessageType
        }


    type MessagingConfigParam =
        | MsgWorkState of MessagingWorkState


    type MessagingClientConfigParam =
        | DummyConfig


    type MessageDeliveryResult =
        | DeliveredSuccessfully
        | DataVersionMismatch of MessagingDataVersion
        | ExceptionOccurred of exn
        | ServerIsShuttingDown


    type MsgServiceState =
        {
            msgVersion : MessagingDataVersion
            msgWorkState : MessagingWorkState
            msgInfo : list<(MessagingClientId * list<MessageId>)>
        }


    type MsgSvcShutDownInfo =
        {
            msgSvcTcpChannel : TcpChannel
        }


    type IMessagingService =
        abstract getVersion : unit -> MessagingDataVersion
        abstract sendMessage : Message -> MessageDeliveryResult
        abstract configureService : MessagingConfigParam -> unit
        abstract tryPeekMessage : MessagingClientId -> Message option
        abstract tryDeleteFromServer : MessagingClientId -> MessageId -> bool
        abstract getState : unit -> MsgServiceState

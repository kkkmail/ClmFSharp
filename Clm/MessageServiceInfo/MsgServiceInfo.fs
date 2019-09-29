namespace MessagingServiceInfo

open System
open System.Runtime.Remoting.Channels
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
        | CanTransmitMessages
        | ShuttingDown


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type PartitionerMessage =
        | UpdateProgressPrtMsg of RemoteProgressUpdateInfo
        | SaveResultPrtMsg of ResultDataWithId
        | SaveChartsPrtMsg of ChartInfo
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo
        | UnregisterWorkerNodePrtMsg of WorkerNodeId

        /// Currently all these messages are considred as small.
        member this.isLarge = false


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

        member this.isLarge =
            match this with
            | RunModelWrkMsg _ -> true


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | WorkerNodeMsg of WorkerNodeMessage


    type MessageInfo =
        {
            recipient : MessagingClientId
            deliveryType : MessageDeliveryType
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
                recipient = this.partitionerRecipient.messagingClientId
                deliveryType = this.deliveryType
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
                recipient = this.workerNodeRecipient.messagingClientId
                deliveryType = this.deliveryType
                messageData = this.messageData |> WorkerNodeMsg
            }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    type Message =
        {
            messageId : MessageId
            dataVersion : MessagingDataVersion
            sender : MessagingClientId
            messageInfo : MessageInfo
            createdOn : DateTime
        }


    type MessageResult =
        | NoMessage
        | SimpleMessage of Message
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

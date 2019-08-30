namespace MessagingServiceInfo

open System
open ClmSys.VersionInfo
open ClmSys.Rop
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams

module ServiceInfo =

    [<Literal>]
    let MessagingServiceName = "MessagingService"

    [<Literal>]
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


    type WorkerNodeRunModelData =
        {
            remoteProcessId : RemoteProcessId
            modelDataId : ModelDataId
            resultDataId : ResultDataId
            taskParam : ModelCommandLineTaskParam
            runQueueId : RunQueueId
            minUsefulEe : MinUsefulEe
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData * ModelData


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
            sender : MessagingClientId
            messageInfo : MessageInfo
            createdOn : DateTime
        }


    type MessageWithType =
        {
            message : Message
            messageType : MessageType
        }


    type MessagingConfigParam =
        | MsgWorkState of MessagingWorkState


    type MessagingClientConfigParam =
        | DummyConfig


    type MessageDeliveryResult = Result<unit, string>


    type MsgServiceState =
        {
            msgVersion : MessagingDataVersion
            msgWorkState : MessagingWorkState
            msgInfo : list<(MessagingClientId * list<MessageId>)>
        }


    type IMessagingService =
        abstract getVersion : unit -> MessagingDataVersion
        abstract sendMessage : Message -> MessageDeliveryResult
        //abstract getMessages : MessagingClientId -> List<Message>
        abstract configureService : MessagingConfigParam -> unit
        abstract tryPeekMessage : MessagingClientId -> Message option
        abstract tryDeleteFromServer : MessagingClientId -> MessageId -> bool
        abstract getState : unit -> MsgServiceState

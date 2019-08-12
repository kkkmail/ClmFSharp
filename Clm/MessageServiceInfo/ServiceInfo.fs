namespace MessagingServiceInfo

open System
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData

module ServiceInfo =

    [<Literal>]
    let MessagingServiceName = "MessagingService"

    [<Literal>]
    let MessagingProgramName = "MessagingService.exe"


    let getServiceUrl (i : ServiceAccessInfo) =
        getServiceUrlImpl i.serviceAddress.value i.servicePort.value MessagingServiceName


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type PartitionerMessage =
        | RunCompletedPrtMsg
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo


    type StorageMessage =
        | UpdateProgressStrMsg of ProgressUpdateInfo
        | RunCompletedStrMsg
        | SaveModelDataStrMsg of ModelData
        | SaveChartsStrMsg of ChartInfo


    type WorkerNodeMessage =
        | RunModelWrkMsg of ModelData


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | StorageMsg of StorageMessage
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


    type StorageMessageInfo =
        {
            storageRecipient : StorageId
            deliveryType : MessageDeliveryType
            messageData : StorageMessage
        }

        member this.messageInfo =
            {
                recipient = this.storageRecipient.messagingClientId
                deliveryType = this.deliveryType
                messageData = this.messageData |> StorageMsg
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


    type MessagingConfigParam =
        | DummyConfig


    type MessagingClientConfigParam =
        | DummyConfig


    type IMessagingService =
        abstract sendMessage : Message -> unit
        abstract getMessages : MessagingClientId -> List<Message>
        abstract configureService : MessagingConfigParam -> unit

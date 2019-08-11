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


    type WorkerNodeMessage =
        | RegisterWorkerNodeMsg of WorkerNodeInfo
        | UpdateProgressMsg of ProgressUpdateInfo
        | SaveModelDataMsg of ModelData
        | SaveChartsMsg of ChartInfo


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | WorkerNodeMsg of WorkerNodeMessage


    type MessageInfo =
        {
            recipient : MessagingClientId
            deliveryType : MessageDeliveryType
            messageData : MessageData
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

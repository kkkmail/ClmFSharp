namespace MessagingServiceInfo

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

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


    type MessageData =
        | TextData of string
        | BinaryData of byte[]


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

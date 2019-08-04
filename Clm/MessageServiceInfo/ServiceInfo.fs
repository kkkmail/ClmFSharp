namespace MessagingServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open ClmSys.MessagingData
open System.Threading

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


    type MessageInfo<'T> =
        {
            recipient : MessagingClientId
            deliveryType : MessageDeliveryType
            messageData : 'T
        }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    type Message<'T> =
        {
            messageId : MessageId
            sender : MessagingClientId
            messageInfo : MessageInfo<'T>
            createdOn : DateTime
        }


    type MessagingConfigParam =
        | DummyConfig


    type MessagingClientConfigParam =
        | DummyConfig


    type IMessagingService<'T> =
        abstract sendMessage : Message<'T> -> unit
        abstract getMessages : MessagingClientId -> List<Message<'T>>
        abstract configureService : MessagingConfigParam -> unit


    type ClmMesage =
        | Dummmy


    type ClmMessageInfo = MessageInfo<ClmMesage>


    type IClmMessagingService = IMessagingService<ClmMesage>

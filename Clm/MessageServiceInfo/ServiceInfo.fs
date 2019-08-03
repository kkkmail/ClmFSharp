namespace MessagingServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open System.Threading

module ServiceInfo =

    [<Literal>]
    let MessagingServiceName = "MessagingServer"

    [<Literal>]
    let MessagingProgramName = "MessagingServer.exe"


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
            recipient : NodeId
            deliveryType : MessageDeliveryType
            messageData : 'T
        }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    type Message<'T> =
        {
            messageId : MessageId
            messageInfo : MessageInfo<'T>
            createdOn : DateTime
        }


    type MesssagingConfigParam =
        | DummyConfig


    type IMessagingServer<'T> =
        abstract sendMessage : Message<'T> -> unit
        abstract getMessages : NodeId -> List<Message<'T>>
        abstract configureService : MesssagingConfigParam -> unit



    type ClmMesage =
        | Dummmy


    type ClmMessageInfo = MessageInfo<ClmMesage>


    type IClmMessagingServer = IMessagingServer<ClmMesage>

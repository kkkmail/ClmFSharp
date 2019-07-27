namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open MessagingServiceInfo.ServiceInfo

module MessagingClient =

    type MessageClientProxy<'T> =
        {
            loadMessages : unit -> List<MessageType * Message<'T>>
            saveMessage : MessageType -> Message<'T> -> unit
            deleteMessage : MessageId -> unit
            sendMessages : List<Message<'T>> -> List<Message<'T>>
            receiveMessages : unit -> List<Message<'T>>
        }

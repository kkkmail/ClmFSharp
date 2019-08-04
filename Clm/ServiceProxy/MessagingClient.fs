namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open MessagingServiceInfo.ServiceInfo

module MessagingClient =

    type MessagingClientProxy<'T> =
        {
            loadMessages : unit -> List<MessageType * Message<'T>>
            saveMessage : MessageType -> Message<'T> -> unit
            deleteMessage : MessageId -> unit
            //sendMessages : List<Message<'T>> -> List<Message<'T>>
            //receiveMessages : unit -> List<Message<'T>>
        }

        /// Default value does nothing.
        static member defaultValue =
            {
                loadMessages = fun () -> []
                saveMessage = fun _ _ -> ()
                deleteMessage = fun _ -> ()
                //sendMessages = fun _ -> []
                //receiveMessages = fun () -> []
            }

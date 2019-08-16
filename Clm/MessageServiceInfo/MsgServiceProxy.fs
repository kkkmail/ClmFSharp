namespace MessagingServiceInfo

open MessagingServiceInfo.ServiceInfo

module ServiceProxy =

    /// Provides IO proxy for messaging client.
    /// A messaging client may or may NOT have SQL server at its disposal.
    /// This proxy encapsulates that.
    type MessagingClientProxy =
        {
            loadMessages : unit -> List<MessageType * Message>
            saveMessage : MessageType -> Message -> unit
            deleteMessage : MessageId -> unit
        }

        /// Default value does nothing.
        static member defaultValue : MessagingClientProxy =
            {
                loadMessages = fun () -> []
                saveMessage = fun _ _ -> ()
                deleteMessage = fun _ -> ()
            }


    type MessagingServiceProxy =
        {
            loadMessages : unit -> List<Message>
            saveMessage : Message -> unit
            deleteMessage : MessageId -> unit
        }

        /// Default value does nothing.
        static member defaultValue =
            {
                loadMessages = fun () -> []
                saveMessage = fun _ -> ()
                deleteMessage = fun _ -> ()
            }

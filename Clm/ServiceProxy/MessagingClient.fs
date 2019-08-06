namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open MessagingServiceInfo.ServiceInfo

module MessagingClient =

    type MessagingClientProxy =
        {
            loadMessages : unit -> List<MessageType * Message>
            saveMessage : MessageType -> Message -> unit
            deleteMessage : MessageId -> unit
            //onMessageReceived : Message<'T> -> unit

            //sendMessages : List<Message<'T>> -> List<Message<'T>>
            //receiveMessages : unit -> List<Message<'T>>
        }

        /// Default value does nothing.
        static member defaultValue : MessagingClientProxy =
            {
                loadMessages = fun () -> []
                saveMessage = fun _ _ -> ()
                deleteMessage = fun _ -> ()

                //onMessageReceived =
                //    fun m -> printfn "Message received: %A" m
                //    //fun _ -> ()

                //sendMessages = fun _ -> []
                //receiveMessages = fun () -> []
            }


    //type ClmMessagingClientProxy = MessagingClientProxy<ClmMesage>

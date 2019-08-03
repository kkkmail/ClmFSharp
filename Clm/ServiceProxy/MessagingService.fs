namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open MessagingServiceInfo.ServiceInfo

module MessagingService =

    type MessagingServiceProxy<'T> =
        {
            loadMessages : unit -> List<Message<'T>>
            saveMessage : Message<'T> -> unit
            deleteMessage : MessageId -> unit
        }

        /// Default value does nothing.
        static member defaultValue =
            {
                loadMessages = fun () -> []
                saveMessage = fun _ -> ()
                deleteMessage = fun _ -> ()
            }


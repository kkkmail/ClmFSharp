﻿namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open MessagingServiceInfo.ServiceInfo

module MessagingService =

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


    //type ClmMessagingServiceProxy = MessagingServiceProxy<ClmMesage>

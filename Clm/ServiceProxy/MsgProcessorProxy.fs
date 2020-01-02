namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.Logging

module MsgProcessorProxy =

    type MessageProcessorProxy =
        {
            logger : Logger

            sendMessage : MessageInfo -> unit
            tryPeekReceivedMessage : unit -> Message option
            tryRemoveReceivedMessage : MessageId -> bool
        }



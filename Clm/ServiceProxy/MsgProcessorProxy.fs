namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralErrors

module MsgProcessorProxy =

    type MessageProcessorProxy =
        {
            sendMessage : MessageInfo -> Result<unit, ClmError>
            tryPeekReceivedMessage : unit -> Result<Message option, ClmError>
            tryRemoveReceivedMessage : MessageId -> Result<unit, ClmError>
        }

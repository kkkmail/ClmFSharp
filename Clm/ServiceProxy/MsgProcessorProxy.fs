namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralErrors

module MsgProcessorProxy =

    type MessageProcessorProxy =
        {
            sendMessage : MessageInfo -> UnitResult
            tryPeekReceivedMessage : unit -> ClmResult<Message option>
            tryRemoveReceivedMessage : MessageId -> UnitResult
        }

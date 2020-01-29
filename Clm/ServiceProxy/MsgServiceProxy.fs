﻿namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ClmSys.MessagingData
open ClmSys.Registry
open ClmSys.GeneralErrors
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors

module MsgServiceProxy =

    type MessagingClientProxyInfo =
        {
            messagingClientName : MessagingClientName
        }


    /// Provides IO proxy for messaging client.
    /// A messaging client may or may NOT have SQL server at its disposal.
    /// This proxy encapsulates that.
    type MessagingClientProxy =
        {
            loadMessages : unit -> ListResult<MessageWithType>
            saveMessage : MessageWithType -> UnitResult
            deleteMessage : MessageId -> UnitResult
        }

        static member create (i : MessagingClientProxyInfo) =
            let name = i.messagingClientName

            {
                loadMessages = tryLoadMessageWithTypeAllFs name
                saveMessage = trySaveMessageWithTypeFs name
                deleteMessage = tryDeleteMessageWithTypeFs name
            }


    /// Provides IO proxy for messaging service.
    type MessagingServiceProxy =
        {
            loadMessages : unit -> ListResult<Message>
            saveMessage : Message -> UnitResult
            deleteMessage : MessageId -> UnitResult
            tryLoadMessage : MessageId ->  Result<Message, ClmError>
        }

        static member create () =
            let name = messagingServiceName

            {
                loadMessages = tryLoadMessageAllFs name
                saveMessage = trySaveMessageFs name
                deleteMessage = tryDeleteMessageFs name
                tryLoadMessage = tryLoadMessageFs name
            }

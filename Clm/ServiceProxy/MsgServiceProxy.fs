namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
open DbData.MsgSvcDatabaseTypes
open System
open ClmSys.GeneralPrimitives

module MsgServiceProxy =

    type MessagingClientStorageType =
        | LocalFolder
        | Database of ConnectionString


    type MessagingClientProxyInfo =
        {
            messagingClientName : MessagingClientName
            storageType : MessagingClientStorageType
        }


    /// Provides IO proxy for messaging client.
    /// Currently it is assumed that messaging client does NOT have SQL server at its disposal.
    /// This proxy encapsulates that.
    type MessagingClientProxy =
        {
//            loadMessages : unit -> ListResult<MessageWithType>
            saveMessage : MessageWithType -> UnitResult
            tryDeleteMessage : MessageId -> UnitResult
        }

        static member create (i : MessagingClientProxyInfo) (c : MessagingClientId) =
            let name = i.messagingClientName

            match i.storageType with
            | LocalFolder ->
                {
//                    loadMessages = loadMessageWithTypeAllFs name
                    saveMessage = saveMessageWithTypeFs name
                    tryDeleteMessage = tryDeleteMessageWithTypeFs name
                }
            | Database connectionString ->

                {
//                    loadMessages = loadMessageWithTypeAllFs name
                    saveMessage = fun m -> saveMessage connectionString m.message
                    tryDeleteMessage = deleteMessage connectionString
                }


    /// Provides IO proxy for messaging service.
    type MessagingServiceProxy =
        {
            tryPickMessage : MessagingClientId -> ClmResult<Message option>
            saveMessage : Message -> UnitResult
            deleteMessage : MessageId -> UnitResult
            deleteExpiredMessages : TimeSpan -> UnitResult
        }

        static member create (connectionString : ConnectionString) =
            {
                tryPickMessage = tryPickMessage connectionString
                saveMessage = saveMessage connectionString
                deleteMessage = deleteMessage connectionString
                deleteExpiredMessages = deleteExpiredMessages connectionString
            }

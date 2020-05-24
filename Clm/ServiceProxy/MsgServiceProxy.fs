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
        | MsSqlDatabase of ConnectionString
        | SqliteDatabase of SqliteConnectionString


    type MessagingClientProxyInfo =
        {
            messagingClientName : MessagingClientName
            storageType : MessagingClientStorageType
        }


    /// Provides IO proxy for messaging client.
    /// Currently it is assumed that messaging client may NOT have SQL server at its disposal.
    type MessagingClientProxy =
        {
            tryPickIncomingMessage : unit -> ClmResult<Message option>
            tryPickOutgoingMessage : unit -> ClmResult<Message option>
            saveMessage : Message -> UnitResult
            tryDeleteMessage : MessageId -> UnitResult
            deleteExpiredMessages : TimeSpan -> UnitResult
        }

        static member create (i : MessagingClientProxyInfo) (c : MessagingClientId) =
            let name = i.messagingClientName

            match i.storageType with
//            | LocalFolder ->
//                {
//                    loadMessages = loadMessageWithTypeAllFs name
//                    saveMessage = saveMessageWithTypeFs name
//                    tryDeleteMessage = tryDeleteMessageWithTypeFs name
//                }
            | MsSqlDatabase connectionString ->

                {
                    tryPickIncomingMessage = fun () -> tryPickIncomingMessage connectionString c
                    tryPickOutgoingMessage = fun () -> tryPickOutgoingMessage connectionString c
                    saveMessage = fun m -> saveMessage connectionString m
                    tryDeleteMessage = deleteMessage connectionString
                    deleteExpiredMessages = deleteExpiredMessages connectionString
                }
            | SqliteDatabase connectionString ->
                {
                    tryPickIncomingMessage = fun () -> tryPickIncomingMessageSqlite connectionString c
                    tryPickOutgoingMessage = fun () -> tryPickOutgoingMessageSqlite connectionString c
                    saveMessage = fun m -> saveMessageWithTypeSqlite connectionString m
                    tryDeleteMessage = deleteMessageSqlite connectionString
                    deleteExpiredMessages = deleteExpiredMessagesSqlite connectionString
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
                tryPickMessage = tryPickIncomingMessage connectionString
                saveMessage = saveMessage connectionString
                deleteMessage = deleteMessage connectionString
                deleteExpiredMessages = deleteExpiredMessages connectionString
            }

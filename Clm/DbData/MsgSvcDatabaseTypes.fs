namespace DbData

open System.Data.SqlClient
open FSharp.Data
open System
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.MessagingServiceErrors
open MessagingServiceInfo.ServiceInfo
open FSharp.Data.Sql
open System.Data.SQLite
open Dapper
open System.Data.Common
open Clm.ModelParams
open ClmSys.GeneralErrors

// ! Must be the last to open !
open Configuration

module MsgSvcDatabaseTypes =

    [<Literal>]
    let private SqliteStorageFolder = DefaultRootFolder


    [<Literal>]
    let MsgDatabase = "MsgClient.db"


    [<Literal>]
    let MsgSqliteConnStr =
        "Data Source=" + SqliteStorageFolder + MsgDatabase + @";Version=3;foreign keys=true"


    let msgSqliteConnStr = MsgSqliteConnStr |> SqliteConnectionString


    type Guid
        with
        member g.ToSqliteString() = g.ToString("N")

    type sqLite = SqlDataProvider<
                   Common.DatabaseProviderTypes.SQLITE,
                   SQLiteLibrary = Common.SQLiteLibrary.SystemDataSQLite,
                   ConnectionString = MsgSqliteConnStr,
                   //ResolutionPath = resolutionPath,
                   CaseSensitivityChange = Common.CaseSensitivityChange.ORIGINAL>


    let serializationFormat = BinaryZippedFormat


    type MsgSvcDB = SqlProgrammabilityProvider<MsgSvcSqlProviderName, ConfigFile = AppConfigFile>
    type MessageTable = MsgSvcDB.dbo.Tables.Message
    type MessageTableRow = MessageTable.Row


    type MessageData = SqlCommandProvider<"
        select *
        from dbo.Message
        where messageId = @messageId", MsgSvcConnectionStringValue, ResultType.DataReader>


    type TryPickRecipientMessageData = SqlCommandProvider<"
           select top 1 *
           from dbo.Message
           where recipientId = @recipientId and dataVersion = @dataVersion
           order by createdOn, messageOrder
           ", MsgSvcConnectionStringValue, ResultType.DataReader>


    type TryPickSenderMessageData = SqlCommandProvider<"
           select top 1 *
           from dbo.Message
           where senderId = @senderId and dataVersion = @dataVersion
           order by createdOn, messageOrder
           ", MsgSvcConnectionStringValue, ResultType.DataReader>


    type Message
        with

        static member tryCreate (r : MessageTableRow) =
            let toError e = e |> MessageCreateErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

            let g() =
                match MessageDeliveryType.tryCreate r.deliveryTypeId, messagingDataVersion.value = r.dataVersion with
                | Some t, true ->
                    {
                        messageDataInfo =
                            {
                                messageId = r.messageId |> MessageId
                                dataVersion = r.dataVersion |> MessagingDataVersion
                                sender = r.senderId |> MessagingClientId

                                recipientInfo =
                                    {
                                        recipient = r.recipientId |> MessagingClientId
                                        deliveryType = t
                                    }

                                createdOn = r.createdOn
                            }

                        messageData = r.messageData |> deserialize serializationFormat
                    }
                    |> Some
                    |> Ok
                | Some _, false -> InvalidDataVersionErr { localVersion = messagingDataVersion; remoteVersion = MessagingDataVersion r.dataVersion } |> toError
                | None, true -> InvalidDeliveryTypeErr r.deliveryTypeId |> toError
                | None, false -> InvalidDeliveryTypeAndDataVersionErr (r.deliveryTypeId, { localVersion = messagingDataVersion; remoteVersion = MessagingDataVersion r.dataVersion }) |> toError

            tryDbFun g

        member r.addRow (t : MessageTable) =
            let g() =
                let newRow =
                    t.NewRow(
                            messageId = r.messageDataInfo.messageId.value,
                            dataVersion = messagingDataVersion.value,
                            deliveryTypeId = r.messageDataInfo.recipientInfo.deliveryType.value,
                            senderId = r.messageDataInfo.sender.value,
                            recipientId = r.messageDataInfo.recipientInfo.recipient.value,
                            messageData = (r.messageData |> serialize serializationFormat)
                            )

                newRow.createdOn <- r.messageDataInfo.createdOn

                t.Rows.Add newRow
                Ok newRow

            tryDbFun g


    let tryCreateMessage (t : MessageTable) =
        match t.Rows |> Seq.tryHead with
        | Some v -> v |> Message.tryCreate
        | None -> Ok None


    let tryPickMessage connectionString (MessagingClientId i) =
        let g () =
            use conn = getOpenConn connectionString
            let t = new MessageTable()
            use d = new TryPickRecipientMessageData(conn)
            d.Execute(i, messagingDataVersion.value) |> t.Load
            tryCreateMessage t

        tryDbFun g


    let tryPickOutgoingMessage connectionString (MessagingClientId i) =
        let g () =
            use conn = getOpenConn connectionString
            let t = new MessageTable()
            use d = new TryPickSenderMessageData(conn)
            d.Execute(i, messagingDataVersion.value) |> t.Load
            tryCreateMessage t

        tryDbFun g


    let tryPickIncomingMessage = tryPickMessage


    /// TODO kk:20200411 - I am not very happy about double ignore below. Refactor when time permits.
    let saveMessage connectionString (m : Message) =
        let g() =
            use conn = getOpenConn connectionString
            use t = new MessageTable()
            m.addRow t |> ignore
            t.Update conn |> ignore
            Ok()

        tryDbFun g


    let deleteMessage (ConnectionString connectionString) (messageId : MessageId) =
        let toError e = e |> MessageDeleteErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

        let g() =
            use cmd = new SqlCommandProvider<"delete from dbo.Message where messageId = @messageId", MsgSvcConnectionStringValue>(connectionString)

            match cmd.Execute(messageId = messageId.value) = 1 with
            | true -> Ok()
            | false -> messageId |> CannotDeleteMessageErr |> toError

        tryDbFun g


    let deleteExpiredMessages (ConnectionString connectionString) (expirationTime : TimeSpan) =
        let g() =
            use cmd = new SqlCommandProvider<"
                delete from dbo.Message
                where
                    deliveryTypeId = 1
                    and dataVersion = @dataVersion
                    and createdOn < @createdOn", MsgSvcConnectionStringValue>(connectionString)

            let result = cmd.Execute(messagingDataVersion.value, DateTime.Now - expirationTime)
            Ok()

        tryDbFun g


    let private executeSqlite (connection : #DbConnection) (sql : string) (parameters : _) =
        let g() =
            let result = connection.Execute(sql, parameters)
            Ok result
        tryDbFun g


    /// TODO kk:20200523 - So far this looks extremely far beyond ugly.
    /// Find the proper way and don't go beyond this one table until that proper way is found.
    ///
    /// Here are some references:
    ///     https://devonburriss.me/how-to-fsharp-pt-9/
    ///     https://isthisit.nz/posts/2019/sqlite-database-with-dapper-and-fsharp/
    ///     http://zetcode.com/csharp/sqlite/
    let saveMessageWithTypeSqlite (SqliteConnectionString connectionString) (m : MessageWithType) =
        let g() =
            use connectionString = new SQLiteConnection(connectionString)

            let sql = @"
                INSERT INTO Message
                    (messageId
                    ,senderId
                    ,recipientId
                    ,dataVersion
                    ,deliveryTypeId
                    ,messageData
                    ,createdOn)
                VALUES
                    (@messageId
                    ,@senderId
                    ,@recipientId
                    ,@dataVersion
                    ,@deliveryTypeId
                    ,@messageData
                    ,@createdOn)"

            let data =
                [
                    ("@messageId", m.message.messageDataInfo.messageId.value.ToSqliteString() |> box)
                    ("@senderId", m.message.messageDataInfo.sender.value.ToSqliteString() |> box)
                    ("@recipientId", m.message.messageDataInfo.recipientInfo.recipient.value.ToSqliteString() |> box)
                    ("@dataVersion", m.message.messageDataInfo.dataVersion.value |> box)
                    ("@deliveryTypeId", m.message.messageDataInfo.recipientInfo.deliveryType.value |> box)
                    ("@messageData", m.message.messageData |> (serialize serializationFormat) |> box)
                    ("@createdOn", m.message.messageDataInfo.createdOn |> box)
                ]
                |> dict
                |> fun d -> Dapper.DynamicParameters(d)

            let result = executeSqlite connectionString sql data
            Ok()

        tryDbFun g


    let deleteMessageSqlite (SqliteConnectionString connectionString) (messageId : MessageId) =
        let toError e = e |> MessageDeleteErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

        let g() =
            use conn = new SQLiteConnection(connectionString)
            use cmd = new SQLiteCommand("delete from dbo.Message where messageId = @messageId", conn)
            cmd.Parameters.Add(SQLiteParameter("@messageId", messageId.value.ToSqliteString())) |> ignore

            match cmd.ExecuteNonQuery() = 1 with
            | true -> Ok()
            | false -> messageId |> CannotDeleteMessageErr |> toError

        tryDbFun g


    let deleteExpiredMessagesSqlite (SqliteConnectionString connectionString) (expirationTime : TimeSpan) =
        let g() =
            use conn = new SQLiteConnection(connectionString)
            use cmd = new SQLiteCommand(@"
                delete from dbo.Message
                where
                    deliveryTypeId = 1
                    and dataVersion = @dataVersion
                    and createdOn < @createdOn", conn)

            cmd.Parameters.Add(SQLiteParameter("@dataVersion", messagingDataVersion.value)) |> ignore
            cmd.Parameters.Add(SQLiteParameter("@createdOn", DateTime.Now - expirationTime)) |> ignore

            let result = cmd.ExecuteNonQuery()
            Ok()

        tryDbFun g

    type SQLiteDataReader
        with
        member rdr.GetGuid(columnName : string) = rdr.GetString(rdr.GetOrdinal(columnName)) |> Guid.Parse
        member rdr.GetInt16(columnName : string) = rdr.GetInt16(rdr.GetOrdinal(columnName))
        member rdr.GetInt32(columnName : string) = rdr.GetInt32(rdr.GetOrdinal(columnName))
        member rdr.GetInt64(columnName : string) = rdr.GetInt64(rdr.GetOrdinal(columnName))
        member rdr.GetDateTime(columnName : string) = rdr.GetDateTime(rdr.GetOrdinal(columnName))
        member rdr.GetBoolean(columnName : string) = rdr.GetBoolean(rdr.GetOrdinal(columnName))

        member rdr.GetBlob(columnName : string) =
            let len = rdr.GetBytes(rdr.GetOrdinal(columnName), 0L, null, 0, Int32.MaxValue) |> int
            let bytes : byte array = Array.zeroCreate len
            rdr.GetBytes(rdr.GetOrdinal(columnName), 0L, bytes, 0, bytes.Length) |> ignore
            bytes


    let toMessageWithType (rdr : SQLiteDataReader) =
        {
            message =
                {
                    messageDataInfo =
                        {
                            messageId = rdr.GetGuid("messageId") |> MessageId
                            dataVersion = rdr.GetInt32("dataVersion") |> MessagingDataVersion
                            sender = rdr.GetGuid("senderId") |> MessagingClientId
                            recipientInfo =
                                {
                                    recipient = rdr.GetGuid("recipientId") |> MessagingClientId
                                    deliveryType = rdr.GetInt32("deliveryTypeId")
                                                   |> MessageDeliveryType.tryCreate
                                                   |> Option.defaultValue GuaranteedDelivery
                                }

                            createdOn = rdr.GetDateTime("createdOn")
                        }

                    messageData = rdr.GetBlob("messageData") |> (deserialize serializationFormat)
                }

            messageType = IncomingMessage
        }


    let tryPickIncomingMessageSqlite (SqliteConnectionString connectionString) (MessagingClientId i) =
        let g () =
            use conn = new SQLiteConnection(connectionString)
            use cmd = new SQLiteCommand(@"
                select top 1 *
                from dbo.Message
                where recipientId = @recipientId and dataVersion = @dataVersion
                order by messageOrder", conn)

            cmd.Parameters.Add(SQLiteParameter("@recipientId", i.ToSqliteString())) |> ignore
            use rdr = cmd.ExecuteReader()

            match rdr.Read() with
            | true -> toMessageWithType rdr |> Some
            | false -> None
            |> Ok

        tryDbFun g


    let tryPickOutgoingMessageSqlite (SqliteConnectionString connectionString) (MessagingClientId i) =
        let g () =
            use conn = new SQLiteConnection(connectionString)
            use cmd = new SQLiteCommand(@"
                select top 1 *
                from dbo.Message
                where senderId = @senderId and dataVersion = @dataVersion
                order by messageOrder", conn)

            cmd.Parameters.Add(SQLiteParameter("@senderId", i.ToSqliteString())) |> ignore
            use rdr = cmd.ExecuteReader()

            match rdr.Read() with
            | true -> toMessageWithType rdr |> Some
            | false -> None
            |> Ok

        tryDbFun g

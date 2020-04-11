namespace DbData

open System.Data
open System.Data.SqlClient
open FSharp.Data
open System
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open ClmSys.Retry
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.MessagingServiceErrors
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module MsgSvcDatabaseTypes =

    let serializationFormat = BinaryZippedFormat


    type MsgSvcDB = SqlProgrammabilityProvider<MsgSvcSqlProviderName, ConfigFile = AppConfigFile>
    type MessageTable = MsgSvcDB.dbo.Tables.Message
    type MessageTableRow = MessageTable.Row


    type MessageData = SqlCommandProvider<"
        select *
        from dbo.Message
        where messageId = @messageId", MsgSvcConnectionStringValue, ResultType.DataReader>


    type TryPickMessageData = SqlCommandProvider<"
           select top 1 *
           from dbo.Message
           where recipientId = @recipientId and dataVersion = @dataVersion
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
                            deliveryTypeId = r.messageDataInfo.dataVersion.value,
                            senderId = r.messageDataInfo.sender.value,
                            recipientId = r.messageDataInfo.recipientInfo.recipient.value,
                            messageData = (r.messageData |> serialize serializationFormat)
                            )

                newRow.createdOn <- r.messageDataInfo.createdOn

                t.Rows.Add newRow
                Ok newRow

            tryDbFun g


    let tryPickMessage connectionString (MessagingClientId i) =
        let g () =
            use conn = getOpenConn connectionString
            use d = new TryPickMessageData(conn)
            let t = new MessageTable()
            d.Execute(i, messagingDataVersion.value) |> t.Load

            match t.Rows |> Seq.tryHead with
            | Some v -> v |> Message.tryCreate
            | None -> Ok None

        tryDbFun g


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
            Ok result

        tryDbFun g

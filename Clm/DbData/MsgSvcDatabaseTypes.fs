namespace DbData

open System.Data
open System.Data.SqlClient
open Newtonsoft.Json
open FSharp.Data
open System
open ClmSys.VersionInfo
open Clm.Substances
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open DynamicSql
open ClmSys.GeneralErrors
open ClmSys.Retry
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.WorkerNodeData
open ClmSys
open ClmSys.PartitionerData
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module MsgSvcDatabaseTypes =
    let x = 1

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
           where recipientId = @recipientId
           order by createdOn, messageOrder
           ", MsgSvcConnectionStringValue, ResultType.DataReader>


    type Message
        with

        static member tryCreate (r : MessageTableRow) =
            match MessageDeliveryType.tryCreate r.deliveryTypeId, messagingDataVersion.value = 0 with
            | Some t, true ->
                {
                    messageDataInfo =
                        {
                            messageId = r.messageId |> MessageId
                            dataVersion = 0
                            sender = r.senderId |> MessagingClientId
                            recipientInfo =
                                {
                                    recipient = r.recipientId |> MessagingClientId
                                    deliveryType = t
                                }

                            createdOn = r.createdOn
                        }

                    messageData = 0
                }
                |> Some
            | _ -> None

﻿namespace MessagingServiceInfo

open System
open System.ServiceModel

open ClmSys.SolverRunnerPrimitives
open ClmSys.VersionInfo
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open Clm.ChartData

module ServiceInfo =

    let messagingProgramName = "MessagingService.exe"


    [<Literal>]
    let MessagingWcfServiceName = "MessagingWcfService"


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery

        member d.value =
            match d with
            | GuaranteedDelivery -> 0
            | NonGuaranteedDelivery -> 1

        static member tryCreate i =
            match i with
            | 0 -> Some GuaranteedDelivery
            | 1 -> Some NonGuaranteedDelivery
            | _ -> None


    type MessageSize =
        | SmallSize
        | MediumSize
        | LargeSize


    type PartitionerMessage =
        | UpdateProgressPrtMsg of ProgressUpdateInfo
        | SaveResultPrtMsg of ResultDataWithId
        | SaveChartsPrtMsg of ChartInfo
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo
        | UnregisterWorkerNodePrtMsg of WorkerNodeId

        member this.messageSize =
            match this with
            | UpdateProgressPrtMsg _ -> SmallSize
            | SaveResultPrtMsg _ -> SmallSize
            | SaveChartsPrtMsg _ -> MediumSize
            | RegisterWorkerNodePrtMsg _ -> SmallSize
            | UnregisterWorkerNodePrtMsg _ -> SmallSize


    type EarlyExitData = ChartData


    type EarlyExitRule =
        | ProgressExceeds of decimal
        | MaxWeightedAverageAbsEeExceeds of float
        | MaxLastEeExceeds of float
        | MaxAverageEeExceeds of float

        member r.isValid (d : EarlyExitData) =
            match r with
            | ProgressExceeds p -> p > d.progress
            | MaxWeightedAverageAbsEeExceeds e -> e > d.maxWeightedAverageAbsEe
            | MaxLastEeExceeds e -> e > d.maxLastEe
            | MaxAverageEeExceeds e -> e > d.maxAverageEe


    type EarlyExitCheckFrequency =
        | EarlyExitCheckFrequency of TimeSpan

        member this.value = let (EarlyExitCheckFrequency v) = this in v
        static member defaultValue = TimeSpan.FromHours(1.0) |> EarlyExitCheckFrequency


    type EarlyExitStrategy =
        | AllOfAny of list<list<EarlyExitRule>> // Outer list - all collections must be satisfied, inner list - at least one rule must be satisfied.

        member e.exitEarly d =
            match e with
            |AllOfAny v ->
                match v with
                | [] -> false // If outer list is empty, then early exit strategy cannot work.
                | _ ->
                    v
                    |> List.map (fun a -> a |> List.fold (fun acc b -> acc || (b.isValid d)) false)
                    |> List.fold (fun acc r -> acc && r) true


        static member defaultProgress = 0.05M
        static member defaultMinEe = 0.15

        static member getDefaultValue p e =
            [
                [
                    ProgressExceeds p
                ]
                [
                    MaxWeightedAverageAbsEeExceeds e
                    MaxLastEeExceeds e
                    MaxAverageEeExceeds e
                ]
            ]
            |> AllOfAny

            static member defaultValue = EarlyExitStrategy.getDefaultValue EarlyExitStrategy.defaultProgress EarlyExitStrategy.defaultMinEe


    type EarlyExitInfo =
        {
            frequency : EarlyExitCheckFrequency
            earlyExitStrategy : EarlyExitStrategy
        }

        static member getDefaultValue f p e =
            {
                frequency = f
                earlyExitStrategy = EarlyExitStrategy.getDefaultValue p e
            }

        static member defaultValue =
            {
                frequency = EarlyExitCheckFrequency.defaultValue
                earlyExitStrategy = EarlyExitStrategy.defaultValue
            }


    type WorkerNodeRunModelData =
        {
            runningProcessData : RunningProcessData
            modelData : ModelData
            minUsefulEe : MinUsefulEe
            earlyExitOpt : EarlyExitInfo option
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData
        | CancelRunWrkMsg of (RunQueueId * CancellationType)
        | RequestResultWrkMsg of (RunQueueId * ResultNotificationType)

        member this.messageSize =
            match this with
            | RunModelWrkMsg _ -> LargeSize
            | CancelRunWrkMsg _ -> SmallSize
            | RequestResultWrkMsg _ -> SmallSize


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | WorkerNodeMsg of WorkerNodeMessage

        static member maxInfoLength = 500

        member this.getMessageSize() =
            match this with
            | TextData s ->
                if s.Length < 1_000 then SmallSize
                else if s.Length < 1_000_000 then MediumSize
                else LargeSize
            | PartitionerMsg m -> m.messageSize
            | WorkerNodeMsg m -> m.messageSize

        member this.keepInMemory() =
            match this.getMessageSize() with
            | SmallSize -> true
            | MediumSize -> false
            | LargeSize -> false

        member this.getInfo() =
            let s = (sprintf "%A" this)
            s.Substring(0, min s.Length MessageData.maxInfoLength)


    type MessageRecipientInfo =
        {
            recipient : MessagingClientId
            deliveryType : MessageDeliveryType
        }


    type MessageInfo =
        {
            recipientInfo : MessageRecipientInfo
            messageData : MessageData
        }


    type PartitionerMessageInfo =
        {
            partitionerRecipient : PartitionerId
            deliveryType : MessageDeliveryType
            messageData : PartitionerMessage
        }

        member this.getMessageInfo() =
            {
                recipientInfo =
                    {
                        recipient = this.partitionerRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> PartitionerMsg
            }


    type WorkerNodeMessageInfo =
        {
            workerNodeRecipient : WorkerNodeId
            deliveryType : MessageDeliveryType
            messageData : WorkerNodeMessage
        }

        member this.getMessageInfo() =
            {
                recipientInfo =
                    {
                        recipient = this.workerNodeRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> WorkerNodeMsg
            }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    /// TODO kk:20190930 - The name is not good.
    type MessageDataInfo =
        {
            messageId : MessageId
            dataVersion : MessagingDataVersion
            sender : MessagingClientId
            recipientInfo : MessageRecipientInfo
            createdOn : DateTime
        }


    type Message =
        {
            messageDataInfo : MessageDataInfo
            messageData : MessageData
        }


    type MessageWithOptionalData =
        {
            messageDataInfo : MessageDataInfo
            messageDataOpt : MessageData option
        }


    type MessageDataInfo
        with
        member this.isExpired(waitTime : TimeSpan) =
            match this.recipientInfo.deliveryType with
            | GuaranteedDelivery -> false
            | NonGuaranteedDelivery -> if this.createdOn.Add waitTime < DateTime.Now then true else false


    type Message
        with
        member this.isExpired waitTime = this.messageDataInfo.isExpired waitTime


    type MessagingConfigParam =
        | DummyConfig


    type MsgWcfSvcShutDownInfo =
        {
            serviceHost : ServiceHost
        }


    type RunQueue
        with

        member q.toRunningProcessDataOpt() =
            q.workerNodeIdOpt
            |> Option.bind (fun w ->
                            {
                                modelDataId = q.info.modelDataId
                                defaultValueId = q.info.defaultValueId
                                runQueueId = q.runQueueId
                                workerNodeId = w
                                commandLineParams = q.modelCommandLineParam
                            }
                            |> Some)


        member q.toMessageInfoOpt getModelData minUsefulEe eeo =
            match q.toRunningProcessDataOpt() with
            | Some d ->
                match getModelData q.info.modelDataId with
                | Ok m ->
                    {
                        workerNodeRecipient = d.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            {
                                runningProcessData = d
                                minUsefulEe = minUsefulEe
                                modelData = m
                                earlyExitOpt = eeo
                            }
                            |> RunModelWrkMsg
                    }.getMessageInfo()
                    |> Some |> Ok
                | Error e -> Error e
            | None -> Ok None


    type IMessagingService =
        abstract getVersion : unit -> ClmResult<MessagingDataVersion>
        abstract sendMessage : Message -> UnitResult
        abstract tryPeekMessage : MessagingClientId -> ClmResult<Message option>
        abstract tryDeleteFromServer : (MessagingClientId * MessageId) -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = MessagingWcfServiceName)>]
    type IMessagingWcfService =

        [<OperationContract(Name = "getVersion")>]
        abstract getVersion : u:byte[] -> byte[]

        [<OperationContract(Name = "sendMessage")>]
        abstract sendMessage : m:byte[] -> byte[]

        [<OperationContract(Name = "tryPeekMessage")>]
        abstract tryPeekMessage : c:byte[] -> byte[]

        [<OperationContract(Name = "tryDeleteFromServer")>]
        abstract tryDeleteFromServer : cm:byte[] -> byte[]


    type WcfCommunicator = (IMessagingWcfService-> byte[] -> byte[])

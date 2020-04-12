namespace MessagingServiceInfo

open System
open System.ServiceModel

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

module ServiceInfo =

    let messagingProgramName = "MessagingService.exe"


    //type MessagingWorkState =
    //    | MsgSvcNotStarted
    //    | CanTransmitMessages
    //    | ShuttingDown


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


    type WorkerNodeRunModelData =
        {
            runningProcessData : RunningProcessData
            modelData : ModelData
            minUsefulEe : MinUsefulEe
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData
        | CancelRunWrkMsg of RunQueueId

        member this.messageSize =
            match this with
            | RunModelWrkMsg _ -> LargeSize
            | CancelRunWrkMsg _ -> SmallSize


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


    //type MessageWithOptionalData
    //    with
    //    member this.isExpired waitTime = this.messageDataInfo.isExpired waitTime

    //    member this.toMessasge() =
    //        match this.messageDataOpt with
    //        | Some m ->
    //            {
    //                messageDataInfo = this.messageDataInfo
    //                messageData = m
    //            }
    //            |> Some
    //        | None -> None


    type Message
        with
        member this.isExpired waitTime = this.messageDataInfo.isExpired waitTime

    //    member this.toMessageWithOptionalData() =
    //        match this.messageData.keepInMemory() with
    //        | true ->
    //            {
    //                messageDataInfo = this.messageDataInfo
    //                messageDataOpt = Some this.messageData
    //            }
    //        | false ->
    //            {
    //                messageDataInfo = this.messageDataInfo
    //                messageDataOpt = None
    //            }


    type MessageResultInfo =
        | NoMessage
        | SmallMessage of Message
        | MediumMessage of Message
        | LargeMessage of Message


    type MessageResult = ClmResult<MessageResultInfo>


    type MessageWithType =
        {
            message : Message
            messageType : MessageType
        }


    type MessagingConfigParam =
        | DummyConfig


    type MessagingClientConfigParam =
        | DummyConfig


    //type MsgServiceState =
    //    {
    //        msgVersion : MessagingDataVersion
    //        msgWorkState : MessagingWorkState
    //        msgInfo : list<(MessagingClientId * list<MessageId>)>
    //    }


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


        member q.toMessageInfoOpt getModelData minUsefulEe =
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
                            }
                            |> RunModelWrkMsg
                    }.getMessageInfo()
                    |> Some |> Ok
                | Error e -> Error e
            | None -> Ok None


    type IMessagingService =
        abstract getVersion : unit -> ClmResult<MessagingDataVersion>
        abstract sendMessage : Message -> UnitResult
        //abstract configureService : MessagingConfigParam -> UnitResult
        abstract tryPeekMessage : MessagingClientId -> ClmResult<Message option>
        abstract tryDeleteFromServer : (MessagingClientId * MessageId) -> UnitResult
        //abstract getState : unit -> ClmResult<MsgServiceState>


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = "MessagingWcfService")>]
    type IMessagingWcfService =

        [<OperationContract(Name = "getVersion")>]
        abstract getVersion : u:byte[] -> byte[]

        [<OperationContract(Name = "sendMessage")>]
        abstract sendMessage : m:byte[] -> byte[]

        //[<OperationContract(Name = "configureService")>]
        //abstract configureService : p:byte[] -> byte[]

        [<OperationContract(Name = "tryPeekMessage")>]
        abstract tryPeekMessage : c:byte[] -> byte[]

        [<OperationContract(Name = "tryDeleteFromServer")>]
        abstract tryDeleteFromServer : cm:byte[] -> byte[]

        //[<OperationContract(Name = "getState")>]
        //abstract getState : u:byte[] -> byte[]


    type WcfCommunicator = (IMessagingWcfService-> byte[] -> byte[])

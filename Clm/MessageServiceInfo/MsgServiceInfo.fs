namespace MessagingServiceInfo

open System
open System.Runtime.Remoting.Channels.Tcp
open System.ServiceModel
open System.ServiceModel.Description

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams

module ServiceInfo =

    let MessagingServiceName = "MessagingService" + " - " + versionNumberValue.value
    let MessagingProgramName = "MessagingService.exe"


    type MessagingWorkState =
        | MsgSvcNotStarted
        | CanTransmitMessages
        | ShuttingDown


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type MessageSize =
        | SmallSize
        | MediumSize
        | LargeSize


    type PartitionerMessage =
        | UpdateProgressPrtMsg of RemoteProgressUpdateInfo
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
            remoteProcessId : RemoteProcessId
            localProcessId : LocalProcessId option
            runningProcessData : RunningProcessData
            taskParam : ModelCommandLineTaskParam
            minUsefulEe : MinUsefulEe
            commandLine : string
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData * ModelData

        member this.messageSize =
            match this with
            | RunModelWrkMsg _ -> LargeSize


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type MessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | WorkerNodeMsg of WorkerNodeMessage

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

        /// TODO kk:20200119 - Implement a shorter version as some messages can be very large.
        member this.getInfo() = sprintf "%A" this


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


    type MessageWithOptionalData
        with
        member this.isExpired waitTime = this.messageDataInfo.isExpired waitTime

        member this.toMessasge() =
            match this.messageDataOpt with
            | Some m ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageData = m
                }
                |> Some
            | None -> None


    type Message
        with
        member this.isExpired waitTime = this.messageDataInfo.isExpired waitTime

        member this.toMessageWithOptionalData() =
            match this.messageData.keepInMemory() with
            | true ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageDataOpt = Some this.messageData
                }
            | false ->
                {
                    messageDataInfo = this.messageDataInfo
                    messageDataOpt = None
                }


    type MessageResultInfo =
        | NoMessage
        | SmallMessage of Message
        | MediumMessage of Message
        | LargeMessage of Message


    type MessageResult = Result<MessageResultInfo, ClmError>


    type MessageWithType =
        {
            message : Message
            messageType : MessageType
        }


    type MessagingConfigParam =
        | MsgWorkState of MessagingWorkState


    type MessagingClientConfigParam =
        | DummyConfig


    type GetVersionResult = Result<MessagingDataVersion, GetVersionError>


    type MessageDeliveryResult = Result<unit, MessageDeliveryError>


    type ServiceConfigured =
        | ServiceConfigured


    type ConfigureServiceResult = Result<ServiceConfigured, ConfigureServiceError>


    type TryPeekMessageResult = Result<Message option, TryPeekMessageError>


    type TryDeleteFromServerResult = Result<bool, TryDeleteFromServerError>


    type MsgServiceState =
        {
            msgVersion : MessagingDataVersion
            msgWorkState : MessagingWorkState
            msgInfo : list<(MessagingClientId * list<MessageId>)>
        }


    type GetStateResult = Result<MsgServiceState, GetStateError>


    type MsgSvcShutDownInfo =
        {
            msgSvcTcpChannel : TcpChannel
        }


    type MsgWcfSvcShutDownInfo =
        {
            serviceHost : ServiceHost
        }


    type IMessagingService =
        abstract getVersion : unit -> GetVersionResult
        abstract sendMessage : Message -> MessageDeliveryResult
        abstract configureService : MessagingConfigParam -> ConfigureServiceResult
        abstract tryPeekMessage : MessagingClientId -> TryPeekMessageResult
        abstract tryDeleteFromServer : (MessagingClientId * MessageId) -> TryDeleteFromServerResult
        abstract getState : unit -> GetStateResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = "MessagingWcfService")>]
    type IMessagingWcfService =

        [<OperationContract(Name = "getVersion")>]
        //abstract getVersion : u:unit -> MessagingDataVersion
        abstract getVersion : u:byte[] -> byte[]

        [<OperationContract(Name = "sendMessage")>]
        //abstract sendMessage : m:Message -> MessageDeliveryResult
        abstract sendMessage : m:byte[] -> byte[]

        [<OperationContract(Name = "configureService")>]
        //abstract configureService : p:MessagingConfigParam -> unit
        abstract configureService : p:byte[] -> byte[]

        [<OperationContract(Name = "tryPeekMessage")>]
        //abstract tryPeekMessage : c:MessagingClientId -> Message option
        abstract tryPeekMessage : c:byte[] -> byte[]

        [<OperationContract(Name = "tryDeleteFromServer")>]
        //abstract tryDeleteFromServer : cm:(MessagingClientId * MessageId) -> bool
        abstract tryDeleteFromServer : cm:byte[] -> byte[]

        [<OperationContract(Name = "getState")>]
        //abstract getState : u:unit -> MsgServiceState
        abstract getState : u:byte[] -> byte[]


    type WcfCommunicator = (IMessagingWcfService-> byte[] -> byte[])

namespace ClmSys

open System
open ClmSys.GeneralData

module MessagingData =

    type MessagingClientName =
        | MessagingClientName of string

        member this.value = let (MessagingClientName v) = this in v


    type MessagingClientId =
        | MessagingClientId of Guid

        member this.value = let (MessagingClientId v) = this in v
        static member create() = Guid.NewGuid() |> MessagingClientId


    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type MessagingServiceAccessInfo =
        {
            messagingServiceAccessInfo : ServiceAccessInfo
        }


    type PartitionerId =
        | PartitionerId of MessagingClientId

        member this.messagingClientId = let (PartitionerId v) = this in v


    type WorkerNodeId =
        | WorkerNodeId of MessagingClientId

        member this.messagingClientId = let (WorkerNodeId v) = this in v


    /// Partitioner MessagingClientId + Messaging Service acces info.
    type PartitionerMsgAccessInfo =
        {
            partitionerId : PartitionerId
            msgSvcAccessInfo : ServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.partitionerId.messagingClientId
                msgSvcAccessInfo = this.msgSvcAccessInfo
            }


    /// Worker Node MessagingClientId + Messaging Server acces info.
    type WorkNodeMsgAccessInfo =
        {
            workerNodeId : WorkerNodeId
            msgSvcAccessInfo : ServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.workerNodeId.messagingClientId
                msgSvcAccessInfo = this.msgSvcAccessInfo
            }

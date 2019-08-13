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


    type PartitionerId =
        | PartitionerId of MessagingClientId

        member this.messagingClientId = let (PartitionerId v) = this in v


    type StorageId =
        | StorageId of MessagingClientId

        member this.messagingClientId = let (StorageId v) = this in v


    type WorkerNodeId =
        | WorkerNodeId of MessagingClientId

        member this.messagingClientId = let (WorkerNodeId v) = this in v


    type PartitionerMsgAccessInfo =
        {
            partitionerId : PartitionerId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type StorageMsgAccessInfo =
        {
            storageId : StorageId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type WorkNodeMsgAccessInfo =
        {
            workerNodeId : WorkerNodeId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    //type MessagingClientAccessInfo =
    //    {
    //        msgClientId : MessagingClientId
    //        msgSvcAccessInfo : ServiceAccessInfo
    //    }


    //type MessagingServiceAccessInfo =
    //    {
    //        messagingServiceAccessInfo : ServiceAccessInfo
    //    }

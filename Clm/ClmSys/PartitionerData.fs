namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

module PartitionerData =

    let defaultPartitionerId = new Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId |> PartitionerId
    let defaultStorageId = new Guid("BDA4BC67-03BF-4461-8074-D16C4B885EB4") |> MessagingClientId |> StorageId


    type PartitionerServiceAccessInfo =
        {
            partitionerMsgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }

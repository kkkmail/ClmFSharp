namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

module PartitionerData =

    let defaultPartitionerMessagingClientId = new Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId
    let defaultStorageMessagingClientId = new Guid("BDA4BC67-03BF-4461-8074-D16C4B885EB4") |> MessagingClientId


    type PartitionerServiceAccessInfo =
        {
            partitionerMsgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }

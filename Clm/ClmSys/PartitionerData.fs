namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

module PartitionerData =

    let defaultPartitionerMessagingClientId = new Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId


    type PartitionerServiceAccessInfo =
        {
            partitionerMsgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }

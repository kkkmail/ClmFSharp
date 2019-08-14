namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

module PartitionerData =

    let defaultPartitionerId = new Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId |> PartitionerId


    type PartitionerServiceAccessInfo =
        {
            partitionerId : PartitionerId
            msgSvcAccessInfo : ServiceAccessInfo
        }

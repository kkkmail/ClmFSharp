namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData

module PartitionerData =

    type PartitionerData =
        {
            partitionerMessangerClientId : MessagingClientId
        }

        static member defaultValue =
            {
                partitionerMessangerClientId = new Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId
            }



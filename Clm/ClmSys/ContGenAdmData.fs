namespace ClmSys

open System
open MessagingData
open MessagingPrimitives
open PartitionerPrimitives
open ContGenPrimitives

module ContGenAdmData =

    type ContGenAdmMsgAccessInfo =
        {
            contGenAdmId : ContGenAdmId
            partitionerId : PartitionerId
            messagingServiceAccessInfo : MessagingServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.contGenAdmId.messagingClientId
                msgSvcAccessInfo = this.messagingServiceAccessInfo
            }

namespace ClmSys

open GeneralData
open MessagingPrimitives

module MessagingData =

    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type MessagingServiceAccessInfo =
        {
            messagingServiceAccessInfo : ServiceAccessInfo
        }


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

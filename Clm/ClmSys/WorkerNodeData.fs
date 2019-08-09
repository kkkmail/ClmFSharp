namespace ClmSys

open System
open ClmSys.GeneralData
open MessagingData

module WorkerNodeData =

    type WorkerNodeServiceAccessInfo =
        {
            wrkMsgClientId : MessagingClientId
            partitionerMsgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }

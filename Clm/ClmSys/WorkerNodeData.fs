namespace ClmSys

open ClmSys.GeneralData
open MessagingData

module WorkerNodeData =

    type WorkerNodeServiceAccessInfo =
        {
            wrkMsgClientId : MessagingClientId
            prtMsgClientId : MessagingClientId
            noOfCores : int
            msgSvcAccessInfo : ServiceAccessInfo
            wrkSvcAccessInfo : ServiceAccessInfo
        }

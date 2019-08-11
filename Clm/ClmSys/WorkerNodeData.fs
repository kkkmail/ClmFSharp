namespace ClmSys

open ClmSys.GeneralData
open MessagingData

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeMsgClientId : MessagingClientId
            noOfCores : int
        }


    type WorkerNodeServiceAccessInfo =
        {
            msgCliAccessInfo : MessagingClientAccessInfo
            noOfCores : int
            prtMsgClientId : MessagingClientId
            wrkSvcAccessInfo : ServiceAccessInfo
        }

        member w.workerNodeInfo =
            {
                workerNodeMsgClientId = w.msgCliAccessInfo.msgClientId
                noOfCores = w.noOfCores
            }

namespace ClmSys

open ClmSys.GeneralData
open ClmSys.MessagingData

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            noOfCores : int
        }


    type WorkerNodeServiceAccessInfo =
        {
            wrkSvcAccessInfo : ServiceAccessInfo
            noOfCores : int
            msgCliAccessInfo : WorkNodeMsgAccessInfo
            partitionerId : PartitionerId
            storageId : StorageId
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.msgCliAccessInfo.workerNodeId
                noOfCores = w.noOfCores
            }

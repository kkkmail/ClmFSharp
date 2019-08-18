namespace ClmSys

open ClmSys.GeneralData
open ClmSys.MessagingData

module WorkerNodeData =

    type WorkerNodePriority =
        | WorkerNodePriority of int

        static member defaultValue = WorkerNodePriority 100


    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            noOfCores : int
            nodePriority : WorkerNodePriority
        }


    type WorkerNodeServiceAccessInfo =
        {
            wrkSvcAccessInfo : ServiceAccessInfo
            noOfCores : int
            msgCliAccessInfo : WorkNodeMsgAccessInfo
            partitionerId : PartitionerId
            nodePriority : WorkerNodePriority
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.msgCliAccessInfo.workerNodeId
                noOfCores = w.noOfCores
                nodePriority = w.nodePriority
            }

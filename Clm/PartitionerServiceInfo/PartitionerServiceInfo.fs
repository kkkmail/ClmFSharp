namespace PartitionerServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    type PartitionerQueueElement =
        {
            remoteProcessId : RemoteProcessId
            runModelParam : RunModelParam
        }


    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            running : list<RemoteProcessId>
        }

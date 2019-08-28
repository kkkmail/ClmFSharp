namespace PartitionerServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    type PartitionerQueueElement =
        {
            queuedRemoteProcessId : RemoteProcessId
        }


    type RunModelParamWithRemoteId =
        {
            remoteProcessId : RemoteProcessId
            runModelParam : RunModelParam
        }

        member this.queueElement =
            {
                queuedRemoteProcessId = this.remoteProcessId
            }


    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            running : list<RemoteProcessId>
        }

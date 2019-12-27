namespace PartitionerServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData

module ServiceInfo =

    type RunModelParamWithRemoteId =
        {
            remoteProcessId : RemoteProcessId
            runModelParam : RunModelParam
        }


    type RunningProcessInfo =
        {
            remoteProcessId : RemoteProcessId
            runQueueId : RunQueueId
        }


    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            runningProcesses : Map<RemoteProcessId, RunQueueId>
        }

        member e.priority =
            (
                e.workerNodeInfo.nodePriority.value,
                (decimal e.runningProcesses.Count) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))
            )

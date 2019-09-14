namespace PartitionerServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData

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


    type RunningProcessInfo =
        {
            remoteProcessId : RemoteProcessId
            runQueueId : RunQueueId
        }


    type WorkerNodeState =
        {
            workerNodeInfo : WorkerNodeInfo
            runningProcesses : list<RunningProcessInfo>
        }

        member e.priority =
            (
                e.workerNodeInfo.nodePriority.value,
                (decimal e.runningProcesses.Length) / (max 1.0m (decimal e.workerNodeInfo.noOfCores))
            )

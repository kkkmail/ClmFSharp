namespace PartitionerServiceInfo
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    type PartitionerQueueElement =
        {
            remoteProcessId : RemoteProcessId
            runModelParam : RunModelParam
        }

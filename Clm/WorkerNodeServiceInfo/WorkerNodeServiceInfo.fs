namespace WorkerNodeServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    [<Literal>]
    let WorkerNodeServiceName = "WorkerNodeService"

    [<Literal>]
    let WorkerNodeServiceProgramName = "WorkerNodeService.exe"


    type IWorkerNodeService =
        abstract updateLocalProgress : LocalProgressUpdateInfo -> unit

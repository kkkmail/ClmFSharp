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

        /// To check if service is working.
        abstract ping : unit -> unit

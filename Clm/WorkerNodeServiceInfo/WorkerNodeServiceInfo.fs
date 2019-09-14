namespace WorkerNodeServiceInfo

open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    [<Literal>]
    let WorkerNodeServiceName = "WorkerNodeService"

    [<Literal>]
    let WorkerNodeServiceProgramName = "WorkerNodeService.exe"


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type IWorkerNodeService =
        abstract updateLocalProgress : LocalProgressUpdateInfo -> unit
        abstract configure : WorkerNodeConfigParam -> unit

        /// To check if service is working.
        abstract ping : unit -> unit

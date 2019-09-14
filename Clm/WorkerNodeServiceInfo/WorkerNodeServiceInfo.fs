namespace WorkerNodeServiceInfo

open ClmSys.VersionInfo
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    let WorkerNodeServiceName = "WorkerNodeService" + " - " + versionNumberValue.value
    let WorkerNodeServiceProgramName = "WorkerNodeService.exe"


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type IWorkerNodeService =
        abstract updateLocalProgress : LocalProgressUpdateInfo -> unit
        abstract configure : WorkerNodeConfigParam -> unit

        /// To check if service is working.
        abstract ping : unit -> unit

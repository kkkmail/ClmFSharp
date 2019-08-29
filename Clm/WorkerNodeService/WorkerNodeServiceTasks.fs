namespace WorkerNodeService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.WindowsService
open WorkerNodeServiceInfo.ServiceInfo

module ServiceTasks =

    let serviceInfo =
        {
            serviceName = ServiceName WorkerNodeServiceName
            runService = startServiceRun
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = ServiceTask<WorkerNodeWindowsService, WorkerNodeServiceAccessInfo, WorkerNodeServiceRunArgs>

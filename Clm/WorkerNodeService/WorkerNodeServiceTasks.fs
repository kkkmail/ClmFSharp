namespace WorkerNodeService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.WindowsService
open WorkerNodeServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels

module ServiceTasks =

    let cleanupService logger i =
        logger.logInfo "WorkerNodeWindowsService: Unregistering TCP channel."
        ChannelServices.UnregisterChannel(i.wrkNodeTcpChannel)


    let serviceInfo =
        {
            serviceName = ServiceName WorkerNodeServiceName
            runService = startServiceRun
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = ServiceTask<WorkerNodeWindowsService, WorkerNodeServiceAccessInfo, WorkerNodeServiceRunArgs>

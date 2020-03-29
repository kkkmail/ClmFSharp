namespace WorkerNodeService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.WindowsService
open WorkerNodeServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels
open ClmSys.WorkerNodePrimitives

module ServiceTasks =

    let cleanupService (logger : Logger) i =
        logger.logInfoString "WorkerNodeWindowsService: Unregistering TCP channel."
        ChannelServices.UnregisterChannel(i.wrkNodeTcpChannel)


    let serviceInfo =
        {
            serviceName = workerNodeServiceName.value
            runService = startServiceRun
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = ServiceTask<WorkerNodeWindowsService, WorkerNodeServiceInfo, WorkerNodeServiceRunArgs>

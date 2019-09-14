namespace PartitionerService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.PartitionerData
open PartitionerService.SvcCommandLine
open PartitionerService.WindowsService
open PartitionerServiceInfo.ServiceInfo

module ServiceTasks =

    let runService l i = startServiceRun l i


    let serviceInfo =
        {
            serviceName = ServiceName PartitionerServiceName
            runService = runService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<PartitionerServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    type PartitionerServiceTask = ServiceTask<PartitionerWindowsService, PartitionerServiceAccessInfo, PartitionerServiceRunArgs>


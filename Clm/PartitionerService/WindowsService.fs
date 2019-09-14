namespace PartitionerService

open System.ServiceProcess
open Argu

open ClmSys.Logging
open ClmSys.PartitionerData
open PartitionerServiceInfo.ServiceInfo
open PartitionerService.ServiceImplementation
open PartitionerService.SvcCommandLine

module WindowsService =

    let startServiceRun (logger : Logger) (i : PartitionerServiceAccessInfo) =
        try
            createServiceImpl i
        with
            | e ->
                logger.logExn "Error occurred" e
                ignore()


    type public PartitionerWindowsService () =
        inherit ServiceBase (ServiceName = PartitionerServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.ignored

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<PartitionerServiceRunArgs>(programName = PartitionerServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun logger i

        override __.OnStop () = base.OnStop()

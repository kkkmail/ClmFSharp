namespace WorkerNodeService

open System.ServiceProcess
open Argu

open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine

module WindowsService =

    let startServiceRun (logger : Logger) (i : WorkerNodeServiceAccessInfo) =
        try
            createServiceImpl i
        with
            | e ->
                logger.logExn "Error occurred" e
                ignore()


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = WorkerNodeServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.ignored

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun logger i

        override __.OnStop () = base.OnStop()

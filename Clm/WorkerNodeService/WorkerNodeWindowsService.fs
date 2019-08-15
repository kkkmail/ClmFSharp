namespace WorkerNodeService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine

module WindowsService =

    let startServiceRun (logger : Logger) (i : WorkerNodeServiceAccessInfo) =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.wrkSvcAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<WorkerNodeService>, WorkerNodeServiceName, WellKnownObjectMode.Singleton )
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

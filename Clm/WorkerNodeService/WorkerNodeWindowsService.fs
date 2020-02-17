namespace WorkerNodeService

open System
open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ProgressNotifierClient.ServiceResponse
open ClmSys.TimerEvents

module WindowsService =
    let private serviceName = WorkerNodeServiceName


    let startServiceRun (logger : Logger) (i : WorkerNodeServiceAccessInfo) : WrkNodeShutDownInfo option =
        try
            logger.logInfoString (sprintf "WindowsService.startServiceRun: registering service %s..." serviceName)
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.workerNodeServiceAccessInfo.servicePort.value)
            logger.logInfoString (sprintf "WindowsService.startServiceRun: registering TCP channel for WorkerNodeService on port: %A" i.workerNodeServiceAccessInfo.servicePort)
            ChannelServices.RegisterChannel (channel, false)
            RemotingConfiguration.RegisterWellKnownServiceType (typeof<WorkerNodeService>, serviceName, WellKnownObjectMode.Singleton)
            let service = (new WorkerNodeResponseHandler(i)).workerNodeService

            try
                logger.logInfoString "WindowsService.startServiceRun: Calling: service.ping()..."
                match service.ping() with
                | Ok() -> ignore()
                | Error e -> logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)
            with
            | e -> logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)

            let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger service.ping "WorkerNodeServiceAccess - ping")
            do h.start()

            {
                wrkNodeTcpChannel = channel
            }
            |> Some

        with
        | e ->
            logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)
            None


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = serviceName)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : WrkNodeShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfoString "WorkerNodeWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(i.wrkNodeTcpChannel)
                shutDownInfo <- None
            | None -> ignore()


        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

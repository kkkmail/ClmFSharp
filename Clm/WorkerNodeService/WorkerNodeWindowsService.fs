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
open ClmSys.TimerEvents
open ClmSys.WorkerNodePrimitives

module WindowsService =

    let private serviceName = workerNodeServiceName


    let startServiceRun (logger : Logger) (i : WorkerNodeServiceInfo) : WrkNodeShutDownInfo option =
        try
            let name = serviceName.value.value
            let address = i.workerNodeServiceAccessInfo.workerNodeServiceAddress.value.value
            let port = i.workerNodeServiceAccessInfo.workerNodeServicePort.value.value
            logger.logInfoString (sprintf "WindowsService.startServiceRun: registering service %s..." name)
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (port)
            logger.logInfoString (sprintf "WindowsService.startServiceRun: registering TCP channel for WorkerNodeService on address: %s, port: %i" address port)
            ChannelServices.RegisterChannel (channel, false)
            RemotingConfiguration.RegisterWellKnownServiceType (typeof<WorkerNodeService>, name, WellKnownObjectMode.Singleton)
            let service = (new WorkerNodeResponseHandler(i.workerNodeServiceAccessInfo)).workerNodeService

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
        inherit ServiceBase (ServiceName = serviceName.value.value)

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
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

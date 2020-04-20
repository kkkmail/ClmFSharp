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
open ClmSys.Wcf
open ClmSys.ClmErrors
open System.ServiceModel
open ClmSys
open ClmSys.WorkerNodeErrors

module WindowsService =

    let private serviceName = workerNodeServiceName

    let private workerNodeRunner : Lazy<ClmResult<WorkerNodeRunner>> =
        new Lazy<ClmResult<WorkerNodeRunner>>(fun () -> WorkerNodeRunner.create serviceAccessInfo)


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    type WorkerNodeWcfService() =
        let toConfigureError f = f |> ConfigureWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let toMonitorError f = f |> MonitorWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let toPingError f = f |> PingWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr

        let configure a = workerNodeRunner.Value |> Rop.bind (fun e -> e.configure a)
        let monitor () = workerNodeRunner.Value |> Rop.bind (fun e -> e.getState() |> Ok)
        let ping () = workerNodeRunner.Value |> Rop.bind (fun e -> Ok())

        interface IWorkerNodeWcfService with
            member _.configure b = tryReply configure toConfigureError b
            member _.monitor b = tryReply monitor toMonitorError b
            member _.ping b = tryReply ping toPingError b


    //let startServiceRun (logger : Logger) (i : WorkerNodeServiceInfo) : WrkNodeShutDownInfo option =
    //    try
    //        let name = serviceName.value.value
    //        let address = i.workerNodeServiceAccessInfo.workerNodeServiceAddress.value.value
    //        let port = i.workerNodeServiceAccessInfo.workerNodeServicePort.value.value
    //        logger.logInfoString (sprintf "WindowsService.startServiceRun: registering service %s..." name)
    //        serviceAccessInfo <- i
    //        let channel = new Tcp.TcpChannel (port)
    //        logger.logInfoString (sprintf "WindowsService.startServiceRun: registering TCP channel for WorkerNodeService on address: %s, port: %i" address port)
    //        ChannelServices.RegisterChannel (channel, false)
    //        RemotingConfiguration.RegisterWellKnownServiceType (typeof<WorkerNodeService>, name, WellKnownObjectMode.Singleton)
    //        let service = (new WorkerNodeResponseHandler(i.workerNodeServiceAccessInfo)).workerNodeService
    //
    //        try
    //            logger.logInfoString "WindowsService.startServiceRun: Calling: service.ping()..."
    //            match service.ping() with
    //            | Ok() -> ignore()
    //            | Error e -> logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)
    //        with
    //        | e -> logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)
    //
    //        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger service.ping "WorkerNodeServiceAccess - ping")
    //        do h.start()
    //
    //        {
    //            wrkNodeTcpChannel = channel
    //        }
    //        |> Some
    //
    //    with
    //    | e ->
    //        logger.logInfoString (sprintf "WindowsService.startServiceRun: %A" e)
    //        None


    let startWrkNodeWcfServiceRun (logger : Logger) (i : WorkerNodeServiceInfo) : WrkNodeWcfSvcShutDownInfo option =
        try
            printfn "startWrkNodeWcfServiceRun: Creating WCF Worker Node Service..."
            serviceAccessInfo <- i

            match workerNodeRunner.Value with
            | Ok r ->
                r.start()

                let binding = getBinding()
                let baseAddress = new Uri(i.workerNodeServiceAccessInfo.wcfServiceUrl)
                let serviceHost = new ServiceHost(typeof<WorkerNodeWcfService>, baseAddress)
                let d = serviceHost.AddServiceEndpoint(typeof<IWorkerNodeWcfService>, binding, baseAddress)
                do serviceHost.Open()
                printfn "startWrkNodeWcfServiceRun: Completed."

                {
                    wrkNodeServiceHost = serviceHost
                }
                |> Some
            | Error e ->
                printfn "startWrkNodeWcfServiceRun: Error - %A." e
                None
        with
        | e ->
            logger.logExn "startWrkNodeWcfServiceRun: Error starting WCF Worker Node Service." e
            None


    let cleanupService (logger : Logger) (i : WrkNodeWcfSvcShutDownInfo) =
        try
            logger.logInfoString "WorkerNodeWindowsService: Closing WCF service host."
            i.wrkNodeServiceHost.Close()
        with
        | e -> logger.logExn "WorkerNodeWindowsService: Exception occurred: " e


    //type public WorkerNodeWindowsService () =
    //    inherit ServiceBase (ServiceName = serviceName.value.value)

    //    let initService () = ()
    //    do initService ()
    //    let logger = Logger.log4net
    //    let mutable shutDownInfo : WrkNodeShutDownInfo option = None

    //    let tryDispose() =
    //        match shutDownInfo with
    //        | Some i ->
    //            logger.logInfoString "WorkerNodeWindowsService: Unregistering TCP channel."
    //            ChannelServices.UnregisterChannel(i.wrkNodeTcpChannel)
    //            shutDownInfo <- None
    //        | None -> ignore()


    //    override __.OnStart (args : string[]) =
    //        base.OnStart(args)
    //        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
    //        let results = (parser.Parse args).GetAllResults()
    //        let i = getServiceAccessInfo results
    //        shutDownInfo <- startServiceRun logger i

    //    override __.OnStop () =
    //        tryDispose()
    //        base.OnStop()

    //    interface IDisposable with
    //        member __.Dispose() = tryDispose()


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = serviceName.value.value)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : WrkNodeWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                cleanupService logger i
                shutDownInfo <- None
            | None -> ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startWrkNodeWcfServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

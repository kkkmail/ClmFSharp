namespace ContGenService

open System
open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.Runtime.Remoting.Channels.Tcp
open Argu

open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ContGenAdm.ContGenServiceResponse
open ClmSys.TimerEvents

module WindowsService =

    let startServiceRun (logger : Logger) (i : ContGenServiceAccessInfo) =
        try
            logger.logInfo ("startServiceRun: registering ContGenService...")
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.contGenServiceAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<ContGenService>, ContGenServiceName, WellKnownObjectMode.Singleton )

            let service = (new ContGenResponseHandler(i)).contGenService
            //p |> List.map (fun e -> service.configureService e) |> ignore
            service.loadQueue()
            let h = new EventHandler(EventHandlerInfo.defaultValue (fun () -> getServiceState service))
            do h.start()
            Some channel

        with
            | e ->
                logger.logExn "Starting service" e
                None


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ContGenServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.ignored
        let mutable channel : TcpChannel option = None

        let tryUnregisterChannel() =
            match channel with
            | Some c ->
                logger.logInfo "ContGenWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(c)
                channel <- None
            | None -> ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            channel <- startServiceRun logger i

        override __.OnStop () =
            tryUnregisterChannel()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryUnregisterChannel()

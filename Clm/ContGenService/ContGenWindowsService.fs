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
open ClmSys.ContGenData

module WindowsService =

    let startServiceRun (logger : Logger) (i : ContGenServiceAccessInfo) : ContGenShutDownInfo option =
        try
            logger.logInfoString ("startServiceRun: registering ContGenService...")
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.contGenServiceAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                (typeof<ContGenService>, ContGenServiceName, WellKnownObjectMode.Singleton)

            let service = (new ContGenResponseHandler(i)).contGenService
            //let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue (logger.logError) (fun () -> getServiceState service))
            //do h.start()

            let modelRunner =
                createModelRunnerImpl logger parserResults

            do
                modelRunner.start()

            {
                contGenTcpChannel = channel
                service = service
            }
            |> Some

        with
        | e ->
            logger.logExn "Starting service" e
            None


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ContGenServiceName)

        let logger =
            Logger.log4net

        let initService () = ()
        do initService ()

        let mutable shutDownInfo : ContGenShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfoString "ContGenWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(i.contGenTcpChannel)
                shutDownInfo <- None
            | None -> ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

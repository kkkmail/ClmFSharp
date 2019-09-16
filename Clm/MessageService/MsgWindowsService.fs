namespace MessagingService

open System
open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.Runtime.Remoting.Channels.Tcp
open Argu

open ClmSys.MessagingData
open ClmSys.Logging
open MessagingServiceInfo.ServiceInfo
open MessagingService.ServiceImplementation
open MessagingService.SvcCommandLine

module WindowsService =

    let startServiceRun (logger : Logger) (i : MessagingServiceAccessInfo) : MsgSvcShutDownInfo option =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.messagingServiceAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                (typeof<MessagingRemoteService>, MessagingServiceName, WellKnownObjectMode.Singleton)

            {
                msgSvcTcpChannel = channel
            }
            |> Some
        with
        | e ->
            logger.logExn "Error starting service." e
            None


    type public MessagingWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : MsgSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfo "MessagingWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(i.msgSvcTcpChannel)
                shutDownInfo <- None
            | None -> ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

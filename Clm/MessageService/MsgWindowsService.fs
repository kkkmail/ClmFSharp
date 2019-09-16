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

    let startServiceRun (logger : Logger) (i : MessagingServiceAccessInfo) =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.messagingServiceAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                (typeof<MessagingRemoteService>, MessagingServiceName, WellKnownObjectMode.Singleton)

            Some channel
        with
        | e ->
            logger.logExn "Error starting service." e
            None


    type public MessagingWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable channel : TcpChannel option = None

        let tryUnregisterChannel() =
            match channel with
            | Some c ->
                logger.logInfo "MessagingWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(c)
                channel <- None
            | None -> ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            channel <- startServiceRun logger i

        override __.OnStop () =
            tryUnregisterChannel()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryUnregisterChannel()

namespace MessagingServer

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open MessagingServer.ServiceImplementation
open Messaging.Server
open MessagingServer.SvcCommandLine

module WindowsService =

    let startServiceRun (i : MessagingServerAccessInfo) logger =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.messagingServerAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<ClmMessagingServer>, MessagingServiceName, WellKnownObjectMode.Singleton )
        with
            | e ->
                logger e
                ignore()


    type public MessagingServerWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger _ = ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServerRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun i logger

        override __.OnStop () = base.OnStop()

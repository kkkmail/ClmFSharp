﻿namespace MessagingService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ClmSys.GeneralData
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open MessagingService.ServiceImplementation
open Messaging.Server
open MessagingService.SvcCommandLine

module WindowsService =

    let startServiceRun (i : MessagingServiceAccessInfo) logger =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.messagingServiceAccessInfo.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<ClmMessagingServer>, MessagingServiceName, WellKnownObjectMode.Singleton )
        with
            | e ->
                logger e
                ignore()


    type public MessagingWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger _ = ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo versionNumberValue results
            startServiceRun i logger

        override __.OnStop () = base.OnStop()

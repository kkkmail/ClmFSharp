﻿namespace WorkerNodeService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ClmSys.WorkerNodeData

//open Messaging.Service
//open MessagingService.SvcCommandLine

module WindowsService =

    let startServiceRun (i : WorkerNodeServiceAccessInfo) logger =
        try
            serviceAccessInfo <- i
            //let channel = new Tcp.TcpChannel (i.messagingServiceAccessInfo.servicePort.value)
            //ChannelServices.RegisterChannel (channel, false)

            //RemotingConfiguration.RegisterWellKnownServiceType
            //    ( typeof<MessagingRemoteService>, MessagingServiceName, WellKnownObjectMode.Singleton )
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
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun i logger

        override __.OnStop () = base.OnStop()

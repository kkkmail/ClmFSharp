﻿namespace WorkerNodeService

open System
open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.Runtime.Remoting.Channels.Tcp
open Argu

open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ProgressNotifierClient.ServiceResponse
open ClmSys.TimerEvents

module WindowsService =

    let startServiceRun (logger : Logger) (i : WorkerNodeServiceAccessInfo) : WrkNodeShutDownInfo option =
        try
            logger.logInfo ("WindowsService.startServiceRun: registering WorkerNodeService...")
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.workerNodeServiceAccessInfo.servicePort.value)
            logger.logInfo (sprintf "startServiceRun: registering TCP channel for WorkerNodeService on port: %A" i.workerNodeServiceAccessInfo.servicePort)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<WorkerNodeService>, WorkerNodeServiceName, WellKnownObjectMode.Singleton )

            let service = (new WorkerNodeResponseHandler(i)).workerNodeService

            try
                logger.logInfo "WindowsService.startServiceRun: Calling: service.ping()..."
                service.ping()
            with
            | e -> logger.logExn "WindowsService.startServiceRun" e

            let h = new EventHandler(EventHandlerInfo.defaultValue (logger.logExn "WorkerNodeService") service.ping)
            do h.start()

            {
                wrkNodeTcpChannel = channel
            }
            |> Some

        with
        | e ->
            logger.logExn "WindowsService.startServiceRun" e
            None


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = WorkerNodeServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : WrkNodeShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfo "WorkerNodeWindowsService: Unregistering TCP channel."
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

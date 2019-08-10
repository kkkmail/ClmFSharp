namespace WorkerNodeService

open System.ServiceProcess
open Argu

open ClmSys.Logging
open MessagingServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ClmSys.WorkerNodeData

module WindowsService =

    let startServiceRun (logger : Logger) (i : WorkerNodeServiceAccessInfo) =
        try
            serviceAccessInfo <- i
            //let channel = new Tcp.TcpChannel (i.messagingServiceAccessInfo.servicePort.value)
            //ChannelServices.RegisterChannel (channel, false)

            //RemotingConfiguration.RegisterWellKnownServiceType
            //    ( typeof<MessagingRemoteService>, MessagingServiceName, WellKnownObjectMode.Singleton )
        with
            | e ->
                logger.logExn "Error occurred" e
                ignore()


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.ignored

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun logger i

        override __.OnStop () = base.OnStop()

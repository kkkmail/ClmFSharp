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
open System.ServiceModel
open System.ServiceModel.Description

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


    let startWcfServiceRun (logger : Logger) (i : MessagingServiceAccessInfo) : MsgWcfSvcShutDownInfo option =
        try
            printfn "startWcfServiceRun: Creating WCF service..."
            serviceAccessInfo <- i
            let binding = new NetTcpBinding()
            let baseAddress = new Uri(i.messagingServiceAccessInfo.wcfServiceUrl)
            let serviceHost = new ServiceHost(typeof<MessagingWcfService>, baseAddress)
            let d = serviceHost.AddServiceEndpoint(typeof<IMessagingWcfService>, binding, baseAddress)
            do serviceHost.Open()
            printfn "... completed."

            {
                serviceHost = serviceHost
            }
            |> Some
        with
        | e ->
            logger.logExn "Error starting WCF service." e
            None


    type public MessagingWindowsService () =
        inherit ServiceBase (ServiceName = MessagingServiceName)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : MsgSvcShutDownInfo option = None
        let mutable shutDownWcfInfo : MsgWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfo "MessagingWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(i.msgSvcTcpChannel)
                shutDownInfo <- None
            | None -> ignore()

            match shutDownWcfInfo with
            | Some i ->
                logger.logInfo "MessagingWindowsService: Closing WCF service host."
                i.serviceHost.Close()
                shutDownInfo <- None
            | None -> ignore()


        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = MessagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            //shutDownInfo <- startServiceRun logger i
            shutDownWcfInfo <- startWcfServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

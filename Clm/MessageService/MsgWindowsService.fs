namespace MessagingService

open System
open System.ServiceProcess
open System.Runtime.Remoting.Channels
open Argu
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.Wcf
open MessagingServiceInfo.ServiceInfo
open MessagingService.ServiceImplementation
open MessagingService.SvcCommandLine
open System.ServiceModel
open ClmSys.MessagingPrimitives

module WindowsService =

    let startWcfServiceRun (logger : Logger) (i : MessagingServiceAccessInfo) : MsgWcfSvcShutDownInfo option =
        try
            printfn "startWcfServiceRun: Creating WCF service..."
            serviceAccessInfo <- i
            let binding = getBinding()
            let baseAddress = new Uri(i.wcfServiceUrl)

            let serviceHost = new ServiceHost(typeof<MessagingWcfService>, baseAddress)

            ////https://stackoverflow.com/questions/8902203/programmatically-set-instancecontextmode/8908511
            //let serviceHost = new ServiceHost(new MessagingWcfService(), baseAddress)
            //let sb = serviceHost.Description.Behaviors.[typeof<ServiceBehaviorAttribute>] :?> ServiceBehaviorAttribute
            //sb.InstanceContextMode <- InstanceContextMode.Single

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
        inherit ServiceBase (ServiceName = messagingServiceName.value.value)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownWcfInfo : MsgWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownWcfInfo with
            | Some i ->
                try
                    logger.logInfoString "MessagingWindowsService: Closing WCF service host."
                    i.serviceHost.Close()
                with
                | e -> logger.logExn "MessagingWindowsService: Exception occurred: " e

                shutDownWcfInfo <- None
            | None -> ignore()


        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = messagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownWcfInfo <- startWcfServiceRun logger i

        override __.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member __.Dispose() = tryDispose()

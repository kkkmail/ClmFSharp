namespace ContGenService

open System
open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.ServiceModel
open Argu

open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ContGenService.SvcCommandLine

module WindowsService =

    let startServiceRun (i : ServiceAccessInfo) logger =
        try
            let channel = new Tcp.TcpChannel (i.servicePort.value)
            ChannelServices.RegisterChannel (channel, false)

            RemotingConfiguration.RegisterWellKnownServiceType
                ( typeof<ContGenService>, ContGenServiceName, WellKnownObjectMode.Singleton )
        with
            | e ->
                logger e
                ignore()


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ContGenServiceName)

        let initService () = ()
        do initService ()
        let logger e = ignore()

        override service.OnStart (args:string[]) =

            let parser = ArgumentParser.Create<RunArgs>(programName = ProgramName)
            let results = (parser.Parse args).GetAllResults()

            match tryGetServiceAccessInfo results with
                | Some i ->
                    base.OnStart(args)
                    startServiceRun i logger
                | None ->
                    let errMessage = "Invalid service address and/or port."
                    logger errMessage
                    failwith errMessage

        override service.OnStop () =
            base.OnStop()

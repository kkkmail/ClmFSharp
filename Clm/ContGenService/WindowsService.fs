namespace ContGenService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open Argu

open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ContGenService.SvcCommandLine

module WindowsService =

    let startServiceRun (i : ContGenServiceAccessInfo) logger =
        try
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.contGenServiceAccessInfo.servicePort.value)
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
        let logger _ = ignore()

        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            startServiceRun i logger

        override __.OnStop () = base.OnStop()

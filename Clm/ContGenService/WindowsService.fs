namespace ContGenService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.ServiceModel

open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open System

module WindowsService =

    let startServiceRun logger =
        try
            let channel = new Tcp.TcpChannel (ContGenServicePort)
            ChannelServices.RegisterChannel (channel, false)

            //let host = new ServiceHost(typeof<IContGenService>, new Uri(getServiceUrl()))
            //do host.AddServiceEndpoint(typeof<IContGenService>, new UdpBinding(), "udp") |> ignore

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
            base.OnStart(args)
            startServiceRun logger

        override service.OnStop () =
            base.OnStop()

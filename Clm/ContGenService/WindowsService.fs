namespace ContGenService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels

open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo

module WindowsService =

    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ContGenServiceName)

        let initService () = ()
        do initService ()

        override service.OnStart (args:string[]) =
            base.OnStart(args)
            let servicePort = ContGenServicePort

            try
                let channel = new Tcp.TcpChannel (servicePort)
                ChannelServices.RegisterChannel (channel, false)

                RemotingConfiguration.RegisterWellKnownServiceType
                    ( typeof<ContGenService>, ContGenServiceName, WellKnownObjectMode.Singleton )
            with
                | e ->
                    ignore()

        override service.OnStop () =
            base.OnStop()

namespace ContGenService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels

open ServiceInfo.ServiceConfiguration
open ContGenService.ServiceImplementation
open ContGenService.ContGenServiceInfo

module WindowsService =

    type public ProgressNotifierWindowsService () =
        inherit ServiceBase (ServiceName = ProgressNotifierServiceName)

        let initService () = ()
        do initService ()

        override service.OnStart (args:string[]) =
            base.OnStart(args)
            let servicePort = 12345

            try
                let channel = new Tcp.TcpChannel (servicePort)
                ChannelServices.RegisterChannel (channel, false)

                RemotingConfiguration.RegisterWellKnownServiceType
                    ( typeof<ProgressNotifierService>, ProgressNotifierServiceName, WellKnownObjectMode.Singleton )
            with
                | e ->
                    ignore()

        override service.OnStop () =
            base.OnStop()


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ContGenServiceName)

        let initService () = ()
        do initService ()

        override service.OnStart (args:string[]) =
            base.OnStart(args)
            let servicePort = 12346

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

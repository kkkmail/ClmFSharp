﻿namespace ContGenService

open System.ServiceProcess
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels

open ServiceInfo.ServiceConfiguration
open ContGenService.ServiceImplementation

module WindowsService =
    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ServiceName)

        let initService () = ()
        do initService ()

        override service.OnStart (args:string[]) =
            base.OnStart(args)
            let servicePort = 12345

            try
                let channel = new Tcp.TcpChannel (servicePort)
                ChannelServices.RegisterChannel (channel, false)

                RemotingConfiguration.RegisterWellKnownServiceType
                    ( typeof<ContGenService>, ServiceName, WellKnownObjectMode.Singleton )
            with
                | e ->
                    ignore()

        override service.OnStop () =
            base.OnStop()

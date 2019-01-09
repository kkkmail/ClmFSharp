namespace ContGenService

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Linq;
open System.ServiceProcess
open System.Text
open System.Runtime.Remoting
open System.Runtime.Remoting.Channels
open System.Xml
open System.Xml.Linq

open ServiceInfo.ServiceConfiguration
open ContGenService.ServiceImplementation

//open DiscoverInterface.Configuration
//open DiscoverInterface.Serialization
//open DiscoverInterface.Service
//open DiscoverService.AppLog
//open DiscoverService.ServiceImplementation

module WindowsService = 
    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = ServiceName)

//        let initService () : unit = ()
//        do initService ()

        override service.OnStart (args:string[]) =
            base.OnStart(args)
            //DiscoverLog.Instance.logEvent "Started" |> ignore
            //let servicePort : int = ClientConfigParams.Default.servicePort
            let servicePort = 42042

//            let servicePort : int = 
//                try
//                    let codeBaseLoc : string = Path.GetDirectoryName (Assembly.GetExecutingAssembly().CodeBase)
//
//                    let path : string = 
//                        if  codeBaseLoc.ToLower().StartsWith("file:") then codeBaseLoc.Substring(6)
//                        else codeBaseLoc
//
//                    let configFile : string = (if path.EndsWith ("\\") then path else path + "\\") + ServiceConfigFileName
//
//                    match File.ReadAllText configFile |> XElement.Parse |> tryGetServicePort with 
//                    | Some p -> p
//                    | None -> DiscoverServicePort
//                with
//                    | e -> 
//                        DiscoverLog.Instance.logError (e.Message) |> ignore
//                        DiscoverServicePort

            try
                let channel = new Tcp.TcpChannel (servicePort)
                ChannelServices.RegisterChannel (channel, false)

                RemotingConfiguration.RegisterWellKnownServiceType
                    ( typeof<ContGenService>, ServiceName, WellKnownObjectMode.Singleton )
            with
                | e -> 
                    //DiscoverLog.Instance.logError (e.Message) |> ignore
                    ignore()

        override service.OnStop () =
            base.OnStop()
            //DiscoverLog.Instance.logEvent "Ended" |> ignore

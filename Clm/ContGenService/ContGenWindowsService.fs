namespace ContGenService

open System
open System.ServiceProcess
open System.Runtime.Remoting.Channels
open Argu
open ContGenService.ServiceImplementation
open ContGenServiceInfo.ServiceInfo
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ClmSys.ContGenData
open ClmSys.ContGenPrimitives

module WindowsService =

    let startServiceRun (logger : Logger) (i : ContGenServiceInfo) : ContGenShutDownInfo option =
        try
            logger.logInfoString ("startServiceRun: registering ContGenService...")
            serviceAccessInfo <- i
            let channel = new Tcp.TcpChannel (i.contGenServiceAccessInfo.contGenServicePort.value.value)
            ChannelServices.RegisterChannel (channel, false)
            let modelRunner = createModelRunnerImpl logger parserResults
            do modelRunner.start()

            {
                contGenTcpChannel = channel
            }
            |> Some

        with
        | e ->
            logger.logExn "startServiceRun: Starting service failed." e
            None


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = contGenServiceName.value.value)

        let mutable shutDownInfo : ContGenShutDownInfo option = None
        let logger = Logger.log4net
        let initService () = ()
        do initService ()


        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                logger.logInfoString "ContGenWindowsService: Unregistering TCP channel."
                ChannelServices.UnregisterChannel(i.contGenTcpChannel)
                shutDownInfo <- None
            | None -> ignore()


        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startServiceRun logger i


        override __.OnStop () =
            tryDispose()
            base.OnStop()


        interface IDisposable with
            member __.Dispose() = tryDispose()

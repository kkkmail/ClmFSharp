namespace ContGenService

open System
open System.ServiceProcess
open Argu
open ContGenServiceInfo.ServiceInfo
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ClmSys.ContGenPrimitives

module WindowsService =

    let startServiceRun (logger : Logger) parserResults =
        try
            logger.logInfoString ("startServiceRun: registering ContGenService...")
            let modelRunner = createModelRunnerImpl logger parserResults
            do modelRunner.start()
        with
        | e -> logger.logExn "startServiceRun: Starting service failed." e


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = contGenServiceName.value.value)

        let logger = Logger.log4net
        let initService () = ()
        do initService ()
        let tryDispose() = ignore()


        override __.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            startServiceRun logger results


        override __.OnStop () =
            tryDispose()
            base.OnStop()


        interface IDisposable with
            member __.Dispose() = tryDispose()

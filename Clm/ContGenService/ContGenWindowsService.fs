namespace ContGenService

open System
open System.ServiceProcess
open System.ServiceModel
open Argu
open ContGenServiceInfo.ServiceInfo
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ClmSys.ContGenPrimitives
open ClmSys.ContGenData
open ClmSys.Wcf
open ContGen.ModelRunner
open ClmSys.ContGenErrors
open ClmSys.ClmErrors
open ClmSys

module WindowsService =

    let mutable serviceData : ContGenServiceData = getContGenServiceData logger []
    let modelRunner : Lazy<ClmResult<ModelRunner>> = new Lazy<ClmResult<ModelRunner>>(fun () -> ModelRunner.create serviceData.modelRunnerData)


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    type ContGenWcfService() =
        let toGetVersionError f = f |> TryDeleteRunQueueWcfErr |> TryDeleteRunQueueErr |> ContGenServiceErr
        let tryCancelRunQueue a = modelRunner.Value |> Rop.bind (fun e -> e.tryCancelRunQueue a)

        interface IContGenWcfService with
            member _.tryCancelRunQueue b = tryReply tryCancelRunQueue toGetVersionError b


    //let startServiceRun (logger : Logger) parserResults =
    //    try
    //        logger.logInfoString ("startServiceRun: registering ContGenService...")
    //        let modelRunner = createModelRunnerImpl logger parserResults
    //        do modelRunner.start()
    //    with
    //    | e -> logger.logExn "startServiceRun: Starting service failed." e

    // ContGenServiceAccessInfo


    let startWcfServiceRun (logger : Logger) (i : ContGenServiceData) : ContGenWcfSvcShutDownInfo option =
        try
            printfn "startWcfServiceRun: Creating WCF ContGen Service..."
            serviceData <- i
            let binding = getBinding()
            let baseAddress = new Uri(i.contGenServiceAccessInfo.wcfServiceUrl)

            let serviceHost = new ServiceHost(typeof<ContGenWcfService>, baseAddress)

            let d = serviceHost.AddServiceEndpoint(typeof<IContGenWcfService>, binding, baseAddress)
            do serviceHost.Open()
            printfn "... completed."

            {
                contGenServiceHost = serviceHost
            }
            |> Some
        with
        | e ->
            logger.logExn "Error starting WCF ContGen Service." e
            None


    //type public ContGenWindowsService () =
    //    inherit ServiceBase (ServiceName = contGenServiceName.value.value)

    //    let logger = Logger.log4net
    //    let initService () = ()
    //    do initService ()
    //    let tryDispose() = ignore()


    //    override __.OnStart (args : string[]) =
    //        base.OnStart(args)
    //        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
    //        let results = (parser.Parse args).GetAllResults()
    //        startServiceRun logger results


    //    override __.OnStop () =
    //        tryDispose()
    //        base.OnStop()


    //    interface IDisposable with
    //        member __.Dispose() = tryDispose()

    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = contGenServiceName.value.value)

        let logger = Logger.log4net
        let initService () = ()
        do initService ()
        let mutable shutDownWcfInfo : ContGenWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownWcfInfo with
            | Some i ->
                try
                    logger.logInfoString "ContGenWindowsService: Closing WCF service host."
                    i.contGenServiceHost.Close()
                with
                | e -> logger.logExn "ContGenWindowsService: Exception occurred: " e

                shutDownWcfInfo <- None
            | None -> ignore()


        override _.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getContGenServiceData logger results
            shutDownWcfInfo <- startWcfServiceRun logger i

        override _.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member _.Dispose() = tryDispose()

namespace ContGenService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open SvcCommandLine
open ClmSys.Logging
open ClmSys.ContGenData
open ClmSys.PartitionerData
open ClmSys.SolverRunnerData
open ContGen.ModelRunner
open DbData.Configuration


module ServiceImplementation =

    let parserResults =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
        (parser.Parse [||]).GetAllResults()


    let mutable serviceAccessInfo = getServiceAccessInfo parserResults


    type ContGenService () =
        inherit MarshalByRefObject()

        do printfn "Initializing ContGenService..."

        //let modelRunner = createModelRunner logger parserResults
        let modelMonitor = ModelMonitor.create clmConnectionString

        //do
        //    modelRunner.start()

        //let serviceProxy, r = getServiceProxy logger parserResults
        //let u = match r with | Some _ -> true | None -> false
        //let a = createServiceImpl (ContGenSvcAccessInfo serviceAccessInfo) serviceProxy u

        //let initService () =
        //    match r with
        //    | Some p ->
        //        {
        //            onUpdateProgress = a.progressUpdated
        //            setRunLimit = fun c -> c |> SetRunLimit |> a.configureService
        //        }
        //        |> p.start
        //    | None -> ignore()

        //do initService ()

        interface IContGenService with
            //member __.getState() = a.getState().runnerState
            member __.getState() = modelMonitor.getRunState()
            //member __.loadQueue() = a.queueStarting()
            //member __.startGenerate() = a.generationStarted()
            //member __.updateLocalProgress p = p.toProgressUpdateInfo() |> a.progressUpdated 
            //member __.updateRemoteProgress p = p.toProgressUpdateInfo() |> a.progressUpdated
            ////member __.configureService (p : ContGenConfigParam) = a.configureService p
            //member __.runModel m p = a.runModel (m, p)

namespace ContGenService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.TimerEvents
//open ContGen.AsyncRun
//open ContGen.Runner
open ContGenServiceInfo.ServiceInfo
open SvcCommandLine
//open ContGen.Partitioner
open ClmSys.Logging
open ClmSys.ContGenData
open ClmSys.PartitionerData
open ClmSys.SolverRunnerData


module ServiceImplementation =

    //let createServiceImpl i p u =
    //    let a = createRunner (ModelRunnerData.defaultValue i p) u
    //    //let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue Logger.log4net.logError a.generationStarted)
    //    //do h.start()
    //    a


    //type AsyncRunnerState
    //    with
    //    member s.runnerState =
    //        {
    //            runLimit = s.runLimit
    //            //maxQueueLength = s.maxQueueLength
    //            runningCount = s.runningCount
    //            running = s.running |> Map.toArray |> Array.map (fun (_, e) -> e)
    //            queue = s.queue |> List.map (fun e -> e.processToStartInfo.modelDataId) |> Array.ofList
    //            workState = s.workState
    //            messageCount = s.messageCount
    //            minUsefulEe = s.minUsefulEe
    //            lastRunError = s.lastRunError
    //        }


    let parserResults =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
        (parser.Parse [||]).GetAllResults()


    let mutable serviceAccessInfo = getServiceAccessInfo parserResults


    type ContGenService () =
        inherit MarshalByRefObject()

        let modelRunner = createModelRunner logger parserResults

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
            member __.getState() = modelRunner.getRunState()
            //member __.loadQueue() = a.queueStarting()
            //member __.startGenerate() = a.generationStarted()
            //member __.updateLocalProgress p = p.toProgressUpdateInfo() |> a.progressUpdated 
            //member __.updateRemoteProgress p = p.toProgressUpdateInfo() |> a.progressUpdated
            ////member __.configureService (p : ContGenConfigParam) = a.configureService p
            //member __.runModel m p = a.runModel (m, p)

        member x.y = 0

namespace ContGenService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.TimerEvents
open ContGen.AsyncRun
open ContGen.Runner
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner
open SvcCommandLine
open ContGen.Partitioner
open ClmSys.Logging

module ServiceImplementation =

    let createServiceImpl i p u =
        let a = createRunner (ModelRunnerParam.defaultValue i p) u
        let h = new EventHandler(EventHandlerInfo.defaultValue a.startGenerate)
        do h.start()
        a


    type AsyncRunnerState
        with
        member s.runnerState =
            {
                runLimit = s.runLimit
                maxQueueLength = s.maxQueueLength
                runningCount = s.runningCount
                running = s.running |> Map.toArray |> Array.map (fun (_, e) -> e)
                queue = s.queue |> List.map (fun e -> e.modelDataId) |> Array.ofList
                workState = s.workState
                messageCount = s.messageCount
                minUsefulEe = s.minUsefulEe
            }

    let parserResults =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
        (parser.Parse [||]).GetAllResults()


    let mutable serviceAccessInfo = getServiceAccessInfo parserResults


    type ContGenService () =
        inherit MarshalByRefObject()

        let serviceProxy, r = getServiceProxy parserResults
        let u = match r with | Some _ -> true | None -> false
        let a = createServiceImpl (ContGenSvcAccessInfo serviceAccessInfo) serviceProxy u

        let initService () =
            match r with
            | Some p ->
                {
                    onUpdateProgress = a.updateProgress
                    setRunLimit = fun c -> c |> SetRunLimit |> a.configureService
                }
                |> p.start
            | None -> ignore()

        do initService ()

        interface IContGenService with
            member __.getState() = a.getState().runnerState
            member __.loadQueue() = a.startQueue()
            member __.startGenerate() = a.startGenerate()
            member __.updateLocalProgress p = a.updateProgress p.progressUpdateInfo
            member __.updateRemoteProgress p = a.updateProgress p.progressUpdateInfo
            member __.configureService (p : ContGenConfigParam) = a.configureService p
            member __.runModel m p = a.runModel (m, p)

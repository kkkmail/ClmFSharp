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

module ServiceImplementation =

    let createServiceImpl i p =
        let a = createRunner (ModelRunnerParam.defaultValue i p)
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


    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    let mutable serviceProxy : RunnerProxy =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = ContGenServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceProxy


    type ContGenService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl (ContGenSvcAccessInfo serviceAccessInfo) serviceProxy

        let initService () = ()
        do initService ()

        interface IContGenService with
            member __.getState() = a.getState().runnerState
            member __.loadQueue() = a.startQueue()
            member __.startGenerate() = a.startGenerate()
            member __.updateProgress p = a.updateProgress p
            member __.configureService (p : ContGenConfigParam) = a.configureService p
            member __.runModel m p = a.runModel (m, p)

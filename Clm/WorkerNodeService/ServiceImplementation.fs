namespace WorkerNodeService

open System
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open MessagingServiceInfo.ServiceProxy
open Argu

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessage =
        | StartWorkerNode
        | Register
        | Unregister


    type WorkerNodeServiceImpl(i : WorkerNodeServiceAccessInfo) =

        member this.onTimer() = ignore()
        member this.updateProgress (p: ProgressUpdateInfo) : unit = failwith ""


    let createServiceImpl i =
        let w = WorkerNodeServiceImpl i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.onTimer)
        do h.start()
        w


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        let notSupported a =
            let msg = (sprintf "The method %A is not supported." a)
            printfn "%s" msg
            failwith msg

        interface IContGenService with
            member __.getState() = notSupported "getState"
            member __.loadQueue() = notSupported "loadQueue"
            member __.startGenerate() = notSupported "startGenerate"
            member __.updateProgress p = w.updateProgress p
            member __.configureService (p : ContGenConfigParam) = notSupported "configureService"
            member __.runModel m p = notSupported "runModel"


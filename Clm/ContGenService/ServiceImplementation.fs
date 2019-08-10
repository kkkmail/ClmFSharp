namespace ContGenService

open System
open ClmSys.GeneralData
open ClmSys.TimerEvents
open ContGen.AsyncRun
open ContGen.Runner
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner

module ServiceImplementation =

    let createServiceImpl i =
        let a = createRunner (ModelRunnerParam.defaultValue i (RunnerProxy()))

        // Send startGenerate in case runner stops due to some reason.
        //let eventHandler _ = a.startGenerate()
        //let timer = new System.Timers.Timer(60_000.0)
        //do timer.AutoReset <- true
        //do timer.Elapsed.Add eventHandler
        //do timer.Start()
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
        {
            contGenServiceAccessInfo =
                {
                    serviceAddress = ServiceAddress DefaultContGenServiceAddress
                    servicePort = ServicePort DefaultContGenServicePort
                }

            minUsefulEe = MinUsefulEe DefaultMinEe
        }


    type ContGenService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IContGenService with
            member __.getState() = a.getState().runnerState
            member __.loadQueue() = a.startQueue()
            member __.startGenerate() = a.startGenerate()
            member __.updateProgress p = a.updateProgress (a, p)
            member __.configureService (p : ContGenConfigParam) = a.configureService p
            member __.runModel m p = a.runModel (m, p)

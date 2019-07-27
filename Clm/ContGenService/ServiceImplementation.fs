namespace ContGenService

open System
open ClmSys.GeneralData
open ContGen.AsyncRun
open ContGen.Runner
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner

module ServiceImplementation =

    let createServiceImpl i =
        let a = createRunner (ModelRunnerParam.defaultValue i (RunnerProxy()))

        // Send startGenerate in case runner stops due to some reason.
        let eventHandler _ = a.startGenerate()
        let timer = new System.Timers.Timer(60_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()

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
            serviceAccessInfo =
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
            member this.getState() = a.getState().runnerState
            member this.loadQueue() = a.startQueue()
            member this.startGenerate() = a.startGenerate()
            member this.updateProgress p = a.updateProgress (a, p)
            member this.configureService (p : ContGenConfigParam) = a.configureService p
            member this.runModel m p = a.runModel (m, p)
            //member this.getServiceAccessInfo () = failwith "ContGenService.getServiceAccessInfo is not yet imiplemented."

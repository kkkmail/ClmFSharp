﻿namespace ContGenService

open System
open ContGen.AsyncRun
open ContGen.Runner
open ContGenServiceInfo.ServiceInfo

module ServiceImplementation =

    let a = createRunner ModelRunnerParam.defaultValue

    type AsyncRunnerState
        with
        member s.runnerState : ContGenRunnerState =
            {
                generating = s.generating
                runningCount = s.runningCount
                running = s.running |> Map.toArray |> Array.map (fun (_, e) -> e)
                queue = s.queue |> List.map (fun e -> e.modelId) |> Array.ofList
                shuttingDown = s.shuttingDown
            }


    type ContGenService () =
        inherit MarshalByRefObject()

        let initService () = ()
        do initService ()

        interface IContGenService with
            member this.getState() = a.getState().runnerState
            member this.startGenerating() = a.startGenerate()

            //member this.stopGenerating()=
            //    failwith ""

            member this.progressUpdate p = a.progressUpdate p

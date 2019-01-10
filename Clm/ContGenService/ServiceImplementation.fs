namespace ContGenService

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
            }


    type ContGenService () =
        inherit MarshalByRefObject()

        let initService () = ()
        do initService ()

        interface IContGenService with
            member this.getState() = a.getState().runnerState
            member this.startGenerating() = a.startGenerate()

            member this.stopGenerating()=
                failwith ""

            member this.notifyOfProgress (p : ProgressUpdateInfo) : unit
                = failwith ""

namespace ContGenService

open System
open System.IO
open System.Diagnostics
open ProgressNotifierServiceInfo.ServiceInfo
open ContGen.AsyncRun
open ContGen.Runner
open ContGenServiceInfo.ServiceInfo
open ContGen.AsyncRun

module ServiceImplementation =

    let a = createRunner ModelRunnerParam.defaultValue

    type AsyncRunnerState
        with
        member s.runnerState : ContGenRunnerState =
            {
                generating = s.generating
                runningCount = s.runningCount
            }

    type ProgressNotifierService () =
        inherit MarshalByRefObject()

        let initService () = ()
        do initService ()

        interface IProgressNotifier with
            member this.notifyOfProgress (p : ProgressUpdateInfo) : unit =
                a.progressUpdate p


    type ContGenService () =
        inherit MarshalByRefObject()

        let initService () = ()
        do initService ()

        interface IContGenService with
            member this.getState() = a.getState().runnerState
            member this.startGenerating() = a.startGenerate()

            member this.stopGenerating()=
                failwith ""

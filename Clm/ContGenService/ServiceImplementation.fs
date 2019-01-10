namespace ContGenService

open System
open System.IO
open System.Diagnostics
open ProgressNotifier.Interfaces
open ContGen.AsyncRun
open ContGen.Runner
open ContGenService.ContGenServiceInfo
open ContGen.AsyncRun

module ServiceImplementation =
    let a = createRunner ModelRunnerParam.defaultValue

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
            member this.getState() : AsyncRunnerState = a.getState()
            member this.startGenerating() = a.startGenerate()

            member this.stopGenerating()=
                failwith ""

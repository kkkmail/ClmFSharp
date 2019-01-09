namespace ContGenService

open System
open System.IO
open System.Diagnostics
open ProgressNotifier.Interfaces
open ContGen.AsyncRun
open ContGen.Runner

module ServiceImplementation =
    type ContGenService () =
        inherit MarshalByRefObject()

        let a = createRunner ModelRunnerParam.defaultValue

        let initService () =
            a.startGenerate()

        do initService ()

        interface IProgressNotifier with
            member this.notifyOfProgress (p : ProgressUpdateInfo) : unit =
                a.progressUpdate p

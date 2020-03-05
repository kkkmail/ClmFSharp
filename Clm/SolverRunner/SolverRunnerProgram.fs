open Microsoft.FSharp.Core
open Clm.CommandLine
open ClmSys
open Argu
open SolverRunner.SolverRunnerTasks
open ClmSys.SolverRunnerErrors
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    let run() =
        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
        let results = parser.Parse argv
        let usage = parser.PrintUsage()
        runSolver results usage

    try
        match run() with
        | CompletedSuccessfully -> CompletedSuccessfully
        | e ->
            SolverRunnerCriticalError.fromErrorCode argv e |> logCriticalError |> ignore
            e
    with
    | exn ->
        printfn "%s" exn.Message
        SolverRunnerCriticalError.fromExn argv exn |> logCriticalError |> ignore
        ExitErrorCodes.UnknownException

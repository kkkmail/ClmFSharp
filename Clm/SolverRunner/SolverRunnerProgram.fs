open Microsoft.FSharp.Core
open Clm.CommandLine
open ClmSys
open Argu
open SolverRunner.SolverRunnerTasks
open ClmSys.SolverRunnerErrors
open ClmSys.ExitErrorCodes
open System.Diagnostics

[<EntryPoint>]
let main argv =
    let currentProcessId =
        try
            Process.GetCurrentProcess().Id
        with
        | _ -> -1

    let logCrit e =
        SolverRunnerCriticalError.fromErrMessage argv e
        |> logCriticalError currentProcessId
        |> ignore

    let run() =
        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
        let results = parser.Parse argv
        let usage = parser.PrintUsage()
        runSolver logCrit results usage

    try
        match run() with
        | CompletedSuccessfully -> CompletedSuccessfully
        | e ->
            SolverRunnerCriticalError.fromErrorCode argv e |> logCriticalError |> ignore
            e
    with
    | exn ->
        printfn "%s" exn.Message
        SolverRunnerCriticalError.fromExn argv exn |> logCriticalError currentProcessId |> ignore
        ExitErrorCodes.UnknownException

open Microsoft.FSharp.Core
open Clm.CommandLine
open Clm
open Argu
open SolverRunner.SolverRunnerTasks


[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = "SolverRunner.exe")
        let results = parser.Parse argv
        let usage = parser.PrintUsage()
        runSolver results usage
    with
        | exn ->
            printfn "%s" exn.Message
            ExitErrorCodes.UnknownException

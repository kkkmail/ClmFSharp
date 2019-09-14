open Microsoft.FSharp.Core
open Clm.CommandLine
open ClmSys
open Argu
open SolverRunner.SolverRunnerTasks
open System
open ClmSys.GeneralData


[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
        let results = parser.Parse argv
        let usage = parser.PrintUsage()
        runSolver results usage
    with
        | exn ->
            printfn "%s" exn.Message
            ExitErrorCodes.UnknownException

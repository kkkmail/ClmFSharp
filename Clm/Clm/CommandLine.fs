namespace Clm
open Argu

module CommandLine =

    [<Literal>]

    let SolverRunnerName = "SolverRunner.exe"

    [<CliPrefix(CliPrefix.Dash)>]
    type SolverRunnerArguments =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>]  EndTime of decimal
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y0")>] TotalAmount of decimal
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-a")>]  UseAbundant of bool
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-m")>]  ModelId of int64
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-n")>]  NotifyAddress of string


    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | EndTime _ -> "specify tEnd."
                | TotalAmount _ -> "specify t0."
                | UseAbundant _ -> "specify if abundant substance is used."
                | ModelId _ -> "specify model data id to run."
                | NotifyAddress _ -> "notify specified web address about progress."

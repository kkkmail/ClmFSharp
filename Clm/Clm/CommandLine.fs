namespace Clm
open Argu
open System

module CommandLine =

    [<Literal>]
    let SolverRunnerProcessName = "SolverRunner"


    [<Literal>]
    let SolverRunnerName = "SolverRunner.exe"


    [<CliPrefix(CliPrefix.Dash)>]
    type SolverRunnerArguments =
        | [<Unique>] [<AltCommandLine("-t")>]  EndTime of decimal
        | [<Unique>] [<AltCommandLine("-y0")>] TotalAmount of decimal
        | [<Unique>] [<AltCommandLine("-a")>]  UseAbundant of bool
        | [<Unique>] [<AltCommandLine("-m")>]  ModelId of Guid
        | [<Unique>] [<AltCommandLine("-w")>]  WrkNodeId of Guid
        | [<Unique>] [<AltCommandLine("-n")>]  NotifyAddress of string
        | [<Unique>] [<AltCommandLine("-p")>]  NotifyPort of int
        | [<Unique>] [<AltCommandLine("-ee")>] MinimumUsefulEe of double
        | [<Unique>] [<AltCommandLine("-r")>]  Remote of bool
        | [<Unique>] [<AltCommandLine("-i")>]  ResultId of Guid


    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | EndTime _ -> "specify tEnd."
                | TotalAmount _ -> "specify t0."
                | UseAbundant _ -> "specify if abundant substance is used."
                | ModelId _ -> "specify model data id to run."
                | WrkNodeId _ -> "specify [optional] worker node id."
                | NotifyAddress _ -> "notify specified web address about progress. If value is not provided, then notificaion will not be performed."
                | NotifyPort _ -> "notify specified port of that web address about progress. Default will be used if value is not provided."
                | MinimumUsefulEe _ -> "minimum value of max ee to generate plots (usually we don't want to generate all plots to save space)."
                | Remote _ -> "the solver is running remotely on a machine which does not have SQL server installed."
                | ResultId _ -> "if specified use that as ResultDataId, oterwise assign automatically."

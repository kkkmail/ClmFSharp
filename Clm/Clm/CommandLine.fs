namespace Clm
open Argu

module CommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type SolverRunnerArguments =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>]  EndTime of decimal
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y0")>] TotalAmount of decimal
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-a")>]  UseAbundant of bool
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-p")>]  PlotResults of bool
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-s")>]  SaveModelSettings of bool
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-n")>]  NotifyAddress of string
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>]  SaveResultData of bool


    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | EndTime _ -> "specify tEnd."
                | TotalAmount _ -> "specify t0."
                | UseAbundant _ -> "specify if abundant substance is used."
                | PlotResults _ -> "specify if output charts to web browser."
                | SaveModelSettings _ -> "if true then saves model settings into the database."
                | NotifyAddress _ -> "notify specified web address about progress."
                | SaveResultData _ -> "if true then saves binary result data and if false then creates but does not open charts."

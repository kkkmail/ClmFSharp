namespace ClmPlot
open System.Data.SqlClient
open Argu
open DbData.Configuration
open DbData.DatabaseTypes
open Microsoft.FSharp.Core

open Analytics.Visualization


module PlotTasks =

    [<CliPrefix(CliPrefix.Dash)>]
    type GeneratePlotArgs =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>] ResultDataId of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | ResultDataId _ -> "resultDataId to use for plotting."

    and
        [<CliPrefix(CliPrefix.None)>]
        ClmPlotArguments =
            | [<Unique>] [<AltCommandLine("plot")>] GeneratePlot of ParseResults<GeneratePlotArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | GeneratePlot _ -> "generate plot."


    let generatePlot (p :list<GeneratePlotArgs>) =
        match p |> List.tryPick (fun e -> match e with | ResultDataId n -> (int64 n |> Some)) with
        | Some resultDataId ->
            use conn = new SqlConnection(ClmConnectionString)
            match tryLoadResultData conn resultDataId with
            | Some r ->
                printfn "Plotting."
                let plotter = new Plotter(PlotDataInfo.defaultValue, r)
                plotter.plotAminoAcids()
                plotter.plotTotalSubst()
                plotter.plotEnantiomericExcess()
                printfn "Completed."
                0
            | None ->
                printfn "Failed to load resultDataId: %A." resultDataId
                -1
        | None -> 
            printfn "No result data id was specified."
            -1


    type ClmPlotTask =
        | GeneratePlotTask of list<GeneratePlotArgs>

        member task.run() =
            match task with
            | GeneratePlotTask p -> generatePlot p

        static member private tryCreateGeneratePlotTask (p : list<ClmPlotArguments>) =
            p |> List.tryPick (fun e -> match e with | GeneratePlot q -> q.GetAllResults() |> GeneratePlotTask |> Some)

        static member tryCreate (p : list<ClmPlotArguments>) =
            [
                ClmPlotTask.tryCreateGeneratePlotTask
            ]
            |> List.tryPick (fun e -> e p)

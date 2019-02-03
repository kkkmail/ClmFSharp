namespace ClmPlot

open ClmSys.GeneralData
open ClmSys.Retry
open ClmSys.ExitErrorCodes
open Argu
open DbData.Configuration
open DbData.DatabaseTypes
open Microsoft.FSharp.Core

open Analytics.Visualization


module PlotTasks =

    let logError e = printfn "Error: %A." e
    let tryDbFun f = tryDbFun logError clmConnectionString f


    [<CliPrefix(CliPrefix.Dash)>]
    type GeneratePlotArgs =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>] ResultId of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | ResultId _ -> "resultDataId to use for plotting."

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
        match p |> List.tryPick (fun e -> match e with | ResultId n -> (int64 n |> Some)) with
        | Some resultDataId ->
            match tryDbFun (tryLoadResultData (ResultDataId resultDataId)) with
            | Some ro ->
                match ro with
                | Some r ->
                    printfn "Plotting."
                    let plotter = new Plotter(PlotDataInfo.defaultValue, r)
                    plotter.plotAminoAcids()
                    plotter.plotTotalSubst()
                    plotter.plotEnantiomericExcess()
                    printfn "Completed."
                    CompletedSuccessfully
                | None ->
                    printfn "Failed to load resultDataId: %A." resultDataId
                    UnknownException
            | None ->
                printfn "Database error occurred."
                DatabaseErrorOccurred
        | None -> 
            printfn "No result data id was specified."
            InvalidCommandLineArgs


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

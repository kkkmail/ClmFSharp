open Microsoft.FSharp.Core
open Argu
open ClmPlot.PlotTasks


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<ClmPlotArguments>(programName = "ClmPlot.exe")
    let results = parser.Parse argv

    match results.GetAllResults() |> ClmPlotTask.tryCreate with
    | Some task -> task.run()
    | None ->
        printfn "%s" (parser.PrintUsage())
        -1

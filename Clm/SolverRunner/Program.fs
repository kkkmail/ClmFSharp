open System
open Microsoft.FSharp.Core
open Clm.ModelInit
open Clm.Model.ModelData
open Clm.CommandLine
open OdeSolver.Solver
open Analytics.Visualization
open Argu


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = "SolverRunner.exe")
    let results = parser.Parse argv

    match results.TryGetResult EndTime, results.TryGetResult TotalAmount with
    | Some tEnd, Some y0 ->
        let a = results.GetResult (UseAbundant, defaultValue = false)
        printfn "Starting at: %A" DateTime.Now
        let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None a)
        printfn "Calling nSolve..."

        let p =
            {
                modelName = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelName
                tEnd = tEnd
                g = update
                h = getInitValues
                y0 = y0
            }

        let result = nSolve p

        match results.TryGetResult OutputResults with
        | Some v when v = true ->
            printfn "Plotting."
            let plotter = new Plotter(PlotDataInfo.defaultValue, modelDataParamsWithExtraData.getModelData(), result)
            plotter.plotAminoAcids()
            plotter.plotTotalSubst()
            plotter.plotEnantiomericExcess()
            printfn "Completed."
        | _ -> ignore()

        0
    | _ -> 
        let usage = parser.PrintUsage()
        printfn "%A" usage
        -1

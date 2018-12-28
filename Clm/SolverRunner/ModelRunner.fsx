//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Clm.ModelInit
open Clm.Model.ModelData
open Clm.OdeSolver.Solver
open Clm.OdeSolver.Visualization
open Clm.OdeSolver.ResultSerialization

//===========================================================
let useResultsFolder = false

let y0 = 10.0
let tEnd = 50000.0
let useAbundant = false
//===========================================================
printfn "Solving for n = %A, y0 = %A..." numberOfSubstances y0
printfn "Starting at: %A" DateTime.Now
//===========================================================
let p =
    {
        modelName = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelName
        tEnd = tEnd
        g = update
        h = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None useAbundant)
        y0 = y0
    }
//===========================================================
printfn "Calling nSolve..."
#time
let result = nSolve p
#time
//===========================================================
printfn "Plotting."
let plotter = new Plotter({ PlotDataInfo.defaultValue with useTempFolder = useResultsFolder |> not }, modelDataParamsWithExtraData, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Plotting is completed."
//===========================================================
//printfn "Saving results..."
//if useResultsFolder then saveResults modelDataParamsWithExtraData.modelDataParams.modelInfo.allResultsFile result
//printfn "Saving is completed."
//===========================================================

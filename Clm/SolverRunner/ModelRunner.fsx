//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Clm.ModelInit
open Model.ModelData
open OdeSolver.Solver
open OdeSolver.Visualization
//===========================================================
let useResultsFolder = false

let y0 = 100.0
let tEnd = 10000.0
let useAbundant = false
//===========================================================
printfn "Solving for n = %A, y0 = %A..." numberOfSubstances y0
printfn "Starting at: %A" DateTime.Now

let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None useAbundant)
//===========================================================
printfn "Calling nSolve..."
#time
let result = nSolve tEnd update getInitValues y0
#time
//===========================================================
printfn "Plotting."
let plotter = new Plotter({ PlotDataInfo.defaultValue with useTempFolder = useResultsFolder |> not }, modelDataParamsWithExtraData, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
//===========================================================

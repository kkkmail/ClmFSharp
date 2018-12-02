//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Clm.Model
open Model.ModelData
open OdeSolvers.Solver
open OdeSolvers.Visualization
//===========================================================
let useTempFolder = false

let y00 = 1.0
let tEnd = 10000.0
//===========================================================
//let y0 = y00 * (2.0 * (double modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids.length))
let y0 = 2.0 * y00

printfn "Solving for n = %A, y0 = %A..." numberOfSubstances y0
printfn "Starting at: %A" DateTime.Now

let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None)
//===========================================================
printfn "Calling nSolve..."
#time
let result = nSolve tEnd update getInitValues y0
#time
//===========================================================
printfn "Plotting."
let plotter = new Plotter({ PlotDataInfo.defaultValue with useTempFolder = useTempFolder }, modelDataParamsWithExtraData, result)
//plotter.plotAll()
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
//===========================================================

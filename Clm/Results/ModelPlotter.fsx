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
open Clm.Results.AllResults
//===========================================================
let useResultsFolder = true
let modelName = "20181227_001"
//===========================================================
let x = 
    allModelResults
    |> List.filter (fun e -> e.modelName = modelName)
    |> List.head

let result : OdeResult = x
//===========================================================
printfn "Plotting."
let plotter = new Plotter({ PlotDataInfo.defaultValue with useTempFolder = useResultsFolder |> not }, modelDataParamsWithExtraData, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Plotting is completed."
//===========================================================

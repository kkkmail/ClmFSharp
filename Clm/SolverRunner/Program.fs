open System
open Microsoft.FSharp.Core
open Clm.Model
open Model.ModelData
open OdeSolvers.Solver
open OdeSolvers.Visualization

[<EntryPoint>]
let main argv = 
    printfn "Input values: %A" argv

    let tEnd0 = 1.0
    let y000 = 1.0

    // This is a tough choice...
    //let y00 = y000 * (2.0 * (double modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids.length))
    let y00 = y000 * 2.0

    let tEnd, y0 = 
        match argv |> List.ofArray with 
        | [] -> tEnd0, y00
        | h :: t ->
            match Double.TryParse h with 
            | true, v -> 
                match t with 
                | [] -> v, y00
                | h1 :: _ ->
                    match Double.TryParse h1 with 
                    | true, v1 -> v, v1
                    | false, _ -> v, y00
            | false, _ -> tEnd0, y00

    printfn "Solving for n = %A, tEnd = %A, y0 = %A." numberOfSubstances tEnd y0
    printfn "Starting at: %A" DateTime.Now

    let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None)

    printfn "Calling nSolve..."
    let result = nSolve tEnd update getInitValues y0

    printfn "Plotting."
    let plotter = new Plotter(PlotDataInfo.defaultValue, modelDataParamsWithExtraData, result)
    plotter.plotAminoAcids()
    plotter.plotTotalSubst()
    plotter.plotEnantiomericExcess()
    printfn "Completed."

    0

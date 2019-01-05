open System
open Microsoft.FSharp.Core
open Clm.ModelParams
open Clm.ModelInit
open Clm.Model.ModelData
open OdeSolver.Solver
open Analytics.Visualization
open Argu


[<EntryPoint>]
let main argv = 
    printfn "Input values: %A" argv

    let tEnd0 = 1.0
    let y00 = 1000.0
    let a0 = false

    let tEnd, y0, a = 
        match argv |> List.ofArray with 
        | [] -> tEnd0, y00, a0
        | h :: t ->
            match Double.TryParse h with 
            | true, v -> 
                match t with 
                | [] -> v, y00, a0
                | h1 :: t1 ->
                    match Double.TryParse h1 with 
                    | true, v1 -> 
                        match t1 with 
                        | [] -> v, v1, a0
                        | h2 :: _ -> 
                            match Int32.TryParse h2 with 
                            | true, v2 -> v, v1, (if v2 = 1 then true else false)
                            | false, _ -> 
                                printfn "Third input parameter is invalid: '%A'." h2
                                v, v1, a0
                    | false, _ -> 
                        printfn "Second input parameter is invalid: '%A'." h1
                        v, y00, a0
            | false, _ -> 
                printfn "First input parameter is invalid: '%A'." h
                tEnd0, y00, a0

    printfn "Solving for n = %A, tEnd = %A, y0 = %A, a = %A." numberOfSubstances tEnd y0 a
    printfn "    If necessary specify run time as first parameter, initial concentration as second parameter, and usage of abundant subst as third."
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

    printfn "Plotting."
    let plotter = new Plotter(PlotDataInfo.defaultValue, modelDataParamsWithExtraData, result)
    plotter.plotAminoAcids()
    plotter.plotTotalSubst()
    plotter.plotEnantiomericExcess()
    printfn "Completed."

    0

open System
open Microsoft.FSharp.Core
open Clm.ModelInit
open Clm.Model.ModelData
open Clm.ModelParams
open Clm.CommandLine
open OdeSolver.Solver
open Analytics.Visualization
open Argu
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open System.Data.SqlClient


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

        printfn "Saving."

        /// TODO kk:20190105 - There is some duplicate code here and in plotEnantiomericExcessImpl. Consolidate.
        let maxEe =
            let noOfOutputPoints = result.t.Length - 1
            let tIdx = [ for i in 0..noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> modelDataParamsWithExtraData.getTotals result.x.[t,*])

            let d t i =
                let (l, d) = a.[t].[i]
                if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

            let getFuncData i = tIdx |> List.map (fun t -> d t i)

            [ for i in 0..(modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids.length - 1)-> i ]
            |> List.map (fun i -> getFuncData i)
            |> List.concat
            |> List.map (fun e -> abs e)
            |> List.max

        let r =
            {
                resultDataId = None
                // TODO kk:20190105 - This should be fixed from the bottom by removing model name and converting it into modelId.
                modelDataId = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelName.Replace("_", "") |> Int64.Parse

                numberOfAminoAcids = modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids
                maxPeptideLength = modelDataParamsWithExtraData.modelDataParams.modelInfo.maxPeptideLength

                aminoAcids = AminoAcid.getAminoAcids modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids
                allSubst = modelDataParamsWithExtraData.allSubst
                allInd = modelDataParamsWithExtraData.allInd
                allRawReactions = modelDataParamsWithExtraData.allRawReactions
                allReactions = modelDataParamsWithExtraData.allReactions

                y0 = decimal y0
                tEnd = decimal tEnd
                useAbundant = false // TODO kk:20190105 This should be propagated...
                x = result.x
                t = result.t
                maxEe = maxEe
            }

        use conn = new SqlConnection(ClmConnectionString)
        saveResultData r conn |> ignore

        match r.resultDataId with
        | Some v ->
            let rs =
                {
                    resultDataId = v
                    settings = failwith ""
                }

            failwith ""
        | None -> ignore()

            //SettingMap


        match results.TryGetResult PlotResults with
        | Some v when v = true ->
            printfn "Plotting."
            let plotter = new Plotter(PlotDataInfo.defaultValue, r)
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

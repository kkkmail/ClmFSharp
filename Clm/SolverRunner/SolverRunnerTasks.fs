namespace SolverRunner

open System
open Microsoft.FSharp.Core
open ClmSys.GeneralData
open ClmSys.ExitErrorCodes
open ClmSys.Retry
open Clm.SettingsExt
open Clm.ModelInit
open Clm.ModelParams
open Clm.CommandLine
open Clm.SettingsExt
open OdeSolver.Solver
open Analytics.Visualization
open Argu
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open ProgressNotifierClient.ServiceResponse
open System.Diagnostics


module SolverRunnerTasks =

    let logError e = printfn "Error: %A." e
    let tryDbFun f = tryDbFun logError clmConnectionString f


    let progressNotifier (r : ResponseHandler) (p : ProgressUpdateInfo) =
        let notify() =
            try
                printfn "Notifying of progress: %A." p
                r.progressNotifierService.updateProgress p
                printfn "...completed."
            with
                | e ->
                    printfn "Exception occurred: %A, progress: %A." e.Message p

        notify |> toAsync |> Async.Start


    type RunProgress =
        | Running of decimal
        | Completed


    let notify m svc p =
        let t =
            match p with
            | Running d -> TaskProgress.create d
            | Completed -> TaskProgress.Completed

        progressNotifier svc
            {
                updatedProcessId = Process.GetCurrentProcess().Id
                updateModelId = m
                progress = t
            }


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount, results.TryGetResult ModelId with
        | Some tEnd, Some y0, Some modelDataId ->
            let mdoo = tryDbFun (tryLoadModelData (ModelDataId modelDataId))

            match mdoo with
            | Some (Some md) ->
                // TODO kk:20190208 - This must be split into several functions.
                let modelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
                let n = ResponseHandler.tryCreate()

                let a = results.GetResult (UseAbundant, defaultValue = false)
                printfn "Starting at: %A" DateTime.Now
                let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None a)

                printfn "Calling nSolve..."
                let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId

                let p =
                    {
                        modelDataId = modelDataId
                        tEnd = double tEnd
                        g = md.modelData.modelBinaryData.calculationData.getDerivative
                        h = getInitValues
                        y0 = double y0
                        progressCallBack = n |> Option.bind (fun svc -> (fun r -> notify (ModelDataId modelDataId) svc (Running r)) |> Some)
                    }

                let result = nSolve p


                // Notify of completion just in case.
                match n with
                | Some svc -> notify (ModelDataId modelDataId) svc Completed
                | None -> ignore()

                printfn "Saving."

                /// Amino acids are in the list and time-depended values are in the array.
                /// TODO kk:20190105 - There is some duplicate code here and in plotEnantiomericExcessImpl. Consolidate.
                let maxEe, maxAverageEe =
                    let aa = [ for i in 0..(modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.numberOfAminoAcids.length - 1)-> i ]

                    let noOfOutputPoints = result.t.Length - 1
                    let tIdx = [| for i in 0..noOfOutputPoints -> i |]
                    let a = tIdx |> Array.map (fun t -> md.modelData.modelBinaryData.calculationData.getTotals result.x.[t,*])

                    let d t i =
                        let (l, d) = a.[t].[i]
                        if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

                    let getFuncData i = tIdx |> Array.map (fun t -> d t i)

                    let maxEe =
                        aa
                        |> List.map (fun i -> getFuncData i |> List.ofArray)
                        |> List.concat
                        |> List.map (fun e -> abs e)
                        |> List.max

                    let maxAverageEe =
                        aa
                        |> List.map (fun i -> getFuncData i)
                        |> List.map (fun e -> e |> Array.average)
                        |> List.map (fun e -> abs e)
                        |> List.max

                    maxEe, maxAverageEe

                let r =
                    {
                        simpleData =
                            {
                                resultDataId = None
                                modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId |> ModelDataId

                                numberOfAminoAcids = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.numberOfAminoAcids
                                maxPeptideLength = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.maxPeptideLength

                                y0 = decimal y0
                                tEnd = decimal tEnd
                                useAbundant = false // TODO kk:20190105 This should be propagated...

                                maxEe = maxEe
                                maxAverageEe = maxAverageEe
                            }

                        binaryData =
                            {
                                aminoAcids = AminoAcid.getAminoAcids modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.numberOfAminoAcids
                                allSubst = modelDataParamsWithExtraData.regularParams.allSubst
                                allInd = modelDataParamsWithExtraData.regularParams.allInd
                                allRawReactions = modelDataParamsWithExtraData.regularParams.allRawReactions
                                allReactions = modelDataParamsWithExtraData.regularParams.allReactions

                                x = result.x
                                t = result.t
                            }
                    }

                let saveResults =
                    match results.TryGetResult SaveResultData with
                    | Some v -> v
                    | None -> false // do not save binary data by default

                (saveResultData ( match saveResults with | false -> r.resultDataWithoutBinary | true -> r.resultData))
                |> tryDbFun
                |> ignore

                let plotAll show =
                    let plotter = new Plotter(PlotDataInfo.defaultValue, r)
                    plotter.plotAminoAcids show
                    plotter.plotTotalSubst show
                    plotter.plotEnantiomericExcess show

                match results.TryGetResult PlotResults with
                | Some v when v = true ->
                    printfn "Plotting."
                    plotAll true
                    printfn "Completed."
                | _ ->
                    printfn "Generating charts..."
                    plotAll false
                    printfn "Completed."

                CompletedSuccessfully
            | _ -> UnknownException // TODO kk:20190208 - return different error codes if there is a command line error or DB error.
        | _ ->
            printfn "%s" usage
            InvalidCommandLineArgs

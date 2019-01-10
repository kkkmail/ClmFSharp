namespace SolverRunner

open System
open Microsoft.FSharp.Core
open Clm
open Clm.ModelInit
open Clm.Model.ModelData
open Clm.ModelParams
open Clm.CommandLine
open Clm.SettingsExt
open OdeSolver.Solver
open Analytics.Visualization
open Argu
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open System.Data.SqlClient
open ProgressNotifier.Interfaces
open ProgressNotifierClient.ServiceResponse
open System.Diagnostics
open System.Threading.Tasks
open ProgressNotifierClient


module SolverRunnerTasks =

    /// This is a copy from AsyncRun.fs
    let doAsyncTask  (f : unit->'a) = 
         async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }


    let progressNotifier (r : ResponseHandler) (p : ProgressUpdateInfo) =
        async
            {
                return! doAsyncTask(fun () ->
                    try
                        printfn "Notifying of progress: %A." p
                        r.progressNotifierService.notifyOfProgress p
                        printfn "...completed."
                    with
                        | e ->
                            printfn "Exception occurred: %A, progress: %A." e.Message p
                )
            }
        |> Async.Start


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount with
        | Some tEnd, Some y0 ->
            let n = ResponseHandler.tryCreate()

            let a = results.GetResult (UseAbundant, defaultValue = false)
            printfn "Starting at: %A" DateTime.Now
            let getInitValues = defaultInit (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData None a)

            match results.TryGetResult SaveModelSettings with
            | Some v when v ->
                printfn "Saving model settings..."
                let settings =
                    modelDataParamsWithExtraData.modelDataParams.setValue [] []
                    |> List.map (fun e -> e.settingPath, e)
                    |> Map.ofList

                let rs =
                    {
                        modelDataId = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelDataId
                        settings = settings
                    }

                use conn = new SqlConnection(ClmConnectionString)
                saveModelSettings conn rs
            | _ -> ignore()

            printfn "Calling nSolve..."

            let p =
                {
                    modelDataId = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelDataId
                    tEnd = tEnd
                    g = update
                    h = getInitValues
                    y0 = y0
                    progressCallBack =
                        match n with
                        | Some svc ->
                            (fun r -> progressNotifier svc
                                            {
                                                updatedProcessId = Process.GetCurrentProcess().Id
                                                progress = TaskProgress.create r
                                            })
                            |> Some
                        | None -> None
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
                    modelDataId = modelDataParamsWithExtraData.modelDataParams.modelInfo.modelDataId

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

            match results.TryGetResult PlotResults with
            | Some v when v = true ->
                printfn "Plotting."
                let plotter = new Plotter(PlotDataInfo.defaultValue, r)
                plotter.plotAminoAcids()
                plotter.plotTotalSubst()
                plotter.plotEnantiomericExcess()
                printfn "Completed."
            | _ -> ignore()

            ExitErrorCodes.CompletedSuccessfully
        | _ ->
            printfn "%s" usage
            ExitErrorCodes.InvalidCommandLineArgs

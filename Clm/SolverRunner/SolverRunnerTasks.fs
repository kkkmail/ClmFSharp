namespace SolverRunner

open System
open Microsoft.FSharp.Core
open ClmSys.GeneralData
open ClmSys.ExitErrorCodes
open ClmSys.Retry
open Clm.ModelInit
open Clm.ModelParams
open Clm.CommandLine
open Clm.ChartData
open OdeSolver.Solver
open Analytics.ChartExt
open Analytics.Visualization2
open Argu
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open ProgressNotifierClient.ServiceResponse
open System.Diagnostics
open Clm.Distributions

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


    let tryGetResponseHandler (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult NotifyAddress with
        | Some address ->
            let port = results.GetResult(NotifyPort, defaultValue = ContGenServicePort)
            ResponseHandler.tryCreate { server = Some address; port = Some port }
        | None -> None


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount, results.TryGetResult ModelId with
        | Some tEnd, Some y0, Some modelDataId ->
            match tryDbFun (tryLoadModelData (ModelDataId modelDataId)) with
            | Some (Some md) ->
                let modelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
                let minUsefulEe = results.GetResult(MinUsefulEe, defaultValue = DefaultMinEe)
                let n = tryGetResponseHandler results
                let a = results.GetResult (UseAbundant, defaultValue = false)

                printfn "Starting at: %A" DateTime.Now
                let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
                let rnd = RandomValueGetter.create (Some seed)
                let getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData a)

                printfn "Calling nSolve..."
                let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
                let binaryInfo = modelDataParamsWithExtraData.binaryInfo

                let chartInitData =
                    {
                        modelDataId = modelDataId
                        defaultValueId = md.modelData.modelDataParams.modelInfo.clmDefaultValueId
                        binaryInfo = binaryInfo
                        y0 = y0
                        tEnd = tEnd
                    }

                let chartDataUpdater = new AsyncUpdater<ChartInitData, ChartSliceData, ChartData>(ChartDataUpdater(), chartInitData)

                let updateChart (t : double) (x : double[]) =
                    ChartSliceData.create binaryInfo t x
                    |> chartDataUpdater.addContent

                let p =
                    {
                        modelDataId = modelDataId.value
                        tEnd = double tEnd
                        g = md.modelData.modelBinaryData.calculationData.getDerivative
                        h = getInitValues
                        y0 = double y0
                        progressCallBack = n |> Option.bind (fun svc -> (fun r -> notify modelDataId svc (Running r)) |> Some)
                        chartCallBack = Some updateChart
                    }

                nSolve p |> ignore

                // Notify of completion just in case.
                match n with
                | Some svc -> notify modelDataId svc Completed
                | None -> ignore()

                printfn "Saving."

                let chartData = chartDataUpdater.getContent()
                let maxEe = chartData.maxEe
                let maxAverageEe = chartData.maxAverageEe

                let r =
                    {
                        resultDataId = Guid.NewGuid() |> ResultDataId
                        resultData =
                            {
                                modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId

                                y0 = decimal y0
                                tEnd = decimal tEnd
                                useAbundant = false // TODO kk:20190105 This should be propagated...

                                maxEe = maxEe
                                maxAverageEe = maxAverageEe
                            }
                    }

                r |> saveResultData |> tryDbFun |> ignore

                let plotAll show =
                    let plotter = new Plotter2(PlotDataInfo.defaultValue, chartData)
                    plotter.plotAminoAcids show
                    plotter.plotTotalSubst show
                    plotter.plotEnantiomericExcess show

                if maxEe >= minUsefulEe
                then
                    printfn "Generating plots..."
                    plotAll false
                else printfn "Value of maxEe = %A is too small. Not creating plots." maxEe

                printfn "Completed."

                CompletedSuccessfully
            | _ -> UnknownException
        | _ ->
            printfn "%s" usage
            InvalidCommandLineArgs

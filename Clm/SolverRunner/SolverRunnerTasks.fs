﻿namespace SolverRunner

open System
open Microsoft.FSharp.Core
open ClmSys.GeneralData
open ClmSys.ExitErrorCodes
open Clm.ModelInit
open Clm.ModelParams
open Clm.CommandLine
open Clm.ChartData
open OdeSolver.Solver
open Analytics.ChartExt
open Analytics.Visualization
open Argu
open ContGenServiceInfo.ServiceInfo
open ProgressNotifierClient.ServiceResponse
open System.Diagnostics
open Clm.Distributions
open Clm.CalculationData
open ServiceProxy.SolverRunner

module SolverRunnerTasks =

    let logError e = printfn "Error: %A." e


    let progressNotifier (r : ResponseHandler) (p : LocalProgressUpdateInfo) =
        let notify() =
            try
                printfn "Notifying of progress: %A." p
                r.updateLocalProgress p
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
                updatedLocalProcessId = Process.GetCurrentProcess().Id |> LocalProcessId
                updateModelId = m
                progress = t
            }


    let getSolverRunnerProxy (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult Remote with
        | None -> SolverRunnerProxyInfo.defaultValue
        | Some _ -> SolverRunnerProxyInfo.defaultRemoteValue


    let tryGetServiceInfo (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult NotifyAddress, results.TryGetResult NotifyPort with
        | Some address, Some port ->
            let ee = results.GetResult(MinimumUsefulEe, defaultValue = DefaultMinEe) |> MinUsefulEe

            match results.TryGetResult Remote with
            | None ->
                {
                    contGenServiceAccessInfo =
                        {
                            serviceAddress = ServiceAddress address
                            servicePort = ServicePort port
                        }

                    minUsefulEe = ee
                }
                |> ContGenSvcAccessInfo
            | Some _ ->
                {
                    workerNodeServiceAccessInfo =
                        {
                            serviceAddress = ServiceAddress address
                            servicePort = ServicePort port
                        }

                    minUsefulEe = ee
                }
                |> WorkerNodeSvcAccessInfo
            |> Some
        | _ -> None


    let getResponseHandler i = ResponseHandler.tryCreate i


    let getPlotDataInfo (ClmDefaultValueId df) =
        let d = PlotDataInfo.defaultValue
        { d with resultInfo = { d.resultInfo with resultLocation = d.resultInfo.resultLocation + @"\" + df.ToString().PadLeft(6, '0') } }


    type AsyncChartDataUpdater = AsyncUpdater<ChartInitData, ChartSliceData, ChartData>


    type RunSolverData =
        {
            modelDataId : ModelDataId
            modelData : ModelData
            getInitValues : double -> double[]
            y0 : double
            useAbundant : bool
            onCompleted : unit -> unit
            chartInitData : ChartInitData
            chartDataUpdater : AsyncChartDataUpdater
            progressCallBack : (decimal -> unit) option
            updateChart : double -> double[] -> unit
        }

        static member create (md : ModelData) i a y0 tEnd =
            let n = getResponseHandler i
            let modelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
            let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
            let binaryInfo = modelDataParamsWithExtraData.binaryInfo
            let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
            let rnd = RandomValueGetter.create (Some seed)

            let chartInitData =
                {
                    modelDataId = modelDataId
                    defaultValueId = md.modelData.modelDataParams.modelInfo.clmDefaultValueId
                    binaryInfo = binaryInfo
                    y0 = y0
                    tEnd = tEnd
                }

            let chartDataUpdater = new AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)
            let updateChart = fun t x -> ChartSliceData.create binaryInfo t x |> chartDataUpdater.addContent

            {
                modelDataId = modelDataId
                modelData = md
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData a)
                y0 = double y0
                useAbundant = a

                onCompleted =
                    match n with
                    | Some svc -> fun () -> notify modelDataId svc Completed
                    | None -> ignore

                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = updateChart
                progressCallBack = n |> Option.bind (fun svc -> (fun r -> notify modelDataId svc (Running r)) |> Some)
            }


    type ChartData
        with
        member cd.toEeData() =
            {
                maxEe = cd.maxEe
                maxAverageEe = cd.maxAverageEe
                maxWeightedAverageAbsEe = cd.maxWeightedAverageAbsEe
                maxLastEe = cd.maxLastEe
            }


    let getNSolveParam (d : RunSolverData) s e =
        {
            modelDataId = d.modelDataId.value
            tStart = s
            tEnd = e
            derivative = d.modelData.modelData.modelBinaryData.calculationData.getDerivative
            initialValues = d.getInitValues d.y0
            progressCallBack = d.progressCallBack
            chartCallBack = Some d.updateChart
            getEeData = (fun () -> d.chartDataUpdater.getContent().toEeData()) |> Some
        }


    let getResultAndChartData (d : RunSolverData) =
        let chartData = d.chartDataUpdater.getContent()

        let r =
            {
                resultDataId = Guid.NewGuid() |> ResultDataId
                resultData =
                    {
                        modelDataId = d.modelDataId

                        y0 = decimal d.y0
                        tEnd = decimal d.chartInitData.tEnd
                        useAbundant = d.useAbundant

                        maxEe = chartData.maxEe
                        maxAverageEe = chartData.maxAverageEe
                        maxWeightedAverageAbsEe = chartData.maxWeightedAverageAbsEe
                        maxLastEe = chartData.maxLastEe
                    }
            }

        (r, chartData)


    type PlotResultsInfo =
        {
            runSolverData : RunSolverData
            serviceAccessInfo : SolverRunnerAccessInfo
            resultDataWithId : ResultDataWithId
            chartData : ChartData
            sovlerRunnerProxy : SolverRunnerProxy
        }


    let plotAllResults (i : PlotResultsInfo) =
        let plotAll show =
            let pdi = getPlotDataInfo i.runSolverData.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let plotter = new Plotter(pdi, i.chartData)

            let plots =
                [
                    plotter.plotAminoAcids show
                    plotter.plotTotalSubst show
                    plotter.plotEnantiomericExcess show
                ]

            i.sovlerRunnerProxy.saveCharts i.resultDataWithId.resultDataId plots

        if i.resultDataWithId.resultData.maxEe >= i.serviceAccessInfo.minUsefulEe.value
        then
            printfn "Generating plots..."
            plotAll false
        else printfn "Value of maxEe = %A is too small. Not creating plots." i.resultDataWithId.resultData.maxEe


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount, results.TryGetResult ModelId, tryGetServiceInfo results with
        | Some tEnd, Some y0, Some modelDataId, Some i ->
            let p = SolverRunnerProxy(getSolverRunnerProxy results)
            match p.tryLoadModelData i (ModelDataId modelDataId) with
            | Some md ->
                printfn "Starting at: %A" DateTime.Now
                let a = results.GetResult (UseAbundant, defaultValue = false)
                let runSolverData = RunSolverData.create md i a y0 tEnd
                let nSolveParam = getNSolveParam runSolverData
                let data = nSolveParam 0.0 (double tEnd)
                let result = nSolve data

                printfn "Saving."
                let (r, chartData) = getResultAndChartData runSolverData
                r |> p.saveResultData |> ignore

                {
                    runSolverData = runSolverData
                    serviceAccessInfo = i
                    resultDataWithId = r
                    chartData = chartData
                    sovlerRunnerProxy = p
                }
                |> plotAllResults

                printfn "Completed."
                runSolverData.onCompleted()

                CompletedSuccessfully
            | _ -> UnknownException
        | _ ->
            printfn "%s" usage
            InvalidCommandLineArgs

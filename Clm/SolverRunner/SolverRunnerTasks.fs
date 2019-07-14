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
open Analytics.Visualization
open Argu
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open ProgressNotifierClient.ServiceResponse
open System.Diagnostics
open Clm.Distributions
open Clm.CalculationData

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


    let tryGetServiceInfo (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult NotifyAddress, results.TryGetResult NotifyPort with
        | Some address, Some port ->
            let ee = results.GetResult(MinimumUsefulEe, defaultValue = DefaultMinEe) |> MinUsefulEe
            Some { serviceAddress = ServiceAddress address; servicePort = ServicePort port; minUsefulEe = ee }
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


    let plotAllResults (d : RunSolverData) (i : ServiceAccessInfo) (r : ResultDataWithId) chartData =
        let plotAll show =
            let pdi = getPlotDataInfo d.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let plotter = new Plotter(pdi, chartData)
            plotter.plotAminoAcids show
            plotter.plotTotalSubst show
            plotter.plotEnantiomericExcess show

        if r.resultData.maxEe >= i.minUsefulEe.value
        then
            printfn "Generating plots..."
            plotAll false
        else printfn "Value of maxEe = %A is too small. Not creating plots." r.resultData.maxEe


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount, results.TryGetResult ModelId, tryGetServiceInfo results with
        | Some tEnd, Some y0, Some modelDataId, Some i ->
            match tryDbFun (tryLoadModelData i (ModelDataId modelDataId)) |> Option.bind id with
            | Some md ->
                printfn "Starting at: %A" DateTime.Now
                let a = results.GetResult (UseAbundant, defaultValue = false)
                let runSolverData = RunSolverData.create md i a y0 tEnd
                let nSolveParam = getNSolveParam runSolverData
                let data = nSolveParam 0.0 (double tEnd)
                let result = nSolve data
                runSolverData.onCompleted()

                printfn "Saving."
                let (r, chartData) = getResultAndChartData runSolverData
                r |> saveResultData |> tryDbFun |> ignore

                plotAllResults runSolverData i r chartData
                printfn "Completed."

                CompletedSuccessfully
            | _ -> UnknownException
        | _ ->
            printfn "%s" usage
            InvalidCommandLineArgs

namespace SolverRunner

open Microsoft.FSharp.Core
open ClmSys.GeneralData
open Clm.ModelInit
open Clm.ModelParams
open Clm.ChartData
open OdeSolver.Solver
open Analytics.ChartExt
open Analytics.Visualization
open ContGenServiceInfo.ServiceInfo
open Clm.Distributions
open Clm.CalculationData
open ServiceProxy.SolverRunner
open System.IO
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo
open ClmSys.SolverRunnerErrors
open ClmSys.GeneralPrimitives

module SolverRunnerTasks =

    let notify progressNotifier r t =
        progressNotifier
            {
                runQueueId = r
                progress = t
            }


    let getPlotDataInfo (df : ClmDefaultValueId) =
        let d = PlotDataInfo.defaultValue
        { d with resultInfo = { d.resultInfo with resultLocation = Path.Combine(d.resultInfo.resultLocation, df.ToString()) } }


    type AsyncChartDataUpdater = AsyncUpdater<ChartInitData, ChartSliceData, ChartData>


    type RunSolverData =
        {
            modelDataId : ModelDataId
            modelData : ModelData
            getInitValues : double -> double[]
            y0 : double
            useAbundant : bool
            onCompleted : unit -> UnitResult
            onFailed : (ErrorMessage -> UnitResult)
            chartInitData : ChartInitData
            chartDataUpdater : AsyncChartDataUpdater
            progressCallBack : (decimal -> UnitResult) option
            updateChart : double -> double[] -> unit
            noOfProgressPoints : int option
            minUsefulEe : MinUsefulEe
        }


        static member create (w : WorkerNodeRunModelData) n pp =
            let modelDataParamsWithExtraData = w.modelData.modelData.getModelDataParamsWithExtraData()
            let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
            let binaryInfo = modelDataParamsWithExtraData.binaryInfo
            let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
            let rnd = RandomValueGetter.create (Some seed)
            let defaultValueId = w.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let commandLineParams = w.runningProcessData.commandLineParams

            let r =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    runQueueId = w.runningProcessData.runQueueId
                    workerNodeId = w.runningProcessData.workerNodeId
                    commandLineParams = commandLineParams
                }

            let chartInitData =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    binaryInfo = binaryInfo
                    y0 = commandLineParams.y0
                    tEnd = commandLineParams.tEnd
                }

            let chartDataUpdater = new AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)
            let updateChart = fun t x -> ChartSliceData.create binaryInfo t x |> chartDataUpdater.addContent

            {
                modelDataId = modelDataId
                modelData = w.modelData
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData commandLineParams.useAbundant)
                y0 = double commandLineParams.y0
                useAbundant = commandLineParams.useAbundant
                onCompleted = fun () -> notify n r.runQueueId Completed
                onFailed = fun e -> notify n r.runQueueId (Failed e)
                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = updateChart
                progressCallBack = (fun p -> notify n r.runQueueId (TaskProgress.create p)) |> Some
                noOfProgressPoints = pp
                minUsefulEe = w.minUsefulEe
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
            noOfOutputPoints = None
            noOfProgressPoints = d.noOfProgressPoints
        }


    let getResultAndChartData rdi w (d : RunSolverData) =
        let chartData = d.chartDataUpdater.getContent()

        let r =
            {
                resultDataId = rdi
                workerNodeId = w
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
            resultDataWithId : ResultDataWithId
            chartData : ChartData
        }


    let plotAllResults (i : PlotResultsInfo) =
        let plotAll () =
            let pdi = getPlotDataInfo i.runSolverData.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let plotter = new Plotter(pdi, i.chartData)

            {
                resultDataId = i.resultDataWithId.resultDataId
                defaultValueId = i.chartData.initData.defaultValueId
                charts =
                    [
                        plotter.getAminoAcids ()
                        plotter.getTotalSubst ()
                        plotter.getEnantiomericExcess ()
                    ]
            }
            |> GeneratedCharts
    
        if i.resultDataWithId.resultData.maxEe >= i.runSolverData.minUsefulEe.value
        then plotAll ()
        else NotGeneratedCharts


    /// Uncomment printfn below in case of severe issues.
    /// Then run ContGenService and WorkerNodeService as EXE with redirect into dump files.
    let runSolver (proxy : SolverRunnerProxy) (w : WorkerNodeRunModelData) : unit =
        let logIfFailed result =
            match result with
            | Ok() -> ignore()
            | Error e -> SolverRunnerCriticalError.fromErrMessage (e.ToString()) |> proxy.logCrit |> ignore

        try
            let runSolverData = RunSolverData.create w proxy.updateProgress None
            let nSolveParam = getNSolveParam runSolverData
            let data = nSolveParam 0.0 (double w.runningProcessData.commandLineParams.tEnd)

            printfn "runSolver: Calling nSolve for runQueueId = %A, modelDataId = %A..." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
            nSolve data |> ignore
            printfn "runSolver: ...call to nSolve for runQueueId = %A, modelDataId = %A is completed." w.runningProcessData.runQueueId w.runningProcessData.modelDataId

            let (r, chartData) = getResultAndChartData (w.runningProcessData.runQueueId.toResultDataId()) w.runningProcessData.workerNodeId runSolverData
            let result = proxy.saveResult r

            let chartResult =
                {
                    runSolverData = runSolverData
                    resultDataWithId = r
                    chartData = chartData
                }
                |> plotAllResults
                |> proxy.saveCharts

            printfn "runSolver: Notifying of completion for runQueueId = %A, modelDataId = %A..." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
            let completedResult =
                {
                    runQueueId = w.runningProcessData.runQueueId
                    progress = Completed
                }
                |> proxy.updateProgress

            foldUnitResults [ result; chartResult; completedResult ] |> logIfFailed
            printfn "runSolver: All completed for runQueueId = %A, modelDataId = %A is completed." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
        with
        | e ->
            {
                runQueueId = w.runningProcessData.runQueueId
                progress = e.ToString() |> ErrorMessage |> Failed
            }
            |> proxy.updateProgress
            |> logIfFailed

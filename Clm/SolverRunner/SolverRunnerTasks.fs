namespace SolverRunner

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
open WorkerNodeServiceInfo.ServiceInfo
open System.IO
open ClmSys.Retry
open ClmSys.SolverRunnerData
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodeData
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open NoSql.FileSystemTypes
open ClmSys.Registry
open ClmSys.ClmErrors
open ClmSys.Rop

module SolverRunnerTasks =

    let getErrName processId =
        "SolverRunnerErr\\" + processId.ToString().PadLeft(6, '0') |> MessagingClientName

    let logError e = printfn "Error: %A." e
    let logCriticalError processId = saveSolverRunnerErrFs (getErrName processId)


    let progressNotifier (r : WorkerNodeResponseHandler) (p : LocalProgressUpdateInfo) =
        let result =
            try
                printfn "Notifying of progress: %A." p

                match tryFun (fun () -> r.updateLocalProgress p) with
                | Ok (Ok()) -> Ok()
                | Ok (Error e) ->
                    printfn "Internal error occurred: %A" e
                    Error e
                | Error ex ->
                    printfn "Error occurred: %A" ex
                    ("SolverRunnerTasks.progressNotifier", ex) |> UnhandledExn |> Error
            with
            | e ->
                printfn "Exception occurred: %A, progress: %A." e.Message p
                ("SolverRunnerTasks.progressNotifier", e) |> UnhandledExn |> Error

        result


    let notify (r : RunningProcessData) (svc : WorkerNodeResponseHandler) (t : TaskProgress) =
        progressNotifier svc
            {
                localProcessId = Process.GetCurrentProcess().Id |> LocalProcessId
                runningProcessData = r
                progress = t
            }


    let getSolverRunnerProxy (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult Remote with
        | None | Some false -> SolverRunnerProxyInfo.defaultValue
        | Some true -> SolverRunnerProxyInfo.defaultRemoteValue


    let tryGetServiceInfo (results : ParseResults<SolverRunnerArguments>) =
        match results.TryGetResult NotifyAddress, results.TryGetResult NotifyPort with
        | Some address, Some port ->
            let ee = results.GetResult(MinimumUsefulEe, defaultValue = DefaultMinEe) |> MinUsefulEe

            {
                nodeServiceAccessInfo =
                    {
                        serviceAddress = ServiceAddress address
                        servicePort = ServicePort port
                        inputServiceName = WorkerNodeServiceName
                    }

                minUsefulEe = ee
            }
            |> Some
        | _ -> None


    let getResponseHandler i = WorkerNodeResponseHandler.tryCreate i


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
            onCompleted : ChartGenerationResult -> UnitResult
            onFailed : (string -> UnitResult)
            chartInitData : ChartInitData
            chartDataUpdater : AsyncChartDataUpdater
            progressCallBack : (decimal -> UnitResult) option
            updateChart : double -> double[] -> unit
            noOfProgressPoints : int option
        }


        static member create (md : ModelData) i (c : ModelCommandLineParam) (d : RunQueueId) w pp =
            let n = getResponseHandler i
            let modelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
            let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
            let binaryInfo = modelDataParamsWithExtraData.binaryInfo
            let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
            let rnd = RandomValueGetter.create (Some seed)
            let defaultValueId = md.modelData.modelDataParams.modelInfo.clmDefaultValueId

            let r =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    runQueueId = d
                    workerNodeId = w
                    commandLineParams = c
                }

            let chartInitData =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    binaryInfo = binaryInfo
                    y0 = c.y0
                    tEnd = c.tEnd
                }

            let chartDataUpdater = new AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)
            let updateChart = fun t x -> ChartSliceData.create binaryInfo t x |> chartDataUpdater.addContent

            {
                modelDataId = modelDataId
                modelData = md
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData c.useAbundant)
                y0 = double c.y0
                useAbundant = c.useAbundant

                onCompleted =
                    match n with
                    | Some svc -> fun c -> notify r svc (Completed c)
                    | None ->  fun _ -> Ok()

                onFailed =
                    match n with
                    | Some svc -> fun e -> notify r svc (Failed (w, d.toRemoteProcessId()))
                    | None -> fun _ -> Ok()

                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = updateChart
                progressCallBack = n |> Option.bind (fun svc -> (fun p -> notify r svc (TaskProgress.create p)) |> Some)
                noOfProgressPoints = pp
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
            serviceAccessInfo : NodeServiceAccessInfo
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

            match ChartInfo.tryCreate i.resultDataWithId.resultDataId i.chartData.initData.defaultValueId plots with
            | Ok c -> i.sovlerRunnerProxy.saveChartInfo c |> mapSuccessValue GeneratedCharts
            | Error e ->
                printfn "Unable to create ChartInfo for resultDataId: %A, plots: %A, error: %A" i.resultDataWithId.resultDataId plots e
                Error e

        if i.resultDataWithId.resultData.maxEe >= i.serviceAccessInfo.minUsefulEe.value
        then
            printfn "Generating plots..."
            plotAll false
        else
            printfn "Value of maxEe = %A is too small. Not creating plots." i.resultDataWithId.resultData.maxEe
            Ok NotGeneratedCharts


    let runSolver (logCrit : string -> unit) (results : ParseResults<SolverRunnerArguments>) usage =
        match results.TryGetResult EndTime, results.TryGetResult TotalAmount, results.TryGetResult ModelId, tryGetServiceInfo results, results.TryGetResult ResultId, results.TryGetResult WrkNodeId with
        | Some tEnd, Some y0, Some modelDataId, Some i, Some d, Some g ->
            let p = SolverRunnerProxy.create (getSolverRunnerProxy results)
            match p.loadModelData i (ModelDataId modelDataId) with
            | Ok md ->
                printfn "Starting at: %A" DateTime.Now
                let a = results.GetResult (UseAbundant, defaultValue = false)

                let c =
                    {
                        tEnd = tEnd
                        y0 = y0
                        useAbundant = a
                    }

                let w = g |> MessagingClientId |> WorkerNodeId
                let pp = results.TryGetResult ProgrNotifPoints
                let runSolverData = RunSolverData.create md i c (RunQueueId d) w pp

                try
                    let nSolveParam = getNSolveParam runSolverData
                    let data = nSolveParam 0.0 (double tEnd)
                    nSolve data |> ignore

                    printfn "Saving."
                    let (r, chartData) = getResultAndChartData (ResultDataId d) w runSolverData

                    let chartResult =
                        {
                            runSolverData = runSolverData
                            serviceAccessInfo = i
                            resultDataWithId = r
                            chartData = chartData
                            sovlerRunnerProxy = p
                        }
                        |> plotAllResults

                    let result = r |> p.saveResultData
                    printfn "Completed."

                    let onCompleted c =
                        match runSolverData.onCompleted c with
                        | Ok() -> ignore()
                        | Error e -> logCrit (sprintf "%A" e)

                    match chartResult, result with
                    | Ok c, Ok() -> onCompleted c
                    | Ok c, Error e ->
                        logCrit (sprintf "%A" e)
                        onCompleted c
                    | Error e, Ok() ->
                        logCrit (sprintf "%A" e)
                        onCompleted NotGeneratedCharts
                    | Error e1, Error e2 ->
                        logCrit (sprintf "%A" (e1 + e2))
                        onCompleted NotGeneratedCharts

                    CompletedSuccessfully
                with
                | ex ->
                    printfn "Failed!"

                    match runSolverData.onFailed (ex.ToString()) with
                    | Ok() -> ignore()
                    | Error e -> logCrit (sprintf "ERROR: %A, EXCEPTION: %A" e ex)

                    UnknownException
            | _ ->
                let msg = sprintf "Unable to load model with id: %A" modelDataId
                printfn "%s" msg
                logCrit msg
                UnknownException
        | _ ->
            printfn "%s" usage
            logCrit usage
            InvalidCommandLineArgs

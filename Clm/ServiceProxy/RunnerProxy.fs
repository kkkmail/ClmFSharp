namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData

module Runner =

    let getCommandLine (p : RunModelParam) r =
        let data =
            {
                modelDataId = p.callBackInfo.modelDataId
                resultDataId = p.callBackInfo.runQueueId.toResultDataId()
                workerNodeId = p.callBackInfo.workerNodeId
                minUsefulEe = p.commandLineParam.serviceAccessInfo.minUsefulEe
                remote = r
            }

        let commandLineParams = p.commandLineParam.toCommandLine data
        commandLineParams


    let runLocalModel (p : RunModelParam) r =
        let fullExeName = getExeName p.exeName
        let commandLineParams = getCommandLine p r
        printfn "runModel::commandLineParams = %A\n" commandLineParams
        runProc p.callBackInfo fullExeName commandLineParams None


    type LocalRunnerConfig =
        {
            connectionString : ConnectionString
        }

        static member defaultValue =
            {
                connectionString = clmConnectionString
            }


    type PartitionerRunnerConfig =
        {
            connectionString : ConnectionString
            runModel : RunModelParam -> ProcessStartedResult
        }

        static member defaultValue r =
            {
                connectionString = clmConnectionString
                runModel = r
            }


    type RunnerProxyInfo =
        | LocalRunnerProxy of LocalRunnerConfig
        | PartitionerRunnerProxy of PartitionerRunnerConfig

        static member defaultValue = LocalRunnerProxy LocalRunnerConfig.defaultValue


    type RunnerProxy =
        {
            tryLoadClmDefaultValue : ClmDefaultValueId -> ClmDefaultValue option
            tryUpdateModelData : ModelData -> bool option
            saveRunQueueEntry : ModelDataId -> ClmDefaultValueId -> ModelCommandLineParam -> RunQueueId option
            tryUpdateClmTask : ClmTask -> bool option
            addClmTask : ClmTask -> ClmTask option
            tryLoadClmTask : SolverRunnerAccessInfo -> ClmTaskId -> ClmTask option
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> ModelData option
            loadIncompleteClmTasks : SolverRunnerAccessInfo -> list<ClmTask> option
            loadRunQueue : SolverRunnerAccessInfo -> list<RunQueue> option
            deleteRunQueueEntry : RunQueueId -> unit option
            runModel : RunModelParam -> ProcessStartedResult
        }


        static member create (i : RunnerProxyInfo) =
            let logError e = printfn "Error: %A" e
            let tryDbFun c f = tryDbFun logError c f

            let connectionString =
                match i with
                | LocalRunnerProxy c -> c.connectionString
                | PartitionerRunnerProxy c -> c.connectionString

            let tryLoadClmDefaultValueImpl d = tryDbFun connectionString (tryLoadClmDefaultValue d) |> Option.bind id
            let tryUpdateModelDataImpl m = tryDbFun connectionString (tryUpdateModelData m)
            let saveRunQueueEntryImpl modelId d p = tryDbFun connectionString (saveRunQueueEntry modelId d p)
            let tryUpdateClmTaskImpl a = tryDbFun connectionString (tryUpdateClmTask a)
            let addClmTaskImpl a = tryDbFun connectionString (addClmTask a)
            let tryLoadClmTaskImpl a t = tryDbFun connectionString (tryLoadClmTask a t) |> Option.bind id
            let tryLoadModelDataImpl a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id
            let loadIncompleteClmTasksImpl a = tryDbFun connectionString (loadIncompleteClmTasks a)
            let loadRunQueueImpl a = tryDbFun connectionString (loadRunQueue a)
            let deleteRunQueueEntryImpl runQueueId = tryDbFun connectionString (deleteRunQueueEntry runQueueId)

            let runModelImpl (p : RunModelParam) : ProcessStartedResult =
                printfn "RunnerProxy.runModelImpl: p = %A, i = %A" p i
                match i with
                | LocalRunnerProxy _ ->
                    match runLocalModel p false with
                    | Some e -> e.toProcessStartedInfo() |> StartedSuccessfully
                    | None -> FailedToStart
                | PartitionerRunnerProxy c -> c.runModel p

            {
                tryLoadClmDefaultValue = tryLoadClmDefaultValueImpl
                tryUpdateModelData = tryUpdateModelDataImpl
                saveRunQueueEntry = saveRunQueueEntryImpl
                tryUpdateClmTask = tryUpdateClmTaskImpl
                addClmTask = addClmTaskImpl
                tryLoadClmTask = tryLoadClmTaskImpl
                tryLoadModelData = tryLoadModelDataImpl
                loadIncompleteClmTasks = loadIncompleteClmTasksImpl
                loadRunQueue = loadRunQueueImpl
                deleteRunQueueEntry = deleteRunQueueEntryImpl
                runModel = runModelImpl
            }

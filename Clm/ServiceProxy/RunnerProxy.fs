namespace ServiceProxy

open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open ClmSys
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverRunnerData

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
            loadClmDefaultValue : ClmDefaultValueId -> ClmResult<ClmDefaultValue>
            updateModelData : ModelData -> UnitResult
            saveRunQueue : ModelDataId -> ClmDefaultValueId -> ModelCommandLineParam -> ClmResult<RunQueueId>
            updateClmTask : ClmTask -> UnitResult
            addClmTask : ClmTask -> ClmResult<ClmTask>
            loadClmTask : SolverRunnerAccessInfo -> ClmTaskId -> ClmResult<ClmTask>
            loadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            loadIncompleteClmTasks : SolverRunnerAccessInfo -> ListResult<ClmTask>
            loadRunQueue : SolverRunnerAccessInfo -> ListResult<RunQueue>
            deleteRunQueue : RunQueueId -> UnitResult
            runModel : RunModelParam -> ProcessStartedResult
        }


        static member create (i : RunnerProxyInfo) =
            let connectionString =
                match i with
                | LocalRunnerProxy c -> c.connectionString
                | PartitionerRunnerProxy c -> c.connectionString

            let runModelImpl (p : RunModelParam) : ProcessStartedResult =
                printfn "RunnerProxy.runModelImpl: p = %A, i = %A" p i
                match i with
                | LocalRunnerProxy _ ->
                    match runLocalModel p false with
                    | Ok r -> StartedSuccessfully(r.toProcessStartedInfo(), None) |> Ok
                    | Error e -> e |> ProcessStartedErr |> Error
                | PartitionerRunnerProxy c -> c.runModel p

            {
                loadClmDefaultValue = loadClmDefaultValue connectionString
                updateModelData = updateModelData connectionString
                saveRunQueue = saveRunQueue connectionString
                updateClmTask = updateClmTask connectionString
                addClmTask = addClmTask connectionString
                loadClmTask = loadClmTask connectionString
                loadModelData = loadModelData connectionString
                loadIncompleteClmTasks = loadIncompleteClmTasks connectionString
                loadRunQueue = loadRunQueue connectionString
                deleteRunQueue = deleteRunQueue connectionString
                runModel = runModelImpl
            }

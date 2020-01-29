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
            tryLoadClmDefaultValue : ClmDefaultValueId -> ClmResult<ClmDefaultValue>
            tryUpdateModelData : ModelData -> UnitResult
            saveRunQueueEntry : ModelDataId -> ClmDefaultValueId -> ModelCommandLineParam -> ClmResult<RunQueueId>
            tryUpdateClmTask : ClmTask -> UnitResult
            addClmTask : ClmTask -> ClmResult<ClmTask>
            tryLoadClmTask : SolverRunnerAccessInfo -> ClmTaskId -> ClmResult<ClmTask>
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            loadIncompleteClmTasks : SolverRunnerAccessInfo -> ListResult<ClmTask>
            loadRunQueue : SolverRunnerAccessInfo -> ListResult<RunQueue>
            deleteRunQueueEntry : RunQueueId -> UnitResult
            runModel : RunModelParam -> ProcessStartedResult
        }


        static member create (i : RunnerProxyInfo) =
            let connectionString =
                match i with
                | LocalRunnerProxy c -> c.connectionString
                | PartitionerRunnerProxy c -> c.connectionString

            let runModelImpl (p : RunModelParam) =
                printfn "RunnerProxy.runModelImpl: p = %A, i = %A" p i
                match i with
                | LocalRunnerProxy _ -> (runLocalModel p false) |> Rop.bind (fun e -> e.toProcessStartedInfo() |> Ok)
                | PartitionerRunnerProxy c -> c.runModel p

            {
                tryLoadClmDefaultValue = tryLoadClmDefaultValue connectionString
                tryUpdateModelData = tryUpdateModelData connectionString
                saveRunQueueEntry = saveRunQueueEntry connectionString
                tryUpdateClmTask = tryUpdateClmTask connectionString
                addClmTask = addClmTask connectionString
                tryLoadClmTask = tryLoadClmTask connectionString
                tryLoadModelData = tryLoadModelData connectionString
                loadIncompleteClmTasks = loadIncompleteClmTasks connectionString
                loadRunQueue = loadRunQueue connectionString
                deleteRunQueueEntry = deleteRunQueueEntry connectionString
                runModel = runModelImpl
            }

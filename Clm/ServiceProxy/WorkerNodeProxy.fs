namespace ServiceProxy

open NoSql.FileSystemTypes
open ServiceProxy.Runner
open ClmSys.Registry
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralErrors
open ClmSys.GeneralPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerData


module WorkerNodeProxy =

    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


    type WorkerNodeProxyInfo =
        {
            //storageType : StorageType
            dummy : int
        }

        static member defaultValue =
            {
                //storageType = RemoteStorage
                dummy = 0
            }

    type WorkerNodeProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            tryLoadWorkerNodeRunModelData : RemoteProcessId -> ClmResult<WorkerNodeRunModelData>
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            runModel : RunModelParam ->  Result<LocalProcessStartedInfo, ProcessStartedError>
            getCommandLine : RunModelParam -> string
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>

            saveModelData : ModelData -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult

            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId>
            tryDeleteResultData : ResultDataId -> UnitResult
            loadAllResultData : unit -> ListResult<ResultDataWithId>

            tryLoadChartInfo : ResultDataId -> ClmResult<ChartInfo>
            tryDeleteChartInfo : ResultDataId -> UnitResult
        }

        static member create (i : WorkerNodeProxyInfo) =
            let name = workerNodeServiceName

            {
                saveWorkerNodeRunModelData = trySaveWorkerNodeRunModelDataFs name
                tryLoadWorkerNodeRunModelData = tryLoadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                runModel = fun p -> runLocalModel p true
                getCommandLine = fun p -> getCommandLine p true
                loadAllWorkerNodeRunModelData = tryLoadWorkerNodeRunModelDataAllFs name

                // These ones are needed for SolverRunner interop.
                // Note that the "name" is different here.
                saveModelData = trySaveModelDataFs solverRunnerName
                tryDeleteModelData = tryDeleteModelDataFs solverRunnerName

                tryLoadResultData = tryLoadResultDataFs solverRunnerName
                tryDeleteResultData = tryDeleteResultDataFs solverRunnerName
                loadAllResultData = tryLoadResultDataAllFs solverRunnerName

                tryLoadChartInfo = tryLoadChartInfoFs solverRunnerName
                tryDeleteChartInfo = tryDeleteChartInfoFs solverRunnerName
            }

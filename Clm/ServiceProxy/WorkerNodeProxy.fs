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
            loadWorkerNodeRunModelData : RemoteProcessId -> ClmResult<WorkerNodeRunModelData>
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            runModel : RunModelParam ->  Result<LocalProcessStartedInfo, ProcessStartedError>
            getCommandLine : RunModelParam -> string
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>

            saveModelData : ModelData -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult

            loadResultData : ResultDataId -> ClmResult<ResultDataWithId>
            tryDeleteResultData : ResultDataId -> UnitResult
            loadAllResultData : unit -> ListResult<ResultDataWithId>

            loadChartInfo : ResultDataId -> ClmResult<ChartInfo>
            tryDeleteChartInfo : ResultDataId -> UnitResult
        }

        static member create (i : WorkerNodeProxyInfo) =
            let name = workerNodeServiceName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                runModel = fun p -> runLocalModel p true
                getCommandLine = fun p -> getCommandLine p true
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name

                // These ones are needed for SolverRunner interop.
                // Note that the "name" is different here.
                saveModelData = saveModelDataFs solverRunnerName
                tryDeleteModelData = tryDeleteModelDataFs solverRunnerName

                loadResultData = loadResultDataFs solverRunnerName
                tryDeleteResultData = tryDeleteResultDataFs solverRunnerName
                loadAllResultData = loadResultDataAllFs solverRunnerName

                loadChartInfo = loadChartInfoFs solverRunnerName
                tryDeleteChartInfo = tryDeleteChartInfoFs solverRunnerName
            }

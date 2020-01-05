namespace ServiceProxy

open ClmSys.Retry
open NoSql.FileSystemTypes
open ServiceProxy.Runner
open ClmSys.GeneralData
open ClmSys.Registry
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralErrors


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
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> Result<unit, ClmError>
            tryLoadWorkerNodeRunModelData : RemoteProcessId ->  Result<WorkerNodeRunModelData, ClmError>
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> Result<unit, ClmError>
            runModel : RunModelParam ->  Result<LocalProcessStartedInfo, ProcessStartedError>
            getCommandLine : RunModelParam -> string
            loadAllWorkerNodeRunModelData : unit -> Result<list<Result<WorkerNodeRunModelData, ClmError>>, ClmError>

            saveModelData : ModelData -> Result<unit, ClmError>
            tryDeleteModelData : ModelDataId -> Result<unit, ClmError>

            tryLoadResultData : ResultDataId -> Result<ResultDataWithId, ClmError>
            tryDeleteResultData : ResultDataId -> Result<unit, ClmError>
            loadAllResultData : unit -> Result<list<Result<ResultDataWithId, ClmError>>, ClmError>

            tryLoadChartInfo : ResultDataId -> Result<ChartInfo, ClmError>
            tryDeleteChartInfo : ResultDataId -> Result<unit, ClmError>
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

namespace ServiceProxy

open NoSql.FileSystemTypes
open ClmSys.Registry
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralErrors
open ClmSys.GeneralPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralData
open SolverRunner
open ClmSys.SolverRunnerErrors

module WorkerNodeProxy =

    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


    type WorkerNodeProxyData =
        {
            minUsefulEe : MinUsefulEe
            noOfProgressPoints : int option
        }

        static member defaultValue =
            {
                minUsefulEe = MinUsefulEe.defaultValue
                noOfProgressPoints = None
            }

    type WorkerNodeProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            loadWorkerNodeRunModelData : RunQueueId -> ClmResult<WorkerNodeRunModelData>
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            logCrit : SolverRunnerCriticalError -> UnitResult
        }

        static member create (i : WorkerNodeProxyData) =
            let name = workerNodeServiceName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name
                logCrit = saveSolverRunnerErrFs name
            }

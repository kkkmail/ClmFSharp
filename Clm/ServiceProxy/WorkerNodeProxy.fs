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
            //runModel : SolverRunnerProxy -> WorkerNodeRunModelData -> unit
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
        }

        static member create (i : WorkerNodeProxyData) =
            let name = workerNodeServiceName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                //runModel = fun p -> runLocalModel p true i.minUsefulEe i.noOfProgressPoints
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name
            }

namespace ServiceProxy

open Clm.ModelParams
open Clm.CalculationData
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.Registry
open PartitionerServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverRunnerData
open ClmSys.WorkerNodePrimitives
open Clm.Generator.ClmModelData
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData

module ModelRunnerProxy =

    type RunModelProxy =
        {
            minUsefulEe : MinUsefulEe
            sendRunModelMessage : MessageInfo -> UnitResult
            loadModelData : ModelDataId -> ClmResult<ModelData>
        }


    type TryRunFirstModelProxy =
        {
            tryLoadFirstRunQueue : unit -> ClmResult<RunQueue option>
            tryGetAvailableWorkerNode : unit -> ClmResult<WorkerNodeId option>
            runModel : RunQueue -> UnitResult
            upsertRunQueue : RunQueue -> UnitResult
        }


    type TryRunModelResult =
        | WorkScheduled
        | NoWork
        | NoAvailableWorkerNodes


    type TryRunAllModelsProxy =
        {
            tryRunFirstModel : unit -> ClmResult<TryRunModelResult>
        }


    type UpdateProgressProxy =
        {
            tryLoadRunQueue : RunQueueId -> ClmResult<RunQueue option>
            upsertRunQueue : RunQueue -> UnitResult
        }


    type RegisterProxy =
        {
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }


    type UnregisterProxy =
        {
            loadWorkerNodeInfo : WorkerNodeId -> ClmResult<WorkerNodeInfo>
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }

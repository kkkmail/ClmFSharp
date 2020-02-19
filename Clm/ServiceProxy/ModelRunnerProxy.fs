﻿namespace ServiceProxy

open Clm.ModelParams
open Clm.CalculationData
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverRunnerData
open ClmSys.WorkerNodePrimitives
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo

module ModelRunnerProxy =

    type RunModelProxy =
        {
            minUsefulEe : MinUsefulEe
            sendRunModelMessage : MessageInfo -> UnitResult
            loadModelData : ModelDataId -> ClmResult<ModelData>
        }

        static member create c e s =
            {
                minUsefulEe = e
                sendRunModelMessage = s
                loadModelData = loadModelData c
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

        static member create c =
            {
                tryLoadRunQueue = tryLoadRunQueue c
                upsertRunQueue = upsertRunQueue c
            }


    type RegisterProxy =
        {
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }

        static member create c =
            {
                upsertWorkerNodeInfo = upsertWorkerNodeInfo c
            }


    type UnregisterProxy =
        {
            loadWorkerNodeInfo : WorkerNodeId -> ClmResult<WorkerNodeInfo>
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }
        static member create c =
            {
                loadWorkerNodeInfo = loadWorkerNodeInfo c
                upsertWorkerNodeInfo = upsertWorkerNodeInfo c
            }


    type SaveResultProxy =
        {
            saveResultData : ResultDataWithId -> UnitResult
        }

        static member create c =
            {
                saveResultData = saveResultData c
            }


    type SaveChartsProxy =
        {
            saveCharts : ChartInfo -> UnitResult
        }

        static member create resultLocation =
            {
                saveCharts = fun (c : ChartInfo) -> saveLocalChartInfo (Some (resultLocation, c.defaultValueId)) c
            }


    type ProcessMessageProxy =
        {
            updateProgress : RemoteProgressUpdateInfo -> UnitResult
            saveResult : ResultDataWithId -> UnitResult
            saveCharts : ChartInfo -> UnitResult
            register : WorkerNodeInfo -> UnitResult
            unregister : WorkerNodeId -> UnitResult
        }


    type GetRunStateProxy =
        {
            loadRunQueueProgress : unit -> ListResult<RunQueue>
        }

        static member create c =
            {
                loadRunQueueProgress = fun () -> loadRunQueueProgress c
            }

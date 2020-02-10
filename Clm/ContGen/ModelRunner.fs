namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.ModelGeneratorErrors
open ServiceProxy.ModelGeneratorProxy
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CalculationData
open ClmSys.WorkerNodeData
open ClmSys.ContGenData
open ClmSys.SolverRunnerData
open ClmSys.Rop
open ServiceProxy.ModelRunnerProxy
open ClmSys.ModelRunnerErrors
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodePrimitives
open ServiceProxy.ModelRunnerProxy

module ModelRunner =

    let private toError g f = f |> g |> ModelRunnerErr |> Error
    let private addError g f e = ((f |> g |> ModelRunnerErr) + e) |> Error


    let runModel (proxy : RunModelProxy) (q : RunQueue) =
        match q.toMessageInfoOpt proxy.loadModelData proxy.minUsefulEe with
        | Ok (Some m) -> proxy.sendRunModelMessage m
        | Ok None -> q.runQueueId |> MissingWorkerNodeErr |> toError RunModelErr
        | Error e -> (addError RunModelErr) (UnableToLoadModelDataErr (q.runQueueId, q.info.modelDataId )) e


    /// Tries to run the first not scheduled run queue entry using the first available worker node.
    let tryRunFirstModel (proxy : TryRunFirstModelProxy) =
        let addError = addError TryRunFirstModelErr

        match proxy.tryLoadFirstRunQueue() with
        | Ok (Some q) ->
            match proxy.tryGetAvailableWorkerNode() with
            | Ok (Some n) ->
                let q1 = { q with workerNodeIdOpt = Some n; runQueueStatus = InProgressRunQueue }

                match proxy.upsertRunQueue q1 with
                | Ok() ->
                    match proxy.runModel q1 with
                    | Ok() -> Ok WorkScheduled
                    | Error e ->
                        match proxy.upsertRunQueue { q1 with runQueueStatus = FailedRunQueue } with
                        | Ok() -> addError UnableToRunModelErr e
                        | Error f -> addError UnableToRunModelAndUpsertStatusErr (f + e)
                | Error e -> addError UpsertRunQueueErr e
            | Ok None -> Ok NoAvailableWorkerNodes
            | Error e -> addError TryGetAvailableWorkerNodeErr e
        | Ok None -> Ok NoWork
        | Error e -> addError TryLoadFirstRunQueueErr e



    /// Tries to run all available work items (run queue) on all availalble work nodes until one or the other is exhausted.
    let tryRunAllModels (proxy : TryRunAllModelsProxy) =
        let rec doWork() =
            match proxy.tryRunFirstModel() with
            | Ok r ->
                match r with
                | WorkScheduled -> doWork()
                | NoWork -> Ok()
                | NoAvailableWorkerNodes -> Ok()
            | Error e -> addError TryRunAllModelsErr UnableToTryRunFirstModelErr e

        doWork()


    let updateProgress (proxy : UpdateProgressProxy) (i : RemoteProgressUpdateInfo) =
        let addError = addError UpdateProgressErr
        let toError = toError UpdateProgressErr

        match proxy.tryLoadRunQueue i.runningProcessData.runQueueId with
        | Ok (Some q) ->
            match q.runQueueStatus with
            | InProgressRunQueue ->
                let q1 = { q with progress = i.progress }

                let q2 =
                    match i.progress with
                    | NotStarted | InProgress _ -> q1
                    | Completed -> { q1 with runQueueStatus = CompletedRunQueue }
                    | Failed _ -> { q1 with runQueueStatus = FailedRunQueue }

                match proxy.upsertRunQueue q2 with
                | Ok() -> Ok()
                | Error e -> addError (UnableToLoadRunQueueErr i.runningProcessData.runQueueId) e
            | NotStartedRunQueue | InactiveRunQueue | CompletedRunQueue | FailedRunQueue | ModifyingRunQueue ->
                toError (InvalidRunQueueStatusErr i.runningProcessData.runQueueId)
        | Ok None -> toError (UnableToFindLoadRunQueueErr i.runningProcessData.runQueueId)
        | Error e -> addError (UnableToLoadRunQueueErr i.runningProcessData.runQueueId) e



    let register (proxy : RegisterProxy) (r : WorkerNodeInfo) =
        match proxy.upsertWorkerNodeInfo r with
        | Ok() -> Ok()
        | Error e -> addError RegisterErr (UnableToUpsertWorkerNodeInfoErr r.workerNodeId) e


    type OnUnregisterProxy =
        {
            loadWorkerNodeInfo : WorkerNodeId -> ClmResult<WorkerNodeInfo>
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }


    let onUnregister (proxy : OnUnregisterProxy) (r : WorkerNodeId) =
        let addError f e = ((f |> OnUnregisterErr |> PartitionerErr) + e) |> Error
        let toError e = e |> OnUnregisterErr |> PartitionerErr |> Error

        match proxy.loadWorkerNodeInfo r with
        | Ok w ->
            match proxy.upsertWorkerNodeInfo w with
            | Ok() -> Ok()
            | Error e -> addError CannotUpsertWorkerNodeInfo e
        | Error e -> addError CannotLoadWorkerNodeInfo e

namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.AsyncRunErrors
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

module ModelRunner =

    let private toError g f = f |> g |> ModelRunnerErr |> Error
    let private addError g f e = ((f |> g |> ModelRunnerErr) + e) |> Error


    type RunModelProxy =
        {
            minUsefulEe : MinUsefulEe
            sendRunModelMessage : MessageInfo -> UnitResult
            loadModelData : ModelDataId -> ClmResult<ModelData>
        }


    let runModel (proxy : RunModelProxy) (q : RunQueue) =
        match q.toMessageInfoOpt proxy.loadModelData proxy.minUsefulEe with
        | Ok (Some m) -> proxy.sendRunModelMessage m
        | Ok None -> q.runQueueId |> MissingWorkerNodeErr |> toError RunModelErr
        | Error e -> (addError RunModelErr) (UnableToLoadModeldata (q.runQueueId, q.info.modelDataId )) e


    type TryRunFirstModelProxy =
        {
            tryLoadFirstRunQueue : unit -> ClmResult<RunQueue option>
            tryGetAvailableWorkerNode : unit -> ClmResult<WorkerNodeId option>
        }


    let tryRunFirstModel (proxy : TryRunFirstModelProxy) =
        match proxy.tryLoadFirstRunQueue() with
        | Ok (Some q) ->
            Ok()
        | Ok None -> Ok()
        | Error e -> Error e

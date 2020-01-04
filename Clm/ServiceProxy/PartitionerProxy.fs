namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.Registry
open ClmSys.Logging
open PartitionerServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ClmSys.GeneralErrors
open MessagingServiceInfo.ServiceInfo

module PartitionerProxy =

    type PartitionerProxyInfo =
        {
            partitionerConnectionString : ConnectionString
            resultLocation : string
        }

        static member defaultValue =
            {
                partitionerConnectionString = clmConnectionString
                resultLocation = DefaultResultLocationFolder
            }


    type PartitionerProxy =
        {
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> Result<ModelData, ClmError>
            saveResultData : ResultDataWithId -> Result<unit, ClmError>
            tryLoadResultData : ResultDataId -> Result<ResultDataWithId, ClmError>

            saveCharts : ChartInfo -> unit

            saveRunModelParamWithRemoteId : RunModelParamWithRemoteId -> unit
            tryLoadRunModelParamWithRemoteId : RemoteProcessId -> RunModelParamWithRemoteId option
            loadAllRunModelParamWithRemoteId : unit -> list<RunModelParamWithRemoteId>
            tryDeleteRunModelParamWithRemoteId : RemoteProcessId -> unit option

            saveWorkerNodeState : WorkerNodeState -> bool option
            loadAllWorkerNodeState : unit -> list<WorkerNodeState>
            tryDeleteWorkerNodeState : WorkerNodeId -> unit option
        }


        static member create (i : PartitionerProxyInfo) =
            let name = partitionerServiceName
            let connectionString = i.partitionerConnectionString

            {
                tryLoadModelData = tryLoadModelData connectionString
                saveResultData = saveResultData connectionString
                tryLoadResultData = tryLoadResultData connectionString

                saveCharts = fun (c : ChartInfo) -> c.trySave logger (Some (i.resultLocation, c.defaultValueId)) |> ignore

                saveRunModelParamWithRemoteId = fun q -> tryFun (fun () -> saveRunModelParamWithRemoteIdFs name q) |> ignore
                tryLoadRunModelParamWithRemoteId = fun q -> tryFun (fun () -> tryLoadRunModelParamWithRemoteIdFs name q) |> Option.bind id
                loadAllRunModelParamWithRemoteId = fun () -> loadAll getRunModelParamWithRemoteIdsFs tryLoadRunModelParamWithRemoteIdFs name
                tryDeleteRunModelParamWithRemoteId = fun m -> tryFun (fun _ -> tryDeleteRunModelParamWithRemoteIdFs name m)

                saveWorkerNodeState = fun s -> tryFun (fun _ -> saveWorkerNodeStateFs name s)
                loadAllWorkerNodeState = fun () -> loadAll getWorkerNodeStateIdsFs tryLoadWorkerNodeStateFs name
                tryDeleteWorkerNodeState = fun s -> tryFun (fun _ -> tryDeleteWorkerNodeStateFs name s)
            }

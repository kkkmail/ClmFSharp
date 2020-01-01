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

module PartitionerProxy =

    type PartitionerProxyInfo =
        {
            partitionerConnectionString : ConnectionString
            resultLocation : string
            logger : Logger
        }

        static member defaultValue =
            {
                partitionerConnectionString = clmConnectionString
                resultLocation = DefaultResultLocationFolder
                logger = logger
            }


    type PartitionerProxy =
        {
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> ModelData option

            saveResultData : ResultDataWithId -> unit
            tryLoadResultData : ResultDataId -> ResultDataWithId option

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
            let logExn = i.logger.logExn "PartitionerProxy"
            let tryDbFun c f = tryDbFun logExn c f
            let tryFun f = tryFun logExn f
            let connectionString = i.partitionerConnectionString


            let loadAll getIds tryLoad name =
                match tryFun (getIds name) with
                | Some i ->
                    i
                    |> List.map (fun e -> tryFun (fun _ -> tryLoad name e) |> Option.bind id)
                    |> List.choose id
                | None -> []

            {
                tryLoadModelData = fun a m -> tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id

                saveResultData = fun r -> tryDbFun connectionString (saveResultData r) |> ignore
                tryLoadResultData = fun r -> tryDbFun connectionString (tryLoadResultData r) |> Option.bind id

                saveCharts = fun (c : ChartInfo) -> c.trySave logger (Some (i.resultLocation, c.defaultValueId)) |> ignore

                saveRunModelParamWithRemoteId = fun q -> tryFun (fun () -> saveRunModelParamWithRemoteIdFs name q) |> ignore
                tryLoadRunModelParamWithRemoteId = fun q -> tryFun (fun () -> tryLoadRunModelParamWithRemoteIdFs name q) |> Option.bind id
                loadAllRunModelParamWithRemoteId = fun () -> loadAll getRunModelParamWithRemoteIdsFs tryLoadRunModelParamWithRemoteIdFs name
                tryDeleteRunModelParamWithRemoteId = fun m -> tryFun (fun _ -> tryDeleteRunModelParamWithRemoteIdFs name m)

                saveWorkerNodeState = fun s -> tryFun (fun _ -> saveWorkerNodeStateFs name s)
                loadAllWorkerNodeState = fun () -> loadAll getWorkerNodeStateIdsFs tryLoadWorkerNodeStateFs name
                tryDeleteWorkerNodeState = fun s -> tryFun (fun _ -> tryDeleteWorkerNodeStateFs name s)
            }

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
            let logError e = printfn "Error: %A" e
            let tryDbFun c f = tryDbFun logError c f
            let tryFun f = tryFun logError f
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


    //type PartitionerProxy1(i : PartitionerProxyInfo) =
    //    let name = partitionerServiceName
    //    let logError e = printfn "Error: %A" e
    //    let tryDbFun c f = tryDbFun logError c f
    //    let tryFun f = tryFun logError f
    //    let connectionString = i.partitionerConnectionString


    //    let loadAll getIds tryLoad name =
    //        match tryFun (getIds name) with
    //        | Some i ->
    //            i
    //            |> List.map (fun e -> tryFun (fun _ -> tryLoad name e) |> Option.bind id)
    //            |> List.choose id
    //        | None -> []


    //    member __.tryLoadModelData a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id

    //    member __.saveResultData r = tryDbFun connectionString (saveResultData r) |> ignore
    //    member __.tryLoadResultData r = tryDbFun connectionString (tryLoadResultData r) |> Option.bind id

    //    member __.saveCharts (c : ChartInfo) = c.trySave logger (Some (i.resultLocation, c.defaultValueId)) |> ignore

    //    member __.saveRunModelParamWithRemoteId q = tryFun (fun () -> saveRunModelParamWithRemoteIdFs name q) |> ignore
    //    member __.tryLoadRunModelParamWithRemoteId q = tryFun (fun () -> tryLoadRunModelParamWithRemoteIdFs name q) |> Option.bind id
    //    member __.loadAllRunModelParamWithRemoteId () = loadAll getRunModelParamWithRemoteIdsFs tryLoadRunModelParamWithRemoteIdFs name
    //    member __.tryDeleteRunModelParamWithRemoteId m = tryFun (fun _ -> tryDeleteRunModelParamWithRemoteIdFs name m)

    //    member __.saveWorkerNodeState s = tryFun (fun _ -> saveWorkerNodeStateFs name s)
    //    member __.loadAllWorkerNodeState () = loadAll getWorkerNodeStateIdsFs tryLoadWorkerNodeStateFs name
    //    member __.tryDeleteWorkerNodeState s = tryFun (fun _ -> tryDeleteWorkerNodeStateFs name s)

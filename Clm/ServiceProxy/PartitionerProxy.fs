namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.Registry
open ClmSys.Logging

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


    type PartitionerProxy(i : PartitionerProxyInfo) =
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


        member __.tryLoadModelData a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id
        member __.saveResultData r = tryDbFun connectionString (saveResultData r) |> ignore
        member __.tryLoadResultData r = tryDbFun connectionString (tryLoadResultData r) |> Option.bind id

        member __.saveCharts (c : ChartInfo) = c.trySave logger (Some (i.resultLocation, c.defaultValueId)) |> ignore

        member __.saveRunModelParamWithRemoteId q = tryFun (fun () -> saveRunModelParamWithRemoteIdFs name q) |> ignore
        member __.tryLoadRunModelParamWithRemoteId q = tryFun (fun () -> tryLoadRunModelParamWithRemoteIdFs name q) |> Option.bind id
        member __.loadAllRunModelParamWithRemoteId () = loadAll getRunModelParamWithRemoteIdsFs tryLoadRunModelParamWithRemoteIdFs name
        member __.tryDeleteRunModelParamWithRemoteId m = tryFun (fun _ -> tryDeleteRunModelParamWithRemoteIdFs name m)

        member __.saveWorkerNodeState s = tryFun (fun _ -> saveWorkerNodeStateFs name s)
        member __.loadAllWorkerNodeState () = loadAll getWorkerNodeStateIdsFs tryLoadWorkerNodeStateFs name
        member __.tryDeleteWorkerNodeState s = tryFun (fun _ -> tryDeleteWorkerNodeStateFs name s)

        member __.savePartitionerQueueElement q = tryFun (fun () -> savePartitionerQueueElementFs name q) |> ignore
        member __.tryLoadPartitionerQueueElement q = tryFun (fun () -> tryLoadPartitionerQueueElementFs name q) |> Option.bind id
        member __.loadAllPartitionerQueueElement () = loadAll getPartitionerQueueElementIdsFs tryLoadPartitionerQueueElementFs name
        member __.tryDeletePartitionerQueueElement m = tryFun (fun _ -> tryDeletePartitionerQueueElementFs name m)

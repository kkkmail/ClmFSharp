namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open NoSql.FileSystemTypes
open ClmSys.Registry
open ClmSys.Logging
open PartitionerServiceInfo.ServiceInfo
open ClmSys.ServiceInstaller

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

        member __.saveWorkerNodeInfo w = tryFun (fun () -> saveWorkerNodeInfoFs name w) |> ignore
        member __.loadAllWorkerNodeInfo () = loadAll getWorkerNodeInfoIdsFs tryLoadWorkerNodeInfoFs name
        member __.tryDeleteWorkerNodeInfo m = tryFun (fun _ -> tryDeleteWorkerNodeInfoFs name m)

        member __.saveCharts (c : ChartInfo) = c.trySave logger (Some i.resultLocation) |> ignore

        member __.savePartitionerQueueElement q = tryFun (fun () -> savePartitionerQueueElementFs name q) |> ignore
        member __.loadAllPartitionerQueueElement () = loadAll getPartitionerQueueElementIdsFs tryLoadPartitionerQueueElementFs name
        member __.tryDeletePartitionerQueueElement m = tryFun (fun _ -> tryDeletePartitionerQueueElementFs name m)

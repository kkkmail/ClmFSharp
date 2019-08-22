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

module PartitionerProxy =

    type PartitionerProxyInfo =
        {
            partitionerConnectionString : ConnectionString
        }

        static member defaultValue =
            {
                partitionerConnectionString = clmConnectionString
            }


    type PartitionerProxy(i : PartitionerProxyInfo) =
        let name = partitionerServiceName
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f
        let tryFun f = tryFun logError f
        let connectionString = i.partitionerConnectionString

        let loadAllWorkerNodeInfoImpl () =
            match tryFun (getWorkerNodeInfoIdsFs name) with
            | Some i ->
                i
                |> List.map (fun e -> tryFun (fun _ -> tryLoadWorkerNodeInfoFs name e) |> Option.bind id)
                |> List.choose id
            | None -> []


        member __.tryLoadModelData a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id
        member __.saveResultData r = tryDbFun connectionString (saveResultData r) |> ignore

        member __.saveWorkerNodeInfo (w : WorkerNodeInfo) = tryFun (fun () -> saveWorkerNodeInfoFs name w) |> ignore
        member __.loadAllWorkerNodeInfo () = loadAllWorkerNodeInfoImpl ()
        member __.tryDeleteWorkerNodeInfo m = tryFun (fun _ -> tryDeleteWorkerNodeInfoFs name m)

namespace ServiceProxy

open ClmSys.Retry
open ContGenServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ServiceProxy.Runner
open ClmSys.GeneralData

module WorkerNodeProxy =

    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


    type WorkerNodeProxyInfo =
        {
            //storageType : StorageType
            dummy : int
        }

        static member defaultValue =
            {
                //storageType = RemoteStorage
                dummy = 0
            }

    //type WorkerNodeProxyInfo =
    //    {
    //        dummy : int
    //    }

    //    static member defaultValue =
    //        {
    //            //workerNodeConnectionString = clmConnectionString
    //            dummy = 0
    //        }


    type WorkerNodeProxy(i : WorkerNodeProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryFun f = tryFun logError f

        let loadAllWorkerNodeRunModelDataImpl () =
            match tryFun getWorkerNodeRunModelDataIdsFs with
            | Some i ->
                i
                |> List.map (fun e -> tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs e) |> Option.bind id)
                |> List.choose id
            | None -> []


        member __.saveWorkerNodeRunModelData m = tryFun (fun _ -> saveWorkerNodeRunModelDataFs m) |> ignore
        member __.tryLoadWorkerNodeRunModelData m = tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs m) |> Option.bind id
        member __.tryDeleteWorkerNodeRunModelData m = tryFun (fun _ -> tryDeleteWorkerNodeRunModelDataFs m)
        member __.runModel p = runLocalModel p true
        member __.loadAllWorkerNodeRunModelData() = loadAllWorkerNodeRunModelDataImpl()

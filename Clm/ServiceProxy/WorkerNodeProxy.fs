namespace ServiceProxy

open ClmSys.Retry
open ContGenServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ServiceProxy.Runner
open ClmSys.GeneralData
open ClmSys.Registry

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
        let name = workerNodeServiceName
        let logError e = printfn "Error: %A" e
        let tryFun f = tryFun logError f

        let loadAllWorkerNodeRunModelDataImpl () =
            match tryFun (getWorkerNodeRunModelDataIdsFs name) with
            | Some i ->
                i
                |> List.map (fun e -> tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs name e) |> Option.bind id)
                |> List.choose id
            | None -> []

        member __.saveWorkerNodeRunModelData m = tryFun (fun _ -> saveWorkerNodeRunModelDataFs name m) |> ignore
        member __.tryLoadWorkerNodeRunModelData m = tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs name m) |> Option.bind id
        member __.tryDeleteWorkerNodeRunModelData m = tryFun (fun _ -> tryDeleteWorkerNodeRunModelDataFs name m)
        member __.runModel p = runLocalModel p true
        member __.loadAllWorkerNodeRunModelData() = loadAllWorkerNodeRunModelDataImpl()

        // These ones are needed for SolverRunner.
        member __.saveModelData m = tryFun (fun _ -> saveModelDataFs solverRunnerName m) |> ignore
        member __.tryDeleteModelData m = tryFun (fun _ -> tryDeleteModelDataFs solverRunnerName m)

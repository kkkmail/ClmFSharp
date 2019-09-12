namespace ServiceProxy

open ClmSys.Retry
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

        let loadAllResultDataImpl () =
            match tryFun (getResultDataIdsFs name) with
            | Some i ->
                i
                |> List.map (fun e -> tryFun (fun _ -> tryLoadResultDataFs name e) |> Option.bind id)
                |> List.choose id
            | None -> []

        member __.saveWorkerNodeRunModelData m = tryFun (fun _ -> saveWorkerNodeRunModelDataFs name m) |> ignore
        member __.tryLoadWorkerNodeRunModelData m = tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs name m) |> Option.bind id
        member __.tryDeleteWorkerNodeRunModelData m = tryFun (fun _ -> tryDeleteWorkerNodeRunModelDataFs name m)
        member __.runModel p = runLocalModel p true
        member __.loadAllWorkerNodeRunModelData() = loadAllWorkerNodeRunModelDataImpl()

        // These ones are needed for SolverRunner interop.
        // Note that the "name" is different here.
        member __.saveModelData m = tryFun (fun _ -> saveModelDataFs solverRunnerName m) |> ignore
        member __.tryDeleteModelData m = tryFun (fun _ -> tryDeleteModelDataFs solverRunnerName m)

        member __.tryLoadResultData r = tryFun (fun _ -> tryLoadResultDataFs solverRunnerName r) |> Option.bind id
        member __.tryDeleteResultData r = tryFun (fun _ -> tryDeleteResultDataFs solverRunnerName r)
        member __.loadAllResultData() = loadAllResultDataImpl()

        member __.tryLoadChartInfo r = tryFun (fun _ -> tryLoadChartInfoFs solverRunnerName r) |> Option.bind id
        member __.tryDeleteChartInfo r = tryFun (fun _ -> tryDeleteChartInfoFs solverRunnerName r)

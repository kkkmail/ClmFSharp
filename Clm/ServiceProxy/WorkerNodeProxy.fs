namespace ServiceProxy

open ClmSys.Retry
open NoSql.FileSystemTypes
open ServiceProxy.Runner
open ClmSys.GeneralData
open ClmSys.Registry
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams

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

    type WorkerNodeProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> unit
            tryLoadWorkerNodeRunModelData : RemoteProcessId -> WorkerNodeRunModelData option
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> unit option
            runModel : RunModelParam -> LocalProcessStartedInfo option
            getCommandLine : RunModelParam -> string
            loadAllWorkerNodeRunModelData : unit -> list<WorkerNodeRunModelData>

            saveModelData : ModelData -> unit
            tryDeleteModelData : ModelDataId -> unit option

            tryLoadResultData : ResultDataId -> ResultDataWithId option
            tryDeleteResultData : ResultDataId -> unit option
            loadAllResultData : unit -> list<ResultDataWithId>

            tryLoadChartInfo : ResultDataId -> ChartInfo option
            tryDeleteChartInfo : ResultDataId -> unit option
        }

        static member create (i : WorkerNodeProxyInfo) =
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

            {
                saveWorkerNodeRunModelData = fun m -> tryFun (fun _ -> saveWorkerNodeRunModelDataFs name m) |> ignore
                tryLoadWorkerNodeRunModelData = fun m -> tryFun (fun _ -> tryLoadWorkerNodeRunModelDataFs name m) |> Option.bind id
                tryDeleteWorkerNodeRunModelData = fun m -> tryFun (fun _ -> tryDeleteWorkerNodeRunModelDataFs name m)
                runModel = fun p -> runLocalModel p true
                getCommandLine = fun p -> getCommandLine p true
                loadAllWorkerNodeRunModelData = loadAllWorkerNodeRunModelDataImpl

                // These ones are needed for SolverRunner interop.
                // Note that the "name" is different here.
                saveModelData = fun m -> tryFun (fun _ -> saveModelDataFs solverRunnerName m) |> ignore
                tryDeleteModelData = fun m -> tryFun (fun _ -> tryDeleteModelDataFs solverRunnerName m)

                tryLoadResultData = fun r -> tryFun (fun _ -> tryLoadResultDataFs solverRunnerName r) |> Option.bind id
                tryDeleteResultData = fun r -> tryFun (fun _ -> tryDeleteResultDataFs solverRunnerName r)
                loadAllResultData = loadAllResultDataImpl

                tryLoadChartInfo = fun r -> tryFun (fun _ -> tryLoadChartInfoFs solverRunnerName r) |> Option.bind id
                tryDeleteChartInfo = fun r -> tryFun (fun _ -> tryDeleteChartInfoFs solverRunnerName r)
            }

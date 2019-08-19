namespace ServiceProxy

open ClmSys.Retry
open ContGenServiceInfo.ServiceInfo
open NoSql.FileSystemTypes

module WorkerNodeProxy =

    type WorkerNodeProxyInfo =
        {
            //workerNodeConnectionString : ConnectionString
            dummy : int
        }

        static member defaultValue =
            {
                //workerNodeConnectionString = clmConnectionString
                dummy = 0
            }


    type WorkerNodeProxy(i : WorkerNodeProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryFun f = tryFun logError f

        let saveResultDataImpl m = tryFun (fun _ -> saveModelDataFs m) |> ignore

        let runModelImpl (p : RunModelParamWithCallBack) : ProcessStartInfo =
            failwith ""

        member __.saveModelData m = saveResultDataImpl m
        member __.runModel p = runModelImpl p

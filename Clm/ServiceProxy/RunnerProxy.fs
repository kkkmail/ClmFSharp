namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo

module Runner =

    type LocalRunnerConfig =
        {
            connectionString : ConnectionString
        }

        static member defaultValue =
            {
                connectionString = clmConnectionString
            }


    type PartitionerRunnerConfig =
        {
            connectionString : ConnectionString
            runModel : RunModelParamWithCallBack -> ProcessStartInfo
        }

        static member defaultValue r =
            {
                connectionString = clmConnectionString
                runModel = r
            }


    type RunnerProxyInfo =
        | LocalRunnerProxy of LocalRunnerConfig
        | PartitionerRunnerProxy of PartitionerRunnerConfig

        static member defaultValue = LocalRunnerProxy LocalRunnerConfig.defaultValue


    type RunnerProxy(i : RunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f


        let connectionString =
            match i with
            | LocalRunnerProxy c -> c.connectionString
            | PartitionerRunnerProxy c -> c.connectionString


        let tryLoadClmDefaultValueImpl d = tryDbFun connectionString (tryLoadClmDefaultValue d) |> Option.bind id
        let tryUpdateModelDataImpl m = tryDbFun connectionString (tryUpdateModelData m)
        let saveRunQueueEntryImpl modelId p = tryDbFun connectionString (saveRunQueueEntry modelId p)
        let tryUpdateClmTaskImpl a = tryDbFun connectionString (tryUpdateClmTask a)
        let addClmTaskImpl a = tryDbFun connectionString (addClmTask a)
        let tryLoadClmTaskImpl a t = tryDbFun connectionString (tryLoadClmTask a t) |> Option.bind id
        let tryLoadModelDataImpl a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id
        let loadIncompleteClmTasksImpl a = tryDbFun connectionString (loadIncompleteClmTasks a)
        let loadRunQueueImpl a = tryDbFun connectionString (loadRunQueue a)
        let deleteRunQueueEntryImpl runQueueId = tryDbFun connectionString (deleteRunQueueEntry runQueueId)


        let runModelImpl (p : RunModelParamWithCallBack) =
            match i with
            | LocalRunnerProxy _ ->
                let fullExeName = getExeName p.runModelParam.exeName

                let data =
                    {
                        modelDataId = p.callBack.calledBackModelId
                        minUsefulEe = p.runModelParam.minUsefulEe
                        remote = false
                    }

                let commandLineParams = p.runModelParam.commandLineParam.toCommandLine data
                printfn "runModel::commandLineParams = %A\n" commandLineParams

                //let runArgs =
                //    {
                //        callBackInfo = p.callBack
                //        notifyOnStarted = n
                //        fileName = fullExeName
                //        commandLineArgs = commandLineParams
                //        startDir = None
                //    }

                runProc p.callBack fullExeName commandLineParams None
                //runProc runArgs

            | PartitionerRunnerProxy c -> c.runModel p


        //new() = RunnerProxy(RunnerProxyInfo.defaultValue)

        member __.tryLoadClmDefaultValue d = tryLoadClmDefaultValueImpl d
        member __.tryUpdateModelData m = tryUpdateModelDataImpl m
        member __.saveRunQueueEntry modelId p = saveRunQueueEntryImpl modelId p
        member __.tryUpdateClmTask c = tryUpdateClmTaskImpl c
        member __.addClmTask c = addClmTaskImpl c
        member __.tryLoadClmTask i t = tryLoadClmTaskImpl i t
        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.loadIncompleteClmTasks i = loadIncompleteClmTasksImpl i
        member __.loadRunQueue i = loadRunQueueImpl i
        member __.deleteRunQueueEntry runQueueId = deleteRunQueueEntryImpl runQueueId
        member __.runModel p = runModelImpl p

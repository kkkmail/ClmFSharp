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


    type RemoteRunnerConfig =
        {
            connectionString : ConnectionString
        }


    type RunnerProxyInfo =
        | LocalRunner of LocalRunnerConfig
        | RemoteRunner of RemoteRunnerConfig

        static member defaultValue = LocalRunner LocalRunnerConfig.defaultValue


    type RunnerProxy(i : RunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f


        let connectionString =
            match i with
            | LocalRunner c -> c.connectionString
            | RemoteRunner c -> c.connectionString


        let tryLoadClmDefaultValueImpl d =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryLoadClmDefaultValue d)
            | RemoteRunner c -> failwith ""


        let tryUpdateModelDataImpl m =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryUpdateModelData m)
            | RemoteRunner c -> failwith ""


        let runModelImpl (p : RunModelParam) =
            match i with
            | LocalRunner c ->
                let fullExeName = getExeName p.exeName

                let data =
                    {
                        modelDataId = p.callBack.calledBackModelId
                        minUsefulEe = p.minUsefulEe
                    }

                let commandLineParams = p.commandLineParam.toCommandLine data
                printfn "runModel::commandLineParams = %A\n" commandLineParams
                runProc p.callBack fullExeName commandLineParams None

            | RemoteRunner c -> failwith ""


        let saveRunQueueEntryImpl modelId p = tryDbFun connectionString (saveRunQueueEntry modelId p)
        let tryUpdateClmTaskImpl c = tryDbFun connectionString (tryUpdateClmTask c)
        let addClmTaskImpl c = tryDbFun connectionString (addClmTask c)
        let tryLoadClmTaskImpl i t = tryDbFun connectionString (tryLoadClmTask i t)
        let tryLoadModelDataImpl i m = tryDbFun connectionString (tryLoadModelData i m)
        let loadIncompleteClmTasksImpl i = tryDbFun connectionString (loadIncompleteClmTasks i)
        let loadRunQueueImpl i = tryDbFun connectionString (loadRunQueue i)
        let deleteRunQueueEntryImpl runQueueId = tryDbFun connectionString (deleteRunQueueEntry runQueueId)

        new() = RunnerProxy(RunnerProxyInfo.defaultValue)
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
        member __.runModel (p : RunModelParam) = runModelImpl (p : RunModelParam)

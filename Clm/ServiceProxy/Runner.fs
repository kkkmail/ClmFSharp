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
            | LocalRunner c -> tryDbFun c.connectionString (tryLoadClmDefaultValue d) |> Option.bind id
            | RemoteRunner c -> c.tryLoadClmDefaultValue d


        let tryUpdateModelDataImpl m =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryUpdateModelData m)
            | RemoteRunner c -> failwith ""


        let runModelImpl (p : RunModelParam) =
            match i with
            | LocalRunner _ ->
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


        let saveRunQueueEntryImpl modelId p =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (saveRunQueueEntry modelId p)
            | RemoteRunner c -> failwith ""


        let tryUpdateClmTaskImpl a =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryUpdateClmTask a)
            | RemoteRunner c -> failwith ""


        let addClmTaskImpl a =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (addClmTask a)
            | RemoteRunner c -> failwith ""


        let tryLoadClmTaskImpl a t =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryLoadClmTask a t)
            | RemoteRunner c -> failwith ""


        let tryLoadModelDataImpl a m =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (tryLoadModelData a m)
            | RemoteRunner c -> failwith ""


        let loadIncompleteClmTasksImpl a =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (loadIncompleteClmTasks a)
            | RemoteRunner c -> failwith ""


        let loadRunQueueImpl a =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (loadRunQueue a)
            | RemoteRunner c -> failwith ""


        let deleteRunQueueEntryImpl runQueueId =
            match i with
            | LocalRunner c -> tryDbFun c.connectionString (deleteRunQueueEntry runQueueId)
            | RemoteRunner c -> failwith ""


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
        member __.runModel p = runModelImpl p

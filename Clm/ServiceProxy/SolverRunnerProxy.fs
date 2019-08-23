namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open ClmSys.Registry
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.CalculationData
open NoSql.FileSystemTypes

module SolverRunner =

    type LocalSolverRunnerConfig =
        {
            connectionString : ConnectionString
        }

        static member defaultValue =
            {
                connectionString = clmConnectionString
            }


    type RemoteSolverRunnerConfig =
        | RemoteSolverRunnerConfig


    type SolverRunnerProxyInfo =
        | LocalSolverRunner of LocalSolverRunnerConfig
        | RemoteSolverRunner of RemoteSolverRunnerConfig

        static member defaultValue = LocalSolverRunner LocalSolverRunnerConfig.defaultValue
        static member defaultRemoteValue = RemoteSolverRunner RemoteSolverRunnerConfig


    type SolverRunnerProxy(i : SolverRunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f
        let tryFun f = tryFun logError f


        let tryLoadModelDataImpl a m =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (tryLoadModelData a m)
            | RemoteSolverRunner _ -> tryFun (fun _ -> tryLoadModelDataFs solverRunnerName m)
            |> Option.bind id


        let saveResultDataImpl r =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (saveResultData r) |> ignore
            | RemoteSolverRunner _ ->
                printfn "SolverRunnerProxy.saveResultDataImpl - saving results..."
                tryFun (fun () -> saveResultDataFs solverRunnerName r) |> ignore


        let saveChartInfoImpl r =
            match i with
            | LocalSolverRunner _ -> ignore()
            | RemoteSolverRunner _ ->
                printfn "SolverRunnerProxy.saveChartsImpl - saving charts: %A" r
                tryFun (fun () -> saveChartInfoFs solverRunnerName r) |> ignore


        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.saveResultData r = saveResultDataImpl r
        member __.saveChartInfo c = saveChartInfoImpl c

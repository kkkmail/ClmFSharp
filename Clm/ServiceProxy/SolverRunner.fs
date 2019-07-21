namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo

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
        {
            connectionString : ConnectionString
        }


    type SolverRunnerProxyInfo =
        | LocalSolverRunner of LocalSolverRunnerConfig
        | RemoteSolverRunner of RemoteSolverRunnerConfig

        static member defaultValue = LocalSolverRunner LocalSolverRunnerConfig.defaultValue


    type SolverRunnerProxy(i : SolverRunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f


        let connectionString =
            match i with
            | LocalSolverRunner c -> c.connectionString
            | RemoteSolverRunner c -> c.connectionString

        let tryLoadModelDataImpl i m = tryDbFun connectionString (tryLoadModelData i m)
        let saveResultDataImpl r = tryDbFun connectionString (saveResultData r)
        let saveChartsImpl p = ignore()


        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.saveResultData r = saveResultDataImpl r
        member __.saveCharts (p : List<string>) = saveChartsImpl p

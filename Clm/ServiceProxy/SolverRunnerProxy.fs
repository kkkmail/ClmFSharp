namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
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
            | RemoteSolverRunner c -> tryFun (fun _ -> tryLoadModelDataFs m)
            |> Option.bind id


        let saveResultDataImpl r =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (saveResultData r) |> ignore
            | RemoteSolverRunner _ -> tryFun (fun _ -> saveResultDataFs r) |> ignore


        let saveChartsImpl r p =
            match i with
            | LocalSolverRunner _ -> ignore()
            | RemoteSolverRunner _ -> saveChartsFs r p |> ignore


        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.saveResultData r = saveResultDataImpl r
        member __.saveCharts (r : ResultDataId) (p : List<string>) = saveChartsImpl r p

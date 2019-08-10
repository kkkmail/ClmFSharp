namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData

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
            tryLoadModelData : ContGenServiceAccessInfo -> ModelDataId -> ModelData option
            saveResultData : ResultDataWithId -> unit
            saveCharts : list<string> -> unit
        }


    type SolverRunnerProxyInfo =
        | LocalSolverRunner of LocalSolverRunnerConfig
        | RemoteSolverRunner of RemoteSolverRunnerConfig

        static member defaultValue = LocalSolverRunner LocalSolverRunnerConfig.defaultValue


    type SolverRunnerProxy(i : SolverRunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f


        let tryLoadModelDataImpl a m =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (tryLoadModelData a m) |> Option.bind id
            | RemoteSolverRunner c -> c.tryLoadModelData a m


        let saveResultDataImpl r =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (saveResultData r) |> ignore
            | RemoteSolverRunner c -> c.saveResultData r


        let saveChartsImpl p =
            match i with
            | LocalSolverRunner _ -> ignore()
            | RemoteSolverRunner c -> c.saveCharts p


        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.saveResultData r = saveResultDataImpl r
        member __.saveCharts (p : List<string>) = saveChartsImpl p

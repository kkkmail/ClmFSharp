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
            //connectionString : ConnectionString
            dummy : int
        }


    type SolverRunnerProxyInfo =
        | LocalSolverRunner of LocalSolverRunnerConfig
        | RemoteSolverRunner of RemoteSolverRunnerConfig

        static member defaultValue = LocalSolverRunner LocalSolverRunnerConfig.defaultValue


    type SolverRunnerProxy(i : SolverRunnerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryFun f = tryFun logError f
        let tryDbFun c f = tryDbFun logError c f


        let tryLoadModelDataRemote (a : ContGenServiceAccessInfo) (m : ModelDataId) =
            failwith ""

        let saveResultDataRemote (r : ResultDataWithId) =
            failwith ""


        let saveChartsRemote (p : List<string>) =
            failwith ""


        let tryLoadModelDataImpl a m =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (tryLoadModelData a m)
            | RemoteSolverRunner c -> tryLoadModelDataRemote a m


        let saveResultDataImpl r =
            match i with
            | LocalSolverRunner c -> tryDbFun c.connectionString (saveResultData r)
            | RemoteSolverRunner c -> tryFun (saveResultDataRemote r)


        let saveChartsImpl p =
            match i with
            | LocalSolverRunner c -> ignore()
            | RemoteSolverRunner c -> tryFun (saveChartsRemote p) |> ignore


        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
        member __.saveResultData r = saveResultDataImpl r
        member __.saveCharts (p : List<string>) = saveChartsImpl p

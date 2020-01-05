namespace ServiceProxy

open ClmSys.GeneralData
open ClmSys.Registry
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralErrors

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


    type SolverRunnerProxy =
        {
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            saveResultData : ResultDataWithId -> UnitResult
            saveChartInfo : ChartInfo -> UnitResult
        }

        static member create (i : SolverRunnerProxyInfo) =
            let tryLoadModelDataImpl a m =
                match i with
                | LocalSolverRunner c -> tryLoadModelData c.connectionString a m
                | RemoteSolverRunner _ -> tryLoadModelDataFs solverRunnerName m

            let saveResultDataImpl r =
                match i with
                | LocalSolverRunner c -> saveResultData c.connectionString r
                | RemoteSolverRunner _ -> trySaveResultDataFs solverRunnerName r

            let saveChartInfoImpl r =
                match i with
                | LocalSolverRunner _ -> Ok()
                | RemoteSolverRunner _ -> trySaveChartInfoFs solverRunnerName r

            {
                tryLoadModelData = tryLoadModelDataImpl
                saveResultData = saveResultDataImpl
                saveChartInfo = saveChartInfoImpl
            }

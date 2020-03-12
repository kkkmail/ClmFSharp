﻿namespace ServiceProxy

open ClmSys.Registry
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerData
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.WorkerNodeData

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
            loadModelData : NodeServiceAccessInfo -> ModelDataId -> ClmResult<ModelData>
            saveResultData : ResultDataWithId -> UnitResult
            saveChartInfo : ChartInfo -> UnitResult
        }

        static member create (i : SolverRunnerProxyInfo) =
            let loadModelDataImpl a m =
                match i with
                | LocalSolverRunner c -> loadModelData c.connectionString m
                | RemoteSolverRunner _ -> loadModelDataFs solverRunnerName m

            let saveResultDataImpl r =
                match i with
                | LocalSolverRunner c -> saveResultData c.connectionString r
                | RemoteSolverRunner _ -> saveResultDataFs solverRunnerName r

            let saveChartInfoImpl r =
                match i with
                | LocalSolverRunner _ -> Ok()
                | RemoteSolverRunner _ -> saveChartInfoFs solverRunnerName r

            {
                loadModelData = loadModelDataImpl
                saveResultData = saveResultDataImpl
                saveChartInfo = saveChartInfoImpl
            }

namespace ServiceProxy

open ClmSys.Registry
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ContGenServiceInfo.ServiceInfo
open ClmSys.SolverRunnerErrors

module SolverRunner =

    type SolverRunnerProxy =
        {
            updateProgress : ProgressUpdateInfo -> UnitResult
            saveResult : ResultDataWithId -> UnitResult
            saveCharts : ChartGenerationResult -> UnitResult
            logCrit : SolverRunnerCriticalError -> UnitResult
        }

        //static member create (i : SolverRunnerProxyInfo) =
        //    let loadModelDataImpl a m =
        //        match i with
        //        | LocalSolverRunner c -> loadModelData c.connectionString m
        //        | RemoteSolverRunner _ -> loadModelDataFs solverRunnerName m

        //    let saveResultDataImpl r =
        //        match i with
        //        | LocalSolverRunner c -> saveResultData c.connectionString r
        //        | RemoteSolverRunner _ -> saveResultDataFs solverRunnerName r

        //    let saveChartInfoImpl r =
        //        match i with
        //        | LocalSolverRunner _ -> Ok()
        //        | RemoteSolverRunner _ -> saveChartInfoFs solverRunnerName r

        //    {
        //        loadModelData = loadModelDataImpl
        //        saveResultData = saveResultDataImpl
        //        saveChartInfo = saveChartInfoImpl
        //    }

namespace ServiceProxy

open Clm.ModelParams
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

﻿namespace ServiceProxy

open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ContGenServiceInfo.ServiceInfo
open ClmSys.SolverRunnerErrors
open ClmSys.GeneralPrimitives

module SolverRunner =

    type SolverRunnerProxy =
        {
            updateProgress : ProgressUpdateInfo -> UnitResult
            transmitMessages : UnitResult -> UnitResult
            saveResult : ResultDataWithId -> UnitResult
            saveCharts : ChartGenerationResult -> UnitResult
            logCrit : SolverRunnerCriticalError -> UnitResult
            checkCancellation : RunQueueId -> bool
        }

namespace ClmSys

open GeneralPrimitives
open ContGenPrimitives

module ModelRunnerErrors =

    type RunModelError =
        | MissingWorkerNodeErr of RunQueueId
        | UnableToLoadModelDataErr of RunQueueId * ModelDataId


    type TryRunFirstModelError =
        | TryLoadFirstRunQueueErr
        | TryGetAvailableWorkerNodeErr
        | UpsertRunQueueErr
        | UnableToRunModelErr


    type TryRunAllModelsError =
        | UnableTotryRunFirstModelErr


    type ModelRunnerError =
        | RunModelErr of RunModelError
        | TryRunFirstModelErr of TryRunFirstModelError
        | TryRunAllModelsErr of TryRunAllModelsError

namespace ClmSys

open GeneralPrimitives
open ContGenPrimitives
open WorkerNodePrimitives

module ModelRunnerErrors =

    type RunModelError =
        | MissingWorkerNodeErr of RunQueueId
        | UnableToLoadModelDataErr of RunQueueId * ModelDataId


    type TryRunFirstModelError =
        | TryLoadFirstRunQueueErr
        | TryGetAvailableWorkerNodeErr
        | UpsertRunQueueErr
        | UnableToRunModelErr
        | UnableToRunModelAndUpsertStatusErr


    type TryRunAllModelsError =
        | UnableToTryRunFirstModelErr


    type UpdateProgressError =
        | UnableToLoadRunQueueErr of RunQueueId
        | UnableToFindLoadRunQueueErr of RunQueueId
        | InvalidRunQueueStatusErr of RunQueueId


    type RegisterError =
        | UnableToUpsertWorkerNodeInfoErr of WorkerNodeId


    type UnregisterError =
        | UnableToLoadWorkerNodeInfoErr of WorkerNodeId
        | UnableToUpsertWorkerNodeInfoOnUnregisterErr of WorkerNodeId


    type ModelRunnerError =
        | RunModelErr of RunModelError
        | TryRunFirstModelErr of TryRunFirstModelError
        | TryRunAllModelsErr of TryRunAllModelsError
        | UpdateProgressErr of UpdateProgressError
        | RegisterErr of RegisterError
        | UnregisterErr of UnregisterError

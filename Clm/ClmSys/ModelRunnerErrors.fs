namespace ClmSys

open GeneralPrimitives
open ContGenPrimitives

module ModelRunnerErrors =

    type RunModelError =
        | MissingWorkerNodeErr of RunQueueId
        | UnableToLoadModeldata of RunQueueId * ModelDataId


    type ModelRunnerError =
        | RunModelErr of RunModelError


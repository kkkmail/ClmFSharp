namespace ClmSys

open System
open ContGenPrimitives

module RunnerErrors =

    type GenerateImplError =
        | SaveModelErr of ModelDataId
        | LoadParametersErr of ModelDataId
        | GetQueueIdErr of ModelDataId
        | GenerateModelExn of exn

    type RunRunnerModelError =
        | InvalidDataErr of ModelDataId


    type RunnerError =
        | GenerateImplErr of GenerateImplError
        | RunRunnerModelErr of RunRunnerModelError


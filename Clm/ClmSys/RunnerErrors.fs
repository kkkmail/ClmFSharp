namespace ClmSys

open ContGenPrimitives

module RunnerErrors =

    type GenerateImplError =
        | SaveModelErr of ModelDataId
        | LoadParametersErr of ModelDataId
        | GetQueueIdErr of ModelDataId
        | GenerateModelExn of exn


    type RunnerError =
        | GenerateImplErr of GenerateImplError


namespace ClmSys

open ContGenPrimitives

module RunnerErrors =

    type GenerateImplError =
        | GenerateModelErr of ModelDataId


    type RunnerError =
        | GenerateImplErr of GenerateImplError


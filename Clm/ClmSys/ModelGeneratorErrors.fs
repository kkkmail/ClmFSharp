namespace ClmSys

open ClmSys.ContGenPrimitives

module ModelGeneratorErrors =


    type GenerateModelError =
        | UnableLoadParamsErr of ClmTaskId
        | UnableUpsertModelDataErr of ClmTaskId
        | TaskCompletedErr of ClmTaskId


    type ModelGeneratorError =
        | GenerateModelErr of GenerateModelError

namespace ClmSys

open ClmSys.ContGenPrimitives

module ModelGeneratorErrors =


    type GenerateModelError =
        | UnableLoadParamsErr of ClmTaskId
        | UnableUpsertModelData of ClmTaskId


    type ModelGeneratorError =
        | GenerateModelErr of GenerateModelError

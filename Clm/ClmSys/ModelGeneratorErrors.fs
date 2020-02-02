namespace ClmSys

module ModelGeneratorErrors =


    type OnGenerateModelsError =
        | GenerationFailedErr


    type ModelGeneratorError =
        | OnGenerateModelsErr of OnGenerateModelsError

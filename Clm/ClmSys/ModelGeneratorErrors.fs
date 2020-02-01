namespace ClmSys

module ModelGeneratorErrors =


    type OnGenerateModelsError =
        | X


    type ModelGeneratorError =
        | OnGenerateModelsErr of OnGenerateModelsError

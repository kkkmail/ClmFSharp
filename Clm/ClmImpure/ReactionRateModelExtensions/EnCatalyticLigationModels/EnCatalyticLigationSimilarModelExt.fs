namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationRandomModelExt

module EnCatalyticLigationSimilarModelExt =

    type EnCatalyticLigationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | EnCatalyticLigationRateParam (CatLigSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel (CatLigSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : EnCatalyticLigationSimilarParam) =
                {
                    enCatLigModel = b
                    peptideBondData = a
                    enCatLigSimParam = d.catLigSimParam
                }
                |> EnCatalyticLigationSimilarModel |> EnCatLigSimModel |> EnCatalyticLigationRateModel

            tryCreateModelWithBase EnCatalyticLigationSimilarModel.paramGetter creator EnCatalyticLigationRandomModel.modelGetter EnCatalyticLigationRandomModel.tryCreate (p, m)

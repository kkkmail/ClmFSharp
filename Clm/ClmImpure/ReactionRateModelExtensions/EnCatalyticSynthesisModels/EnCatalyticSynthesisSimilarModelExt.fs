namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.EnCatalyticSynthesisRandomModelExt

module EnCatalyticSynthesisSimilarModelExt =

    type EnCatalyticSynthesisSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | EnCatalyticSynthesisRateParam (EnCatSynthSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticSynthesisRateModel (EnCatSynthSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : EnCatalyticSynthesisSimilarParam) = { enCatSynthModel = b; aminoAcids = a; enCatSynthSimParam = d.enCatSynthSimParam } |> CatalyticSynthesisSimilarModel |> CatSynthSimModel |> CatalyticSynthesisRateModel
            tryCreateModelWithBase EnCatalyticSynthesisSimilarModel.paramGetter creator EnCatalyticSynthesisRandomModel.modelGetter EnCatalyticSynthesisRandomModel.tryCreate (p, m)

namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt

module EnCatalyticLigationRandomModelExt =

    type EnCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel (CatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { ligationModel = b; enCatLigationParam = d } |> EnCatalyticLigationRandomModel |> EnCatLigRndModel |> EnCatalyticLigationRateModel
            tryCreateModelWithBase EnCatalyticLigationRandomParam.paramGetter creator LigationModel.modelGetter LigationModel.tryCreate (p, m)

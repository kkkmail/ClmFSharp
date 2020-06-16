namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open Clm.CalculationData
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationSimilarModelExt

module EnCatalyticLigationModelExt =

    type EnCatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> EnCatalyticLigationRandomModel.tryCreate
            |> EnCatalyticLigationSimilarModel.tryCreate (si.ligationReactions |> PeptideBondData.create)

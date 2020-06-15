namespace ClmImpure

open Clm.ReactionRates
open Clm.ReactionRatesExt
open Clm.CalculationData
open Clm.ReactionTypes
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModels.SedimentationAllModel

module ReactionRateModelsExt =

    type ReactionRateModel
        with

        static member createAll (p : list<ReactionRateModelParamWithUsage>) (si : SubstInfo) =
            (
                [
                    FoodCreationModel.tryCreate
                    WasteRemovalModel.tryCreate
                    WasteRecyclingModel.tryCreate
                    SynthesisModel.tryCreate
                    SugarSynthesisModel.tryCreate
                    DestructionModel.tryCreate
                    CatalyticSynthesisModel.tryCreate si
                    EnCatalyticSynthesisModel.tryCreate si
                    CatalyticDestructionModel.tryCreate si
                    LigationModel.tryCreate
                    CatalyticLigationModel.tryCreate si
                    EnCatalyticLigationModel.tryCreate si
                    SedimentationDirectModel.tryCreate si
                    SedimentationAllModel.tryCreate
                    RacemizationModel.tryCreate
                    CatalyticRacemizationModel.tryCreate si
                ]
                |> List.fold (fun acc r -> r acc) (p, [])
            )
            |> snd

﻿namespace ClmImpure

open Clm.ReactionRates
open ClmImpure.ReactionRateModels.FoodCreationModel
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModels.WasteRemovalModel
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

module ReactionRateModels =

    type ReactionRateModel =
        | FoodCreationRateModel of FoodCreationModel
        | WasteRemovalRateModel of WasteRemovalModel
        | WasteRecyclingRateModel of WasteRecyclingModel
        | SynthesisRateModel of SynthesisModel
        | SugarSynthesisRateModel of SugarSynthesisModel
        | DestructionRateModel of DestructionModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | EnCatalyticSynthesisRateModel of EnCatalyticSynthesisModel
        | CatalyticDestructionRateModel of CatalyticDestructionModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | EnCatalyticLigationRateModel of EnCatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel
        | RacemizationRateModel of RacemizationModel
        | CatalyticRacemizationRateModel of CatalyticRacemizationModel


        member rm.getAllRates() =
            match rm with
            | FoodCreationRateModel m -> m.getAllRates() |> FoodCreationRates
            | WasteRemovalRateModel m -> m.getAllRates() |> WasteRemovalRates
            | WasteRecyclingRateModel m -> m.getAllRates() |> WasteRecyclingRates
            | SynthesisRateModel m -> m.getAllRates() |> SynthesisRates
            | DestructionRateModel m -> m.getAllRates() |> DestructionRates
            | CatalyticSynthesisRateModel m -> m.getAllRates() |> CatalyticSynthesisRates
            | CatalyticDestructionRateModel m -> m.getAllRates() |> CatalyticDestructionRates
            | LigationRateModel m -> m.getAllRates() |> LigationRates
            | CatalyticLigationRateModel m -> m.getAllRates() |> CatalyticLigationRates
            | SedimentationDirectRateModel m -> m.getAllRates() |> SedimentationDirectRates
            | SedimentationAllRateModel m -> m.getAllRates() |> SedimentationAllRates
            | RacemizationRateModel m -> m.getAllRates() |> RacemizationRates
            | CatalyticRacemizationRateModel m -> m.getAllRates() |> CatalyticRacemizationRates


    type ReactionRateModelWithUsage =
        {
            model : ReactionRateModel
            usage : ReactionRateModelParamUsage
        }

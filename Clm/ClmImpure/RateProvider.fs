namespace ClmImpure

open System
open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionTypes
open Clm.ReactionRates

open ClmImpure.ReactionRateFunctions
open Clm.Substances
open Clm.ReactionRates
open Clm.ReactionRatesExt

open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModelsExt

module RateProvider =

    type ReactionRateProvider (p: ReactionRateProviderParams, n : NumberOfAminoAcids) =
        let allModels = ReactionRateModel.createAll p.allParams n
        let tryPick getter = allModels |> List.tryPick getter

        let getRatesImpl rnd t a =
            match a with
            | FoodCreation r -> tryPick FoodCreationModel.modelGetter |> bind (fun m -> m.getRates r)
            | WasteRemoval r -> tryPick WasteRemovalModel.modelGetter |> bind (fun m -> m.getRates r)
            | WasteRecycling r -> tryPick WasteRecyclingModel.modelGetter |> bind (fun m -> m.getRates r)
            | Synthesis r -> tryPick SynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | Destruction r -> tryPick DestructionModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | CatalyticSynthesis r -> tryPick CatalyticSynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | CatalyticDestruction r -> tryPick CatalyticDestructionModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | Ligation r -> tryPick LigationModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | CatalyticLigation r -> tryPick CatalyticLigationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | SedimentationDirect r -> tryPick SedimentationDirectModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | SedimentationAll r -> tryPick SedimentationAllModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | Racemization r -> tryPick RacemizationModel.modelGetter|> bind (fun m -> m.getRates rnd r)
            | CatalyticRacemization r -> tryPick CatalyticRacemizationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)


        //let getRatesImpl rnd t a =
        //    match a with
        //    | FoodCreation r -> p.tryFindFoodCreationParam() |> bind (fun m -> m.getRates r)
        //    | WasteRemoval r -> p.tryFindWasteRemovalModel() |> bind (fun m -> m.getRates r)
        //    | WasteRecycling r -> p.tryFindWasteRecyclingModel() |> bind (fun m -> m.getRates r)
        //    | Synthesis r -> p.tryFindSynthesisModel() |> bind (fun m -> m.getRates rnd r)
        //    | Destruction r -> p.tryFindDestructionModel() |> bind (fun m -> m.getRates rnd r)
        //    | CatalyticSynthesis r -> p.tryFindCatalyticSynthesisModel() |> bind (fun m -> m.getRates rnd t r)
        //    | CatalyticDestruction r -> p.tryFindCatalyticDestructionModel() |> bind (fun m -> m.getRates rnd t r)
        //    | Ligation r -> p.tryFindLigationModel() |> bind (fun m -> m.getRates rnd r)
        //    | CatalyticLigation r -> p.tryFindCatalyticLigationModel() |> bind (fun m -> m.getRates rnd t r)
        //    | SedimentationDirect r -> p.tryFindSedimentationDirectModel() |> bind (fun m -> m.getRates rnd t r)
        //    | SedimentationAll r -> p.tryFindSedimentationAllModel() |> bind (fun m -> m.getRates rnd r)
        //    | Racemization r -> p.tryFindRacemizationModel() |> bind (fun m -> m.getRates rnd r)
        //    | CatalyticRacemization r -> p.tryFindCatalyticRacemizationModel() |> bind (fun m -> m.getRates rnd t r)

        //let getModelImpl n =
        //    match n with
        //    | FoodCreationName -> p.tryFindFoodCreationModel() |> Option.bind(fun e -> FoodCreationRateModel e |> Some)
        //    | WasteRemovalName -> p.tryFindWasteRemovalModel() |> Option.bind(fun e -> WasteRemovalRateModel e |> Some)
        //    | WasteRecyclingName -> p.tryFindWasteRecyclingModel() |> Option.bind(fun e -> WasteRecyclingRateModel e |> Some)
        //    | SynthesisName -> p.tryFindSynthesisModel() |> Option.bind(fun e -> SynthesisRateModel e |> Some)
        //    | DestructionName -> p.tryFindDestructionModel() |> Option.bind(fun e -> DestructionRateModel e |> Some)
        //    | CatalyticSynthesisName -> p.tryFindCatalyticSynthesisModel() |> Option.bind(fun e -> CatalyticSynthesisRateModel e |> Some)
        //    | CatalyticDestructionName -> p.tryFindCatalyticDestructionModel() |> Option.bind(fun e -> CatalyticDestructionRateModel e |> Some)
        //    | LigationName -> p.tryFindLigationModel() |> Option.bind(fun e -> LigationRateModel e |> Some)
        //    | CatalyticLigationName -> p.tryFindCatalyticLigationModel() |> Option.bind(fun e -> CatalyticLigationRateModel e |> Some)
        //    | SedimentationDirectName -> p.tryFindSedimentationDirectModel() |> Option.bind(fun e -> SedimentationDirectRateModel e |> Some)
        //    | SedimentationAllName -> p.tryFindSedimentationAllModel() |> Option.bind(fun e -> SedimentationAllRateModel e |> Some)
        //    | RacemizationName -> p.tryFindRacemizationModel() |> Option.bind(fun e -> RacemizationRateModel e |> Some)
        //    | CatalyticRacemizationName -> p.tryFindCatalyticRacemizationModel() |> Option.bind(fun e -> CatalyticRacemizationRateModel e |> Some)

        let tryGetModelImpl n =
            match n with
            | FoodCreationName -> tryPick FoodCreationModel.modelGetter |> Option.bind(fun e -> FoodCreationRateModel e |> Some)
            | WasteRemovalName -> tryPick WasteRemovalModel.modelGetter |> Option.bind(fun e -> WasteRemovalRateModel e |> Some)
            | WasteRecyclingName -> tryPick WasteRecyclingModel.modelGetter |> Option.bind(fun e -> WasteRecyclingRateModel e |> Some)
            | SynthesisName -> tryPick SynthesisModel.modelGetter |> Option.bind(fun e -> SynthesisRateModel e |> Some)
            | DestructionName -> tryPick DestructionModel.modelGetter |> Option.bind(fun e -> DestructionRateModel e |> Some)
            | CatalyticSynthesisName -> tryPick CatalyticSynthesisModel.modelGetter |> Option.bind(fun e -> CatalyticSynthesisRateModel e |> Some)
            | CatalyticDestructionName -> tryPick CatalyticDestructionModel.modelGetter |> Option.bind(fun e -> CatalyticDestructionRateModel e |> Some)
            | LigationName -> tryPick LigationModel.modelGetter |> Option.bind(fun e -> LigationRateModel e |> Some)
            | CatalyticLigationName -> tryPick CatalyticLigationModel.modelGetter |> Option.bind(fun e -> CatalyticLigationRateModel e |> Some)
            | SedimentationDirectName -> tryPick SedimentationDirectModel.modelGetter |> Option.bind(fun e -> SedimentationDirectRateModel e |> Some)
            | SedimentationAllName -> tryPick SedimentationAllModel.modelGetter |> Option.bind(fun e -> SedimentationAllRateModel e |> Some)
            | RacemizationName -> tryPick RacemizationModel.modelGetter |> Option.bind(fun e -> RacemizationRateModel e |> Some)
            | CatalyticRacemizationName ->tryPick CatalyticRacemizationModel.modelGetter |> Option.bind(fun e -> CatalyticRacemizationRateModel e |> Some)

        member __.providerParams = p
        member __.getRates rnd a = getRatesImpl rnd a
        member __.tryGetModel n = tryGetModelImpl n
        //member __.getAllRates() = p.rateModels |> List.map (fun m -> m.getAllRates())
        member __.getAllRates() = allModels |> List.map (fun m -> m.model.getAllRates())

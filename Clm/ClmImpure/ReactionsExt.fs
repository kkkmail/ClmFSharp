namespace ClmImpure

open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRates
open ReactionRateModels
open RateProvider

module ReactionsExt =

    type ReversibleReaction
        with

        static member tryCreate rnd (g : ReactionRateProvider) t i =
            g.getRates rnd t i |> ReversibleReaction.tryCreateFromRateData i


    type AnyReaction
        with

        static member tryCreateReaction rnd g t i =
            match ReversibleReaction.tryCreate rnd g t i with
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None


    type ReactionRateModel
        with

        member rm.getAllReactions() : list<AnyReaction> =
            let hasData (rd : RateData) =
                match rd.forwardRate, rd.backwardRate with
                | None, None ->
                    false
                | _ ->
                    true

            let createReactions creator ar =
                let x0 =
                    ar

                let x1 =
                    x0
                    |> List.filter (fun e -> hasData e.rateData)

                let x2 = 
                    x1
                    |> List.map (fun e -> AnyReaction.tryCreateReactionFromRateData (e.reaction |> creator) e.rateData)

                let x3 =
                    x2
                    |> List.choose id

                let x4 =
                    x3
                    |> List.concat

                x4

            match rm with
            | FoodCreationRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> FoodCreation))
            | WasteRemovalRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> WasteRemoval))
            | WasteRecyclingRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> WasteRecycling))
            | SynthesisRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> Synthesis))
            | DestructionRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> Destruction))
            | CatalyticSynthesisRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> CatalyticSynthesis))
            | CatalyticDestructionRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> CatalyticDestruction))
            | LigationRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> Ligation))
            | CatalyticLigationRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> CatalyticLigation))
            | SedimentationDirectRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> SedimentationDirect))
            | SedimentationAllRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> SedimentationAll))
            | RacemizationRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> Racemization))
            | CatalyticRacemizationRateModel m -> m.getAllRates() |> (createReactions (fun e -> e |> CatalyticRacemization))

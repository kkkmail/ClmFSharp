namespace Clm

open Clm.Distributions
open Clm.ReactionTypes
open Clm.ReactionRates

module Reactions =

    type ForwardReaction =
        {
            reaction : Reaction
            forwardRate : ReactionRate
        }

        member reaction.enantiomer = { reaction with reaction = reaction.reaction.enantiomer }

        static member tryCreate g i =
            match g i with 
            | Some f ->
                {
                    reaction = i
                    forwardRate = f
                } 
                |> Forward
                |> Some
            | None -> None


    and BackwardReaction =
        {
            reaction : Reaction
            backwardRate : ReactionRate
        }

        member reaction.enantiomer = { reaction with reaction = reaction.reaction.enantiomer }

        static member tryCreate g i = 
            match g i with 
            | Some b ->
                {
                    reaction = i
                    backwardRate = b
                } 
                |> Backward
                |> Some
            | None -> None


    and ReversibleReaction =
        {
            reaction : Reaction
            forwardRate : ReactionRate
            backwardRate : ReactionRate
        }

        member r.enantiomer = { r with reaction = r.reaction.enantiomer }

        static member tryCreateFromRateData i (rd : RateData) =
            match rd.forwardRate, rd.backwardRate with
            | Some f, Some b ->
                {
                    reaction = i
                    forwardRate = f
                    backwardRate = b
                } 
                |> Reversible
                |> Some
            | Some f, None -> ForwardReaction.tryCreate (fun _ -> Some f) i
            | None, Some b -> BackwardReaction.tryCreate (fun _ -> Some b) i
            | None, None -> None

        static member tryCreate (g : ReactionRateProvider) t i =
            g.getRates t i |> ReversibleReaction.tryCreateFromRateData i


    and AnyReaction =
        | Forward of ForwardReaction
        | Backward of BackwardReaction
        | Reversible of ReversibleReaction

        member reaction.enantiomer =
            match reaction with
            | Forward r -> r.enantiomer |> Forward
            | Backward r -> r.enantiomer |> Backward
            | Reversible r -> r.enantiomer |> Reversible

        member this.name =
            match this with
            | Forward r -> r.reaction.name
            | Backward r -> r.reaction.name
            | Reversible r -> r.reaction.name

        member this.fullName =
            let a, i, n =
                match this with
                | Forward r -> " -> ", r.reaction, r.reaction.name.name
                | Backward r -> " <- ", r.reaction, r.reaction.name.name
                | Reversible r -> " <-> ", r.reaction, r.reaction.name.name

            i.info.getName n a

        member this.reaction =
            match this with
            | Forward r -> r.reaction
            | Backward r -> r.reaction
            | Reversible r -> r.reaction

        member this.forwardRate =
            match this with
            | Forward r -> Some r.forwardRate
            | Backward r -> None
            | Reversible r -> Some r.forwardRate

        member this.backwardRate =
            match this with
            | Forward r -> None
            | Backward r -> Some r.backwardRate
            | Reversible r -> Some r.backwardRate

        static member tryCreateReactionFromRateData i (rd : RateData) =
            match ReversibleReaction.tryCreateFromRateData i rd with
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None

        static member tryCreateReaction g t i =
            match ReversibleReaction.tryCreate g t i with
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

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

        static member tryCreate (g : ReactionRateProvider) t i =
            let r = g.getRates t i
            match r.forwardRate, r.backwardRate with
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
            | Forward r ->  r.reaction.name
            | Backward r -> r.reaction.name
            | Reversible r -> r.reaction.name

        member this.fullName =
            let a, i, n =
                match this with
                | Forward r -> " -> ", r.reaction, r.reaction.name.name
                | Backward r -> " <- ", r.reaction, r.reaction.name.name
                | Reversible r -> " <-> ", r.reaction, r.reaction.name.name

            i.info.getName n a

        static member tryCreateReaction g t i =
            match ReversibleReaction.tryCreate g t i with
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None


    type ReactionRateModel
        with

        member rm.getAllReactions() : list<AnyReaction> =
            match rm with
            | FoodCreationRateModel m ->
                let x = m.getAllRates()

                failwith ""

            //| WasteRemovalRateModel m -> m.getAllRates() |> WasteRemovalRates
            //| WasteRecyclingRateModel m -> m.getAllRates() |> WasteRecyclingRates
            //| SynthesisRateModel m -> m.getAllRates() |> SynthesisRates
            //| DestructionRateModel m -> m.getAllRates() |> DestructionRates
            //| CatalyticSynthesisRateModel m -> m.getAllRates() |> CatalyticSynthesisRates
            //| CatalyticDestructionRateModel m -> m.getAllRates() |> CatalyticDestructionRates
            //| LigationRateModel m -> m.getAllRates() |> LigationRates
            //| CatalyticLigationRateModel m -> m.getAllRates() |> CatalyticLigationRates
            //| SedimentationDirectRateModel m -> m.getAllRates() |> SedimentationDirectRates
            //| SedimentationAllRateModel m -> m.getAllRates() |> SedimentationAllRates
            //| RacemizationRateModel m -> m.getAllRates() |> RacemizationRates
            //| CatalyticRacemizationRateModel m -> m.getAllRates() |> CatalyticRacemizationRates

            | WasteRemovalRateModel m -> failwith ""
            | WasteRecyclingRateModel m -> failwith ""
            | SynthesisRateModel m -> failwith ""
            | DestructionRateModel m -> failwith ""
            | CatalyticSynthesisRateModel m -> failwith ""
            | CatalyticDestructionRateModel m -> failwith ""
            | LigationRateModel m -> failwith ""
            | CatalyticLigationRateModel m -> failwith ""
            | SedimentationDirectRateModel m -> failwith ""
            | SedimentationAllRateModel m -> failwith ""
            | RacemizationRateModel m -> failwith ""
            | CatalyticRacemizationRateModel m -> failwith ""

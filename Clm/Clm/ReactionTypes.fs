namespace Clm

open Substances

module ReactionTypes = 

    type ReactionName = 
        | FoodCreationName
        | WasteRemovalName
        | SynthesisName
        | CatalyticSynthesisName
        | LigationName
        | CatalyticLigationName
        | SedimentationDirectName
        | SedimentationAllName
        | RacemizationName
        | CatalyticRacemizationName

        member this.name =
            match this with 
            | FoodCreationName -> "food"
            | WasteRemovalName -> "waste"
            | SynthesisName -> "synthesis"
            | CatalyticSynthesisName -> "catalytic synthesis"
            | LigationName -> "ligation"
            | CatalyticLigationName -> "catalytic ligation"
            | SedimentationDirectName -> "sedimentation direct"
            | SedimentationAllName -> "sedimentation all"
            | RacemizationName -> "racemization"
            | CatalyticRacemizationName -> "catalytic racemization"

        static member all = 
            [
                FoodCreationName
                WasteRemovalName
                SynthesisName
                CatalyticSynthesisName
                LigationName
                CatalyticLigationName
                SedimentationDirectName
                SedimentationAllName
                RacemizationName
                CatalyticRacemizationName
            ]


    type ReactionInfo =
        {
            reactionName : ReactionName
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member this.getName a = 
            let g (l : list<Substance * int>) = 
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            this.reactionName.name + ": " + (g this.input) + a + (g this.output)


    type SynthesisReaction = 
        | SynthesisReact of ChiralAminoAcid
        | DescructionReac of ChiralAminoAcid

        member r.input = 
            match r with 
            | SynthesisReact a -> (Simple Food, 1)
            | DescructionReac a -> (Chiral a, 1)

        member r.output = 
            match r with 
            | SynthesisReact a -> (Chiral a, 1)
            | DescructionReac a -> (Simple Waste, 1)

        member r.info = 
            {
                reactionName = ReactionName.SynthesisName
                input = [ r.input ]
                output = [ r.output ]
            }

            //match r with 
            //| SynthesisReact a -> 
            //    {
            //        reactionName = ReactionName.SynthesisName
            //        input = [ (Simple Food, 1) ]
            //        output = [ (Chiral a, 1) ]
            //    }
            //| DescructionReac a ->
            //    {
            //        reactionName = ReactionName.SynthesisName
            //        input = [ (Chiral a, 1) ]
            //        output = [ (Simple Waste, 1) ]
            //    }

        member r.enantiomer = 
            match r with 
            | SynthesisReact a -> a.enantiomer |> SynthesisReact
            | DescructionReac a -> a.enantiomer |> DescructionReac


    type SynthCatalyst = 
        | SynthCatalyst of Peptide

        member c.enantiomer = 
            let (SynthCatalyst a) = c
            a.enantiomer |> SynthCatalyst


    type CatalyticSynthesisReaction = 
        | CatalyticSynthesisReaction of (SynthesisReaction * SynthCatalyst)

        member r.info = 
            let (CatalyticSynthesisReaction (a, (SynthCatalyst c))) = r
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticSynthesisName
                input = [ a.input; (p, 1) ]
                output = [ a.output; (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticSynthesisReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticSynthesisReaction


    type LigationReaction = 
        | LigationReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member r.info = 
            let (LigationReaction (a, b)) = r

            {
                reactionName = ReactionName.LigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (Substance.fromList (a @ b), 1) ]
            }

        member r.enantiomer = 
            let (LigationReaction (a, b)) = r
            (a |> List.map (fun e -> e.enantiomer), b |> List.map (fun e -> e.enantiomer)) |> LigationReaction


    type LigCatalyst =
        | LigCatalyst of Peptide

        member c.enantiomer = 
            let (LigCatalyst a) = c
            a.enantiomer |> LigCatalyst


    type CatalyticLigationReaction = 
        | CatalyticLigationReaction of (LigationReaction * LigCatalyst)

        member r.info = 
            let (CatalyticLigationReaction (LigationReaction (a, b), LigCatalyst c)) = r

            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticLigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1); (p, 1) ]
                output = [ (Substance.fromList (a @ b), 1); (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticLigationReaction (l, c)) = r
            (l.enantiomer, c.enantiomer) |> CatalyticLigationReaction


    type SedimentationDirectReaction = 
        | SedimentationDirectReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member r.info = 
            let (SedimentationDirectReaction (a, b)) = r

            {
                reactionName = ReactionName.SedimentationDirectName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (AchiralSubst.Waste |> Simple, a.Length + b.Length) ]
            }

        member r.enantiomer = 
            let (SedimentationDirectReaction (a, b)) = r
            (a |> List.map (fun e -> e.enantiomer), b |> List.map (fun e -> e.enantiomer)) |> SedimentationDirectReaction


    type SedimentationAllReaction = 
        | SedimentationAllReaction

        member r.info = 
            {
                reactionName = ReactionName.SedimentationAllName
                input = []
                output = []
            }

        member r.enantiomer = r


    type RacemizationReaction = 
        | RacemizationReaction of ChiralAminoAcid

        member r.info = 
            let (RacemizationReaction a) = r
            {
                reactionName = ReactionName.RacemizationName
                input = [ (Chiral a, 1) ]
                output = [ (Chiral a.enantiomer, 1) ]
            }

        member r.enantiomer = 
            let (RacemizationReaction a) = r
            a.enantiomer |> RacemizationReaction


    type RacemizationCatalyst = 
        | RacemizationCatalyst of Peptide

        member c.enantiomer = 
            let (RacemizationCatalyst a) = c
            a.enantiomer |> RacemizationCatalyst


    type CatalyticRacemizationReaction = 
        | CatalyticRacemizationReaction of (RacemizationReaction * RacemizationCatalyst)

        member r.info = 
            let (CatalyticRacemizationReaction ((RacemizationReaction a), (RacemizationCatalyst c))) = r
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticRacemizationName
                input = [ (Chiral a, 1); (p, 1) ]
                output = [ (Chiral a.enantiomer, 1); (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticRacemizationReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticRacemizationReaction


    type Reaction = 
        | Synthesis of SynthesisReaction
        | CatalyticSynthesis of CatalyticSynthesisReaction
        | Ligation of LigationReaction
        | CatalyticLigation of CatalyticLigationReaction
        | SedimentationDirect of SedimentationDirectReaction
        | SedimentationAll of SedimentationAllReaction
        | Racemization of RacemizationReaction
        | CatalyticRacemization of CatalyticRacemizationReaction

        member r.name = 
            match r with 
            | Synthesis _ -> SynthesisName
            | CatalyticSynthesis _ -> CatalyticSynthesisName
            | Ligation _ -> LigationName
            | CatalyticLigation _ -> CatalyticLigationName
            | SedimentationDirect _ -> SedimentationDirectName
            | SedimentationAll _ -> SedimentationAllName
            | Racemization _ -> RacemizationName
            | CatalyticRacemization _ -> CatalyticRacemizationName

        member r.info = 
            match r with 
            | Synthesis r -> r.info
            | CatalyticSynthesis r -> r.info
            | Ligation r -> r.info
            | CatalyticLigation r -> r.info
            | SedimentationDirect r -> r.info
            | SedimentationAll r -> r.info
            | Racemization r -> r.info
            | CatalyticRacemization r -> r.info

        member r.enantiomer = 
            match r with 
            | Synthesis r -> r.enantiomer |> Synthesis
            | CatalyticSynthesis r -> r.enantiomer |> CatalyticSynthesis
            | Ligation r -> r.enantiomer |> Ligation
            | CatalyticLigation r -> r.enantiomer |> CatalyticLigation
            | SedimentationDirect r -> r.enantiomer |> SedimentationDirect
            | SedimentationAll r -> SedimentationAll r // There are no enantiomers here.
            | Racemization r -> r.enantiomer |> Racemization
            | CatalyticRacemization r -> r.enantiomer |> CatalyticRacemization

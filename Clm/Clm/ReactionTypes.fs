namespace Clm

open Substances

module ReactionTypes = 

    type ReactionName = 
        | SynthesisName
        | CatalyticSynthesisName
        | LigationName
        | CatalyticLigationName
        | SedimentationDirectName
        | SedimentationAllName

        member this.name =
            match this with 
            | SynthesisName -> "synthesis"
            | CatalyticSynthesisName -> "catalytic synthesis"
            | LigationName -> "ligation"
            | CatalyticLigationName -> "catalytic ligation"
            | SedimentationDirectName -> "sedimentation direct"
            | SedimentationAllName -> "sedimentation all"

        static member all = 
            [
                SynthesisName
                CatalyticSynthesisName
                LigationName
                CatalyticLigationName
                SedimentationDirectName
                SedimentationAllName
            ]


    type ReactionInfo =
        {
            reactionName : ReactionName
            input : list<Substance * int>
            output : list<Substance * int>
        }

        //member info.enantiomer = 
        //    let e (i : list<Substance * int>) = i |> List.map (fun (s, n) -> (s.enantiomer, n))

        //    {
        //        reactionName = info.reactionName
        //        input = info.input |> e
        //        output = info.output |> e
        //    }

        member this.getName a = 
            let g (l : list<Substance * int>) = 
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            this.reactionName.name + ": " + (g this.input) + a + (g this.output)


    type SynthesisReaction = 
        | SynthesisReaction of ChiralAminoAcid

        member r.info = 
            let (SynthesisReaction a) = r
            {
                reactionName = ReactionName.SynthesisName
                input = [ (Substance.food, 1) ]
                output = [ (Chiral a, 1) ]
            }

        member r.enantiomer = 
            let (SynthesisReaction a) = r
            a.enantiomer |> SynthesisReaction


    type SynthCatalyst = 
        | SynthCatalyst of Peptide

        member c.enantiomer = 
            let (SynthCatalyst a) = c
            a.enantiomer |> SynthCatalyst


    type CatalyticSynthesisReaction = 
        | CatalyticSynthesisReaction of (SynthesisReaction * SynthCatalyst)

        member r.info = 
            let (CatalyticSynthesisReaction ((SynthesisReaction a), (SynthCatalyst c))) = r
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticSynthesisName
                input = [ (Substance.food, 1); (p, 1) ]
                output = [ (Chiral a, 1); (p, 1) ]
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
                output = [ (FoodSubst.y |> Food, a.Length + b.Length) ]
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


    type Reaction = 
        | Synthesis of SynthesisReaction
        | CatalyticSynthesis of CatalyticSynthesisReaction
        | Ligation of LigationReaction
        | CatalyticLigation of CatalyticLigationReaction
        | SedimentationDirect of SedimentationDirectReaction
        | SedimentationAll of SedimentationAllReaction

        member r.name = 
            match r with 
            | Synthesis _ -> SynthesisName
            | CatalyticSynthesis _ -> CatalyticSynthesisName
            | Ligation _ -> LigationName
            | CatalyticLigation _ -> CatalyticLigationName
            | SedimentationDirect _ -> SedimentationDirectName
            | SedimentationAll _ -> SedimentationAllName

        member r.info = 
            match r with 
            | Synthesis r -> r.info
            | CatalyticSynthesis r -> r.info
            | Ligation r -> r.info
            | CatalyticLigation r -> r.info
            | SedimentationDirect r -> r.info
            | SedimentationAll r -> r.info

        member r.enantiomer = 
            match r with 
            | Synthesis r -> r.enantiomer |> Synthesis
            | CatalyticSynthesis r -> r.enantiomer |> CatalyticSynthesis
            | Ligation r -> r.enantiomer |> Ligation
            | CatalyticLigation r -> r.enantiomer |> CatalyticLigation
            | SedimentationDirect r -> r.enantiomer |> SedimentationDirect
            | SedimentationAll r -> SedimentationAll r

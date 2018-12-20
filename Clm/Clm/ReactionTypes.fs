namespace Clm

open Substances

module ReactionTypes = 

    type ReactionName = 
        | FoodCreationName
        | WasteRemovalName
        | WasteRecyclingName
        | SynthesisName
        | DestructionName
        | CatalyticSynthesisName
        | CatalyticDestructionName
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
            | WasteRecyclingName -> "recycling"
            | SynthesisName -> "synthesis"
            | DestructionName -> "destruction"
            | CatalyticSynthesisName -> "catalytic synthesis"
            | CatalyticDestructionName -> "catalytic destruction"
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
                DestructionName
                CatalyticSynthesisName
                CatalyticDestructionName
                LigationName
                CatalyticLigationName
                SedimentationDirectName
                SedimentationAllName
                RacemizationName
                CatalyticRacemizationName
            ]


    type ReactionInfo =
        {
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member this.getName n a = 
            let g (l : list<Substance * int>) = 
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            n + ": " + (g this.input) + a + (g this.output)


    type FoodCreationReaction = 
        | FoodCreationReaction

        member r.info = 
            {
                input = [ (Simple Abundant, 0) ]
                output = [ (Simple Food, 1) ]
            }

        member r.enantiomer = r


    type WasteRemovalReaction = 
        | WasteRemovalReaction

        member r.info = 
            {
                input = [ (Simple Waste, 1) ]
                output = []
            }

        member r.enantiomer = r


    type WasteRecyclingReaction = 
        | WasteRecyclingReaction

        member r.info = 
            {
                input = [ (Simple Waste, 1) ]
                output = [ (Simple Food, 1) ]
            }

        member r.enantiomer = r


    type SynthesisReaction = 
        | SynthesisReaction of ChiralAminoAcid

        member r.info = 
            let (SynthesisReaction a) = r
            {
                input = [ (Simple Food, 1) ]
                output = [ (Chiral a, 1) ]
            }

        member r.enantiomer = 
            let (SynthesisReaction a) = r
            a.enantiomer |> SynthesisReaction


    type DestructionReaction = 
        | DestructionReaction of ChiralAminoAcid

        member r.info = 
            let (DestructionReaction a) = r
            {
                input = [ (Chiral a, 1) ]
                output = [ (Simple Waste, 1) ]
            }

        member r.enantiomer = 
            let (DestructionReaction a) = r
            a.enantiomer |> DestructionReaction


    type SynthCatalyst = 
        | SynthCatalyst of Peptide

        member c.enantiomer = 
            let (SynthCatalyst a) = c
            a.enantiomer |> SynthCatalyst


    type DestrCatalyst = 
        | DestrCatalyst of Peptide

        member c.enantiomer = 
            let (DestrCatalyst a) = c
            a.enantiomer |> DestrCatalyst


    type CatalyticSynthesisReaction = 
        | CatalyticSynthesisReaction of (SynthesisReaction * SynthCatalyst)

        member r.info = 
            let (CatalyticSynthesisReaction (a, (SynthCatalyst c))) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1) ]
                output = a.info.output @ [ (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticSynthesisReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticSynthesisReaction


    type CatalyticDestructionReaction = 
        | CatalyticDestructionReaction of (DestructionReaction * DestrCatalyst)

        member r.info = 
            let (CatalyticDestructionReaction (a, (DestrCatalyst c))) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1) ]
                output = a.info.output @ [ (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticDestructionReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticDestructionReaction


    type LigationReaction = 
        | LigationReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member r.info = 
            let (LigationReaction (a, b)) = r

            {
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
                input = []
                output = []
            }

        member r.enantiomer = r


    type RacemizationReaction = 
        | RacemizationReaction of ChiralAminoAcid

        member r.info = 
            let (RacemizationReaction a) = r
            {
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
                input = [ (Chiral a, 1); (p, 1) ]
                output = [ (Chiral a.enantiomer, 1); (p, 1) ]
            }

        member r.enantiomer = 
            let (CatalyticRacemizationReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticRacemizationReaction


    let inline getName i = ((^T) : (member name : 'T) (i))
    let inline getInfo i = ((^T) : (member info : 'T) (i))


    type Reaction = 
        | FoodCreation of FoodCreationReaction
        | WasteRemoval of WasteRemovalReaction
        | WasteRecycling of WasteRecyclingReaction
        | Synthesis of SynthesisReaction
        | Destruction of DestructionReaction
        | CatalyticSynthesis of CatalyticSynthesisReaction
        | CatalyticDestruction of CatalyticDestructionReaction
        | Ligation of LigationReaction
        | CatalyticLigation of CatalyticLigationReaction
        | SedimentationDirect of SedimentationDirectReaction
        | SedimentationAll of SedimentationAllReaction
        | Racemization of RacemizationReaction
        | CatalyticRacemization of CatalyticRacemizationReaction

        member r.name = 
            match r with 
            | FoodCreation _ -> FoodCreationName
            | WasteRemoval _ -> WasteRemovalName
            | WasteRecycling _ -> WasteRecyclingName
            | Synthesis _ -> SynthesisName
            | Destruction _ -> DestructionName
            | CatalyticSynthesis _ -> CatalyticSynthesisName
            | CatalyticDestruction _ -> CatalyticDestructionName
            | Ligation _ -> LigationName
            | CatalyticLigation _ -> CatalyticLigationName
            | SedimentationDirect _ -> SedimentationDirectName
            | SedimentationAll _ -> SedimentationAllName
            | Racemization _ -> RacemizationName
            | CatalyticRacemization _ -> CatalyticRacemizationName

        member r.info = 
            match r with 
            | FoodCreation r -> r.info
            | WasteRemoval r -> r.info
            | WasteRecycling r -> r.info
            | Synthesis r -> r.info
            | Destruction r -> r.info
            | CatalyticSynthesis r -> r.info
            | CatalyticDestruction r -> r.info
            | Ligation r -> r.info
            | CatalyticLigation r -> r.info
            | SedimentationDirect r -> r.info
            | SedimentationAll r -> r.info
            | Racemization r -> r.info
            | CatalyticRacemization r -> r.info

        member r.enantiomer = 
            match r with 
            | FoodCreation r -> r.enantiomer |> FoodCreation
            | WasteRemoval r -> r.enantiomer |> WasteRemoval
            | WasteRecycling r -> r.enantiomer |> WasteRecycling
            | Synthesis r -> r.enantiomer |> Synthesis
            | Destruction r -> r.enantiomer |> Destruction
            | CatalyticSynthesis r -> r.enantiomer |> CatalyticSynthesis
            | CatalyticDestruction r -> r.enantiomer |> CatalyticDestruction
            | Ligation r -> r.enantiomer |> Ligation
            | CatalyticLigation r -> r.enantiomer |> CatalyticLigation
            | SedimentationDirect r -> r.enantiomer |> SedimentationDirect
            | SedimentationAll r -> SedimentationAll r // There are no enantiomers here.
            | Racemization r -> r.enantiomer |> Racemization
            | CatalyticRacemization r -> r.enantiomer |> CatalyticRacemization

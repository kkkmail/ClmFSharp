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
                WasteRecyclingName
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


    type ReactionNormalizedInfo =
        {
            inputNormalized : list<Substance>
            outputNormalized : list<Substance>
        }


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

        member this.normalized() =
            let normalize d =
                d
                |> List.map (fun (s, i) -> [ for _ in 0..(i-1) -> s ])
                |> List.concat
                |> List.sort

            {
                inputNormalized = this.input |> normalize
                outputNormalized = this.output |> normalize
            }


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

        member r.withEnantiomerCatalyst =
            let (CatalyticSynthesisReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticSynthesisReaction


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

        member r.withEnantiomerCatalyst =
            let (CatalyticDestructionReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticDestructionReaction


    /// A directed pair of amino acids forming peptide bond.
    /// leftAminoAcid is the amino acid on the left side of the bond and
    /// rightAminoAcid is on the right side of the bond.
    /// Any of these amino acids can be L or R.
    type PeptideBond =
        {
            leftAminoAcid : ChiralAminoAcid
            rightAminoAcid : ChiralAminoAcid
        }

        member r.enantiomer =
            { leftAminoAcid = r.leftAminoAcid.enantiomer; rightAminoAcid = r.rightAminoAcid.enantiomer }

        member r.bingingType =
            match r.leftAminoAcid, r.rightAminoAcid with
            | L _, L _ -> LL
            | L _, R _ -> LR
            | R _, L _ -> RL
            | R _, R _ -> RR


    type LigationReaction =
        | LigationReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member this.value = let (LigationReaction v) = this in v

        member r.info =
            let (LigationReaction (a, b)) = r

            {
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (Substance.fromList (a @ b), 1) ]
            }

        member r.peptideBond =
            let (LigationReaction (a, b)) = r
            { leftAminoAcid = a |> List.rev |> List.head; rightAminoAcid = b |> List.head }

        member r.bingingType = r.peptideBond.bingingType


        member r.enantiomer =
            let (LigationReaction (a, b)) = r
            (a |> List.map (fun e -> e.enantiomer), b |> List.map (fun e -> e.enantiomer)) |> LigationReaction


    type PeptideBondMap =
        | PeptideBondMap of Map<BindingSymmetry, Map<PeptideBond, Set<LigationReaction>>>

        /// Finds all ligation reactions with the same peptide bond INCLUDING input reaction.
        member m.findSame (x : LigationReaction) =
            let (PeptideBondMap v) = m

            v
            |> Map.tryFind x.bingingType
            |> Option.defaultValue Map.empty
            |> Map.tryFind x.peptideBond
            |> Option.defaultValue Set.empty
            |> Set.toList
            |> List.sortBy (fun e -> e.info)

        /// Finds all ligation reactions with the same peptide bond EXCEPT input reaction.
        /// Enantiomers are excluded as well.
        member m.findSameX (x : LigationReaction) =
            let xe = x.enantiomer

            m.findSame x
            |> List.filter (fun e -> e <> x && e <> xe)


        /// Finds all ligation reactions, which have the same peptide bond symmetry as given ligation reaction (e.g. aB + C -> aBC).
        member m.findSimilar (x : LigationReaction) =
            let (PeptideBondMap v) = m

            v
            |> Map.tryFind x.bingingType
            |> Option.defaultValue Map.empty
            |> Map.toList
            |> List.map snd
            |> List.map (fun e -> e |> Set.toList)
            |> List.concat
            |> List.distinct
            |> List.sortBy (fun e -> e.info)

        /// Finds all ligation reactions, which have the same peptide bond symmetry as given ligation reaction (e.g. aB + C -> aBC).
        /// But NOT the same bond. E.g. if incoming reaction is aB + C -> aBC, then peptide bond (of this reaction) is BC,
        /// bond symmetry is LL and this function returns all ligation reactions, which have bond symmetry type LL but not bond BC.
        /// E.g.: aB + D -> aBD, AC + E -> ACE, A + De -> ADe, etc..., but NOT B + C -> BC, B + Ce -> BCe, etc...
        /// Enantiomers are excluded as well.
        member m.findSimilarX (x : LigationReaction) =
            let xp = x.peptideBond
            let xpe = x.peptideBond.enantiomer

            m.findSimilar x
            |> List.filter(fun e -> e.peptideBond <> xp && e.peptideBond <> xpe)

        static member create (p : List<LigationReaction>) =
            p
            |> List.groupBy (fun e -> e.peptideBond)
            |> List.map (fun (a, b) -> a, b |> Set.ofList)
            |> List.groupBy (fun (e, _) -> e.bingingType)
            |> List.map (fun (a, b) -> a, b |> Map.ofList)
            |> Map.ofList
            |> PeptideBondMap


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

        member r.withEnantiomerCatalyst =
            let (CatalyticLigationReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticLigationReaction


    /// A resolving agent, which forms insoluble diasteriomeric salt with one of the enantiomer of some amino acid (or, in general, peptide as well).
    type SedDirAgent =
        | SedDirAgent of list<ChiralAminoAcid>

        member c.enantiomer =
            let (SedDirAgent a) = c
            a |> List.map (fun e -> e.enantiomer) |> SedDirAgent

        member this.value = let (SedDirAgent v) = this in v


    /// A peptide chain, which attaches to the resolving reagent by one of its ends (list head).
    type SedDirReagent =
        | SedDirReagent of list<ChiralAminoAcid>

        member c.enantiomer =
            let (SedDirReagent a) = c
            a |> List.map (fun e -> e.enantiomer) |> SedDirReagent

        member c.value = let (SedDirReagent v) = c in v

        member c.startsWith a =
            match c.value with
            | h :: _ -> h = a
            | [] -> false


    type SedimentationDirectReaction =
        | SedimentationDirectReaction of (SedDirReagent * SedDirAgent)

        member r.info =
            let (SedimentationDirectReaction (a, b)) = r

            {
                input = [ (Substance.fromList a.value, 1); (Substance.fromList b.value, 1) ]
                output = [ (AchiralSubst.Waste |> Simple, a.value.Length + b.value.Length) ]
            }

        member r.enantiomer =
            let (SedimentationDirectReaction (a, b)) = r
            (a.enantiomer, b.enantiomer) |> SedimentationDirectReaction


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

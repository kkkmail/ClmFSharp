namespace Clm

open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes
open Clm.Substances

module ReactionRates =
    open System.Collections.Generic

    /// Specifies how to generate rates.
    /// RandomChoice first randomly determine the reactions with non-zero rates and then gets these rates (without using threshold).
    type RateGenerationType =
        | RandomChoice


    /// Specifies how to apply similarity.
    /// DistrBased uses distribution threshold to determine if an amino acid should / should not be included.
    /// This results in some spread in the number of amino acids.
    /// FixedVal - fixes the number of amino acids, but the choice of them is still random.
    type CatRatesSimGenType =
        | DistrBased
        | FixedVal


    /// Specifies how to generate catalytic rates.
    /// ByIndividualCatalyst - enforces thermodynamic constraint on each catalyst.
    /// ByEnantiomerPairs - enforces thermodynamic constraint on a pair of enantiomer catalysts.
    type CatalyticRateGenerationType =
        | ByIndividualCatalyst of CatRatesSimGenType
        | ByEnantiomerPairs of CatRatesSimGenType

        member this.catRatesSimGenType = match this with | ByIndividualCatalyst c | ByEnantiomerPairs c -> c


    type RateData =
        {
            forwardRate : ReactionRate option
            backwardRate : ReactionRate option
        }

        override r.ToString() = sprintf "{ f: %A; b: %A }" r.forwardRate r.backwardRate


    let bind f xOpt =
        match xOpt with
        | Some x -> f x
        | _ -> { forwardRate = None; backwardRate = None }


    type ReactionRateData<'R> =
        {
            reaction : 'R
            rateData : RateData
        }


    type RateGeneratorInfo<'A, 'B> =
        {
            a : array<'A>
            b : array<'B>
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RelatedReactions<'R> =
        {
            primary : RateData
            similar : list<ReactionRateData<'R>>
        }


    let getRatesWithSimilar (fo, rf) (bo, rb) s =
        let g so ro =
            match so, ro with
            | Some s, Some r -> s * r |> ReactionRate |> Some
            | _ -> None

        {
            primary = { forwardRate = g fo rf; backwardRate = g bo rb }
            similar = s
        }


    let getRates (fo, rf) (bo, rb) = getRatesWithSimilar (fo, rf) (bo, rb) []
    let getForwardRates (fo, rf) = getRates (fo, rf) (None, None)


    type CatRatesEeParam =
        {
            rateMultiplierDistr : RateMultiplierDistribution
            eeForwardDistribution : EeDistribution option
            eeBackwardDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                rateMultiplierDistr = NoneRateMult
                eeForwardDistribution = None
                eeBackwardDistribution = None
            }


    type CatRatesInfo<'R, 'C, 'RC> =
        {
            reaction : 'R
            catalyst : 'C
            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            eeParams : CatRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

    /// Thermodynamic considerations require that the equilibrium does not change in the presence of catalyst.
    /// That requires a racemic mixture of both chiral catalysts (because only a racemic mixture is in the equilibrium state).
    /// Therefore, if sf and sb are forward and backward rates of not catalyzed reaction, then
    /// total forward and backward multipliers due to racemic mixture of catalysts must be equal:
    ///
    /// kf + kfe = kb + kbe, where
    ///     kf -  is forward  multiplier for a catalyst C
    ///     kfe - is forward  multiplier for a catalyst E(C) - enantiomer of C
    ///     kb -  is backward multiplier for a catalyst C
    ///     kbe - is backward multiplier for a catalyst E(C)
    ///
    /// Setting eeParams.eeBackwardDistribution = None imposes more stringent constraint that
    /// forward and backward rate multipliers must be the same for each catalyst independently from
    /// its enantiomer. This seems to be more correct.
    let calculateCatRates<'R, 'C, 'RC> (i : CatRatesInfo<'R, 'C, 'RC>) =
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator

        let rf, rb, rfe, rbe =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.rateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.eeForwardDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates i.reaction
                let fEe = df.nextDouble i.rnd

                let bEe =
                    match i.eeParams.eeBackwardDistribution with
                    | Some d -> d.nextDouble i.rnd
                    | None -> fEe

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)
                let kb = k0 * (1.0 + bEe)
                let kbe = k0 * (1.0 - bEe)

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (kb * sb |> ReactionRate |> Some, kbe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe)
            | _ -> (None, None, None, None)

        {
            primary = { forwardRate = rf; backwardRate = rb }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = rbe } } ]
        }


    type CatRatesSimGeneration =
        | DistributionBased of Distribution
        | FixedValue of Distribution


    type CatRatesSimilarityParam =
        {
            catRatesSimGeneration : CatRatesSimGeneration
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
            getBackwardEeDistr : EeDistributionGetter
        }


    type FoodCreationParam =
        {
            foodCreationRate : double
        }


    type WasteRemovalParam =
        {
            wasteRemovalRate : double
        }


    type WasteRecyclingParam =
        {
            wasteRecyclingRate : double
        }


    type SynthesisRandomParam =
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SynthesisParam =
        | SynthRndParam of SynthesisRandomParam


    type CatalyticSynthesisRandomParam =
        {
            synthesisParam : SynthesisParam
            catSynthRndEeParams : CatRatesEeParam
        }


    type CatalyticSynthesisSimilarParam =
        {
            catSynthParam : CatalyticSynthesisRandomParam
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisParam =
        | CatSynthRndParam of CatalyticSynthesisRandomParam
        | CatSynthSimParam of CatalyticSynthesisSimilarParam


    type DestructionRandomParam =
        {
            destructionDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type DestructionParam =
        | DestrRndParam of DestructionRandomParam


    type CatalyticDestructionRandomParam =
        {
            catDestrRndEeParams : CatRatesEeParam
            destructionParam : DestructionParam
        }


    type CatalyticDestructionSimilarParam =
        {
            catDestrSimParam : CatRatesSimilarityParam
            catDestrParam : CatalyticDestructionRandomParam
        }


    type CatalyticDestructionParam =
        | CatDestrRndParam of CatalyticDestructionRandomParam
        | CatDestrSimParam of CatalyticDestructionSimilarParam


    type SedDirRatesEeParam =
        {
            sedDirRateMultiplierDistr : RateMultiplierDistribution
            eeForwardDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                sedDirRateMultiplierDistr = NoneRateMult
                eeForwardDistribution = None
            }


    type SedimentationDirectRandomParam =
        {
            sedDirRatesEeParam : SedDirRatesEeParam
            sedDirDistribution : Distribution
            forwardScale : double option
        }


    type SedDirRatesInfo =
        {
            sedFormingSubst : SedDirReagent
            sedDirAgent : SedDirAgent
            getBaseRates : SedimentationDirectReaction -> RateData
            eeParams : SedDirRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }


    type SedDirSimilarityParam =
        {
            sedDirSimBaseDistribution : Distribution
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
        }


    type SedDirRatesSimInfo =
        {
            sedDirRatesInfo : SedDirRatesInfo

            aminoAcids : list<AminoAcid>
            reagents : Map<AminoAcid, list<SedDirReagent>>
            simParams : SedDirSimilarityParam
            rateDictionary : Dictionary<SedimentationDirectReaction, RateData>
        }


    type SedimentationDirectSimilarParam =
        {
            sedDirParam : SedimentationDirectRandomParam
            sedDirSimParam : SedDirSimilarityParam
        }


    let calculateSedDirRates (i : SedDirRatesInfo) =
        let reaction = (i.sedFormingSubst, i.sedDirAgent) |> SedimentationDirectReaction
        let re = (i.sedFormingSubst, i.sedDirAgent.enantiomer) |> SedimentationDirectReaction

        let rf, rfe =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.sedDirRateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.eeForwardDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates reaction
                let fEe = df.nextDouble i.rnd

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rfe)
            | _ -> (None, None)

        {
            primary = { forwardRate = rf; backwardRate = None }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = None } } ]
        }


    type SedimentationDirectParam =
        | SedDirRndParam of SedimentationDirectRandomParam
        | SedDirSimParam of SedimentationDirectSimilarParam


    type SedimentationAllRandomParam =
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationAllParam =
        | SedAllRndParam of SedimentationAllRandomParam


    type LigationRandomParam =
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type LigationParam =
        | LigRndParam of LigationRandomParam


    type CatalyticLigationRandomParam =
        {
            ligationParam : LigationParam
            catLigRndEeParams : CatRatesEeParam
        }


    type CatalyticLigationSimilarParam =
        {
            catLigSimParam : CatRatesSimilarityParam
            catLigParam : CatalyticLigationRandomParam
        }


    type CatalyticLigationParam =
        | CatLigRndParam of CatalyticLigationRandomParam
        | CatLigSimParam of CatalyticLigationSimilarParam


    type RacemizationRandomParam =
        {
            racemizationDistribution : Distribution
            forwardScale : double option
        }


    type RacemizationParam =
        | RacemRndParam of RacemizationRandomParam


    type CatalyticRacemizationRandomParam =
        {
            racemizationParam : RacemizationParam
            catRacemRndEeParams : CatRatesEeParam
        }


    type CatalyticRacemizationSimilarParam =
        {
            catRacemParam : CatalyticRacemizationRandomParam
            catRacemSimParam : CatRatesSimilarityParam
        }


    type CatalyticRacemizationParam =
        | CatRacemRndParam of CatalyticRacemizationRandomParam
        | CatRacemSimParam of CatalyticRacemizationSimilarParam


    type AllRatesData =
        | FoodCreationRates of list<ReactionRateData<FoodCreationReaction>>
        | WasteRemovalRates of list<ReactionRateData<WasteRemovalReaction>>
        | WasteRecyclingRates of list<ReactionRateData<WasteRecyclingReaction>>
        | SynthesisRates of list<ReactionRateData<SynthesisReaction>>
        | DestructionRates of list<ReactionRateData<DestructionReaction>>
        | CatalyticSynthesisRates of list<ReactionRateData<CatalyticSynthesisReaction>>
        | CatalyticDestructionRates of list<ReactionRateData<CatalyticDestructionReaction>>
        | LigationRates of list<ReactionRateData<LigationReaction>>
        | CatalyticLigationRates of list<ReactionRateData<CatalyticLigationReaction >>
        | SedimentationDirectRates of list<ReactionRateData<SedimentationDirectReaction>>
        | SedimentationAllRates of list<ReactionRateData<SedimentationAllReaction>>
        | RacemizationRates of list<ReactionRateData<RacemizationReaction>>
        | CatalyticRacemizationRates of list<ReactionRateData<CatalyticRacemizationReaction>>

        member ard.toReactionRates() =
            match ard with
            | FoodCreationRates r -> r |> List.map (fun e -> e.reaction |> FoodCreation, e.rateData)
            | WasteRemovalRates r -> r |> List.map (fun e -> e.reaction |> WasteRemoval, e.rateData)
            | WasteRecyclingRates r -> r |> List.map (fun e -> e.reaction |> WasteRecycling, e.rateData)
            | SynthesisRates r -> r |> List.map (fun e -> e.reaction |> Synthesis, e.rateData)
            | DestructionRates r -> r |> List.map (fun e -> e.reaction |> Destruction, e.rateData)
            | CatalyticSynthesisRates r -> r |> List.map (fun e -> e.reaction |> CatalyticSynthesis, e.rateData)
            | CatalyticDestructionRates r -> r |> List.map (fun e -> e.reaction |> CatalyticDestruction, e.rateData)
            | LigationRates r -> r |> List.map (fun e -> e.reaction |> Ligation, e.rateData)
            | CatalyticLigationRates r -> r |> List.map (fun e -> e.reaction |> CatalyticLigation, e.rateData)
            | SedimentationDirectRates r -> r |> List.map (fun e -> e.reaction |> SedimentationDirect, e.rateData)
            | SedimentationAllRates r -> r |> List.map (fun e -> e.reaction |> SedimentationAll, e.rateData)
            | RacemizationRates r -> r |> List.map (fun e -> e.reaction |> Racemization, e.rateData)
            | CatalyticRacemizationRates r -> r |> List.map (fun e -> e.reaction |> CatalyticRacemization, e.rateData)


    type ReactionRateModelParam =
        | FoodCreationRateParam of FoodCreationParam
        | WasteRemovalRateParam of WasteRemovalParam
        | WasteRecyclingRateParam of WasteRecyclingParam
        | SynthesisRateParam of SynthesisParam
        | DestructionRateParam of DestructionParam
        | CatalyticSynthesisRateParam of CatalyticSynthesisParam
        | CatalyticDestructionRateParam of CatalyticDestructionParam
        | LigationRateParam of LigationParam
        | CatalyticLigationRateParam of CatalyticLigationParam
        | SedimentationDirectRateParam of SedimentationDirectParam
        | SedimentationAllRateParam of SedimentationAllParam
        | RacemizationRateParam of RacemizationParam
        | CatalyticRacemizationRateParam of CatalyticRacemizationParam


        /// TODO kk:20190317 - The dependencies MUST be incorporated at lower level so that to make it compiler's job to check them.
        /// Otherwise, if dependency is forgotten, it becomes hard to trace that bug.
        /// Essentially it must be made IMPOSSIBLE to write a code with the dependency and don't "declare" it upfront. Currently it is possible.
        member rm.dependsOn =
            match rm with
            | FoodCreationRateParam _ -> []
            | WasteRemovalRateParam _ -> []
            | WasteRecyclingRateParam _ -> []
            | SynthesisRateParam _ -> []
            | DestructionRateParam _ -> []
            | CatalyticSynthesisRateParam v ->
                match v with
                | CatSynthRndParam m -> [ m.synthesisParam |> SynthesisRateParam ]
                | CatSynthSimParam m -> [ m.catSynthParam |> CatSynthRndParam |> CatalyticSynthesisRateParam ]
            | CatalyticDestructionRateParam v ->
                match v with
                | CatDestrRndParam m -> [ m.destructionParam |> DestructionRateParam ]
                | CatDestrSimParam m -> [ m.catDestrParam |> CatDestrRndParam |> CatalyticDestructionRateParam ]
            | LigationRateParam _ -> []
            | CatalyticLigationRateParam v ->
                match v with
                | CatLigRndParam m -> [ m.ligationParam |> LigationRateParam ]
                | CatLigSimParam m -> [ m.catLigParam |> CatLigRndParam |> CatalyticLigationRateParam ]
            | SedimentationDirectRateParam v ->
                match v with
                | SedDirRndParam _ -> []
                | SedDirSimParam m -> [ m.sedDirParam |> SedDirRndParam |> SedimentationDirectRateParam ]
            | SedimentationAllRateParam _ -> []
            | RacemizationRateParam _ -> []
            | CatalyticRacemizationRateParam v ->
                match v with
                | CatRacemRndParam m -> [ m.racemizationParam |> RacemizationRateParam ]
                | CatRacemSimParam m -> [ m.catRacemParam |> CatRacemRndParam |> CatalyticRacemizationRateParam ]


    type ReactionRateModelParamUsage =
        | PrimaryParam
        | DependsOnParam


    type ReactionRateModelParamWithUsage =
        {
            modelParam : ReactionRateModelParam
            usage : ReactionRateModelParamUsage
        }


    let tryPickParam picker (mp : list<ReactionRateModelParamWithUsage>) =
        let rec inner a b =
            match a with
            | [] -> None, b |> List.rev
            | h :: t ->
                match picker h with
                | Some x -> Some x, (b |> List.rev) @ t
                | None -> inner t (h :: b)

        inner mp []


    let rec allDep (rm : ReactionRateModelParam) (acc : list<ReactionRateModelParam>) =
        match rm.dependsOn with
        | [] -> acc
        | l -> l |> List.fold (fun a r -> allDep r (r :: a)) acc


    type ReactionRateProviderParams =
        {
            rateParams: list<ReactionRateModelParam>
            successNumberType : SuccessNumberType // kk:20191015 - currently there is one success number type for all relevant reactions.
        }

        member p.tryFindFoodCreationParam() = p.rateParams |> List.tryPick (fun e -> match e with | FoodCreationRateParam m -> Some m | _ -> None)
        member p.tryFindWasteRemovalParam() = p.rateParams |> List.tryPick (fun e -> match e with | WasteRemovalRateParam m -> Some m | _ -> None)
        member p.tryFindWasteRecyclingParam() = p.rateParams |> List.tryPick (fun e -> match e with | WasteRecyclingRateParam m -> Some m | _ -> None)
        member p.tryFindSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | SynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindDestructionParam() = p.rateParams |> List.tryPick (fun e -> match e with | DestructionRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticSynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticDestructionParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticDestructionRateParam m -> Some m | _ -> None)
        member p.tryFindLigationParam() = p.rateParams |> List.tryPick (fun e -> match e with | LigationRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticLigationParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticLigationRateParam m -> Some m | _ -> None)
        member p.tryFindSedimentationDirectParam() = p.rateParams |> List.tryPick (fun e -> match e with | SedimentationDirectRateParam m -> Some m | _ -> None)
        member p.tryFindSedimentationAllParam() = p.rateParams |> List.tryPick (fun e -> match e with | SedimentationAllRateParam m -> Some m | _ -> None)
        member p.tryFindRacemizationParam() = p.rateParams |> List.tryPick (fun e -> match e with | RacemizationRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticRacemizationParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticRacemizationRateParam m -> Some m | _ -> None)

        member p.allParams() =
            let prim = p.rateParams |> Set.ofList
            let dep = Set.difference (p.rateParams |> List.map (fun e -> allDep e []) |> List.concat |> Set.ofList) prim

            (prim |> Set.map (fun e -> { modelParam = e; usage = PrimaryParam }))
            |> Set.union (dep |> Set.map (fun e -> { modelParam = e; usage = DependsOnParam }))
            |> Set.toList
            |> List.sort

namespace Clm

open System
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionTypes
open ClmSys.GeneralData

module ReactionRates =

    /// Specifies how to generate rates.
    /// BruteForce checks each reaction and use threshold to determine if the rates should be assigned.
    /// RandomChoice first randomly determine the reactions with non-zero rates and then gets these rates (without using threshold).
    type RateGenerationType =
        | BruteForce
        | RandomChoice


    type RateData =
        {
            forwardRate : ReactionRate option
            backwardRate : ReactionRate option
        }


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
    /// That requires a racemic mixture of both chiral catalysts (because only a racemic mixture is in the equilibrium state) =>
    /// If sf and sb are forward and backward rates of not catalyzed reaction, then
    /// total forward and backward multiplers due to racemic mixture of catalysts must be the equal:
    /// kf + kfe = kb + kbe, where
    ///     kf -  is forward  multipler for a catalyst C
    ///     kfe - is forward  multipler for a catalyst E(C) - enantiomer of C
    ///     kb -  is backward multipler for a catalyst C
    ///     kbe - is backward multipler for a catalyst E(C)
    let calculateCatRates<'R, 'C, 'RC> (i : CatRatesInfo<'R, 'C, 'RC>) =
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator

        let rf, rb, rfe, rbe =
            let k =
                match i.rateGenerationType with
                | BruteForce -> i.eeParams.rateMultiplierDistr.nextDoubleOpt i.rnd
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


    type CatRatesSimilarityParam =
        {
            simBaseDistribution : Distribution
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
            getBackwardEeDistr : EeDistributionGetter
        }


    //[<Literal>]
    //let FoodCreationParamName = "FoodCreationParam"

    type FoodCreationParam =
        {
            foodCreationRate : double
        }

        //member this.name = FoodCreationParamName

        //static member allNames =
        //    [
        //        FoodCreationParamName
        //    ]


    //[<Literal>]
    //let WasteRemovalParamName = "WasteRemovalParam"

    type WasteRemovalParam =
        {
            wasteRemovalRate : double
        }

        //member this.name = WasteRemovalParamName

        //static member allNames =
        //    [
        //        WasteRemovalParamName
        //    ]


    //[<Literal>]
    //let WasteRecyclingParamName = "WasteRecyclingParam"

    type WasteRecyclingParam =
        {
            wasteRecyclingRate : double
        }

        //member this.name = WasteRecyclingParamName

        //static member allNames =
        //    [
        //        WasteRecyclingParamName
        //    ]


    //[<Literal>]
    //let SynthesisRandomParamName = "SynthesisRandomParam"

    type SynthesisRandomParam =
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        //member this.name = SynthesisRandomParamName

        //static member allNames =
        //    [
        //        SynthesisRandomParamName
        //    ]


    //[<Literal>]
    //let SynthesisParamName = "SynthesisParam"


    //[<Literal>]
    //let SynthRndParamName = "SynthRndParam"


    type SynthesisParam =
        | SynthRndParam of SynthesisRandomParam

        //member this.name =
        //    match this with
        //    | SynthRndParam _ -> SynthRndParamName

        //static member allNames =
        //    [
        //        SynthRndParamName
        //    ]


    //[<Literal>]
    //let CatalyticSynthesisRandomParamName = "CatalyticSynthesisRandomParam"

    type CatalyticSynthesisRandomParam =
        {
            synthesisParam : SynthesisParam
            catSynthRndEeParams : CatRatesEeParam
        }

        //member this.name = CatalyticSynthesisRandomParamName

        //static member allNames =
        //    [
        //        CatalyticSynthesisRandomParamName
        //    ]


    //[<Literal>]
    //let CatalyticSynthesisParamName = "CatalyticSynthesisParam"


    //[<Literal>]
    //let CatSynthRndParamName = "CatSynthRndParam"


    //[<Literal>]
    //let CatSynthSimParamName = "CatSynthSimParam"

    type CatalyticSynthesisSimilarParam =
        {
            catSynthParam : CatalyticSynthesisRandomParam
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisParam =
        | CatSynthRndParam of CatalyticSynthesisRandomParam
        | CatSynthSimParam of CatalyticSynthesisSimilarParam

        //member this.name =
        //    match this with
        //    | CatSynthRndParam _ -> CatSynthRndParamName
        //    | CatSynthSimParam _ -> CatSynthSimParamName

        //static member allNames =
        //    [
        //        CatSynthRndParamName
        //        CatSynthSimParamName
        //    ]


    //[<Literal>]
    //let DestructionRandomParamName = "DestructionRandomParam"

    type DestructionRandomParam =
        {
            destructionDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        //member this.name = DestructionRandomParamName

        //static member allNames =
        //    [
        //        DestructionRandomParamName
        //    ]


    //[<Literal>]
    //let DestructionParamName = "DestructionParam"


    //[<Literal>]
    //let DestrRndParamName = "DestrRndParam"


    type DestructionParam =
        | DestrRndParam of DestructionRandomParam

        //member this.name =
        //    match this with
        //    | DestrRndParam _ -> DestrRndParamName

        //static member allNames =
        //    [
        //        DestrRndParamName
        //    ]


    //[<Literal>]
    //let CatalyticDestructionRandomParamName = "CatalyticDestructionRandomParam"

    type CatalyticDestructionRandomParam =
        {
            catDestrRndEeParams : CatRatesEeParam
            destructionParam : DestructionParam
        }

        //member this.name = CatalyticDestructionRandomParamName


    //[<Literal>]
    //let CatalyticDestructionParamName = "CatalyticDestructionParam"


    //[<Literal>]
    //let CatDestrRndParamName = "CatDestrRndParam"


    //[<Literal>]
    //let CatDestrSimParamName = "CatDestrSimParam"


    type CatalyticDestructionSimilarParam =
        {
            catDestrSimParam : CatRatesSimilarityParam
            catDestrParam : CatalyticDestructionRandomParam
        }


    type CatalyticDestructionParam =
        | CatDestrRndParam of CatalyticDestructionRandomParam
        | CatDestrSimParam of CatalyticDestructionSimilarParam

        //member this.name =
        //    match this with
        //    | CatDestrRndParam _ -> CatDestrRndParamName
        //    | CatDestrSimParam _ -> CatDestrSimParamName

        //static member allNames =
        //    [
        //        CatDestrRndParamName
        //        CatDestrSimParamName
        //    ]


    //[<Literal>]
    //let SedimentationDirectRandomParamName = "SedimentationDirectRandomParam"

    type SedimentationDirectRandomParam =
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }

        //member this.name = SedimentationDirectRandomParamName

        //static member allNames =
        //    [
        //        SedimentationDirectRandomParamName
        //    ]


    //[<Literal>]
    //let SedimentationDirectParamName = "SedimentationDirectParam"


    //[<Literal>]
    //let SedDirRndParamName = "SedDirRndParam"


    type SedimentationDirectParam =
        | SedDirRndParam of SedimentationDirectRandomParam

        //member this.name =
        //    match this with
        //    | SedDirRndParam _ -> SedDirRndParamName

        //static member allNames =
        //    [
        //        SedDirRndParamName
        //    ]


    //[<Literal>]
    //let SedimentationAllRandomParamName = "SedimentationAllRandomParam"

    type SedimentationAllRandomParam =
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }

        //member this.name = SedimentationAllRandomParamName

        //static member allNames =
        //    [
        //        SedimentationAllRandomParamName
        //    ]


    //[<Literal>]
    //let SedimentationAllParamName = "SedimentationAllParam"


    //[<Literal>]
    //let SedAllRndParamName = "SedAllRndParam"


    type SedimentationAllParam =
        | SedAllRndParam of SedimentationAllRandomParam

        //member this.name =
        //    match this with
        //    | SedAllRndParam _ -> SedAllRndParamName

        //static member allNames =
        //    [
        //        SedAllRndParamName
        //    ]


    //[<Literal>]
    //let LigationRandomParamName = "LigationRandomParam"

    type LigationRandomParam =
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        //member this.name = LigationRandomParamName

        //static member allNames =
        //    [
        //        LigationRandomParamName
        //    ]


    //[<Literal>]
    //let LigationParamName = "LigationParam"


    //[<Literal>]
    //let LigRndParamName = "LigRndParam"


    type LigationParam =
        | LigRndParam of LigationRandomParam

        //member this.name =
        //    match this with
        //    | LigRndParam _ -> LigRndParamName

        //static member allNames =
        //    [
        //        LigRndParamName
        //    ]


    //[<Literal>]
    //let CatalyticLigationRandomParamName = "CatalyticLigationRandomParam"

    type CatalyticLigationRandomParam =
        {
            ligationParam : LigationParam
            catLigRndEeParams : CatRatesEeParam
        }

        //member this.name = CatalyticLigationRandomParamName

        //static member allNames =
        //    [
        //        CatalyticLigationRandomParamName
        //    ]


    //[<Literal>]
    //let CatalyticLigationParamName = "CatalyticLigationParam"


    //[<Literal>]
    //let CatLigRndParamName = "CatLigRndParam"


    type CatalyticLigationParam =
        | CatLigRndParam of CatalyticLigationRandomParam

        //member this.name =
        //    match this with
        //    | CatLigRndParam _ -> CatLigRndParamName

        //static member allNames =
        //    [
        //        CatLigRndParamName
        //    ]


    //[<Literal>]
    //let RacemizationRandomParamName = "RacemizationRandomParam"

    type RacemizationRandomParam =
        {
            racemizationDistribution : Distribution
            forwardScale : double option
        }

        //member this.name = RacemizationRandomParamName

        //static member allNames =
        //    [
        //        RacemizationRandomParamName
        //    ]


    //[<Literal>]
    //let RacemizationParamName = "RacemizationParam"


    //[<Literal>]
    //let RacemRndParamName = "RacemRndParam"


    type RacemizationParam =
        | RacemRndParam of RacemizationRandomParam

        //member this.name =
        //    match this with
        //    | RacemRndParam _ -> RacemRndParamName

        //static member allNames =
        //    [
        //        RacemRndParamName
        //    ]


    //[<Literal>]
    //let CatalyticRacemizationRandomParamName = "CatalyticRacemizationRandomParam"

    type CatalyticRacemizationRandomParam =
        {
            racemizationParam : RacemizationParam
            catRacemRndEeParams : CatRatesEeParam
        }

        //member this.name = CatalyticRacemizationRandomParamName

        //static member allNames =
        //    [
        //        CatalyticRacemizationRandomParamName
        //    ]


    //[<Literal>]
    //let CatalyticRacemizationParamName = "CatalyticRacemizationParam"

    //[<Literal>]
    //let CatRacemRndParamName = "CatRacemRndParam"

    //[<Literal>]
    //let CatRacemSimParamName = "CatRacemSimParam"


    type CatalyticRacemizationSimilarParam =
        {
            catRacemParam : CatalyticRacemizationRandomParam
            catRacemSimParam : CatRatesSimilarityParam
        }


    type CatalyticRacemizationParam =
        | CatRacemRndParam of CatalyticRacemizationRandomParam
        | CatRacemSimParam of CatalyticRacemizationSimilarParam

        //member this.name =
        //    match this with
        //    | CatRacemRndParam _ -> CatRacemRndParamName
        //    | CatRacemSimParam _ -> CatRacemSimParamName

        //static member allNames =
        //    [
        //        CatRacemRndParamName
        //        CatRacemSimParamName
        //    ]


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


    //[<Literal>]
    //let ReactionRateModelParamName = "ReactionRateModelParam"

    //[<Literal>]
    //let FoodCreationRateParamName = "FoodCreationRateParam"

    //[<Literal>]
    //let WasteRemovalRateParamName = "WasteRemovalRateParam"

    //[<Literal>]
    //let WasteRecyclingRateParamName = "WasteRecyclingRateParam"

    //[<Literal>]
    //let SynthesisRateParamName = "SynthesisRateParam"

    //[<Literal>]
    //let DestructionRateParamName = "DestructionRateParam"

    //[<Literal>]
    //let CatalyticSynthesisRateParamName = "CatalyticSynthesisRateParam"

    //[<Literal>]
    //let CatalyticDestructionRateParamName = "CatalyticDestructionRateParam"

    //[<Literal>]
    //let LigationRateParamName = "LigationRateParam"

    //[<Literal>]
    //let CatalyticLigationRateParamName = "CatalyticLigationRateParam"

    //[<Literal>]
    //let SedimentationDirectRateParamName = "SedimentationDirectRateParam"

    //[<Literal>]
    //let SedimentationAllRateParamName = "SedimentationAllRateParam"

    //[<Literal>]
    //let RacemizationRateParamName = "RacemizationRateParam"

    //[<Literal>]
    //let CatalyticRacemizationRateParamName = "CatalyticRacemizationRateParam"


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

        //member this.name =
        //    match this with
        //    | FoodCreationRateParam _ -> FoodCreationRateParamName
        //    | WasteRemovalRateParam _ -> WasteRemovalRateParamName
        //    | WasteRecyclingRateParam _ -> WasteRecyclingRateParamName
        //    | SynthesisRateParam _ -> SynthesisRateParamName
        //    | DestructionRateParam _ -> DestructionRateParamName
        //    | CatalyticSynthesisRateParam _ -> CatalyticSynthesisRateParamName
        //    | CatalyticDestructionRateParam _ -> CatalyticDestructionRateParamName
        //    | LigationRateParam _ -> LigationRateParamName
        //    | CatalyticLigationRateParam _ -> CatalyticLigationRateParamName
        //    | SedimentationDirectRateParam _ -> SedimentationDirectRateParamName
        //    | SedimentationAllRateParam _ -> SedimentationAllRateParamName
        //    | RacemizationRateParam _ -> RacemizationRateParamName
        //    | CatalyticRacemizationRateParam _ -> CatalyticRacemizationRateParamName

        //member this.paramName =
        //    match this with
        //    | FoodCreationRateParam p -> p.name
        //    | WasteRemovalRateParam p -> p.name
        //    | WasteRecyclingRateParam p -> p.name
        //    | SynthesisRateParam p -> p.name
        //    | DestructionRateParam p -> p.name
        //    | CatalyticSynthesisRateParam p -> p.name
        //    | CatalyticDestructionRateParam p -> p.name
        //    | LigationRateParam p -> p.name
        //    | CatalyticLigationRateParam p -> p.name
        //    | SedimentationDirectRateParam p -> p.name
        //    | SedimentationAllRateParam p -> p.name
        //    | RacemizationRateParam p -> p.name
        //    | CatalyticRacemizationRateParam p -> p.name

        //member this.variableName = this.name |> toVariableName
        //member this.variableParamName = this.paramName |> toVariableName

        //static member allNames =
        //    [
        //        FoodCreationRateParamName
        //        WasteRemovalRateParamName
        //        WasteRecyclingRateParamName
        //        SynthesisRateParamName
        //        DestructionRateParamName
        //        CatalyticSynthesisRateParamName
        //        CatalyticDestructionRateParamName
        //        LigationRateParamName
        //        CatalyticLigationRateParamName
        //        SedimentationDirectRateParamName
        //        SedimentationAllRateParamName
        //        RacemizationRateParamName
        //        CatalyticRacemizationRateParamName
        //    ]

        //static member allVariableNames =
        //    ReactionRateModelParam.allNames |> List.map (fun e -> toVariableName e)

        //static member allParamNames =
        //    [
        //        FoodCreationParam.allNames
        //        WasteRemovalParam.allNames
        //        WasteRecyclingParam.allNames
        //        SynthesisParam.allNames
        //        DestructionParam.allNames
        //        CatalyticSynthesisParam.allNames
        //        CatalyticDestructionParam.allNames
        //        LigationParam.allNames
        //        CatalyticLigationParam.allNames
        //        SedimentationDirectParam.allNames
        //        SedimentationAllParam.allNames
        //        RacemizationParam.allNames
        //        CatalyticRacemizationParam.allNames
        //    ]
        //    |> List.concat

        //static member allVariableParamNames =
        //    ReactionRateModelParam.allParamNames |> List.map (fun e -> toVariableName e)

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
            | SedimentationDirectRateParam _ -> []
            | SedimentationAllRateParam _ -> []
            | RacemizationRateParam _ -> []
            | CatalyticRacemizationRateParam v ->
                match v with
                | CatRacemRndParam m -> [ m.racemizationParam |> RacemizationRateParam ]
                | CatRacemSimParam m -> [ m.catRacemParam |> CatRacemRndParam |> CatalyticRacemizationRateParam ]


    //[<Literal>]
    //let ReactionRateModelParamUsageName = "ReactionRateModelParamUsage"

    //[<Literal>]
    //let PrimaryParamName = "PrimaryParam"

    //[<Literal>]
    //let DependsOnParamName = "DependsOnParam"


    type ReactionRateModelParamUsage =
        | PrimaryParam
        | DependsOnParam

        //member this.name =
        //    match this with
        //    | PrimaryParam -> PrimaryParamName
        //    | DependsOnParam -> DependsOnParamName


    type ReactionRateModelParamWithUsage =
        {
            modelParam : ReactionRateModelParam
            usage : ReactionRateModelParamUsage
        }


    //type ReactionRateParams =
    //    {
    //        modelParams : list<ReactionRateModelParamWithUsage>
    //        aminoAcids : list<AminoAcid>
    //    }


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

        member p.allParams =
            let prim = p.rateParams |> Set.ofList
            let dep = Set.difference (p.rateParams |> List.map (fun e -> allDep e []) |> List.concat |> Set.ofList) prim

            (prim |> Set.map (fun e -> { modelParam = e; usage = PrimaryParam }))
            |> Set.union (dep |> Set.map (fun e -> { modelParam = e; usage = DependsOnParam }))
            |> Set.toList
            |> List.sort

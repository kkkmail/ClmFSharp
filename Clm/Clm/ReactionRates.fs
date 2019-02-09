namespace Clm

open System
open System.Collections.Generic
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


    let dictionaryToList (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) =
        d
        |> List.ofSeq
        |> List.map (fun e -> e.Key, e.Value)
        |> List.sortBy (fun (k, _) -> k)


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


    let updatePrimaryReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (primary : RateData)
        (r : 'R) =

        let enantiomer = getEnantiomer r
        if d.ContainsKey r |> not then d.Add(r, primary)
        if d.ContainsKey enantiomer |> not then d.Add(enantiomer, primary)


    let updateSimilarReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (similar : list<ReactionRateData<'R>>) =

        similar |> List.map (fun e -> if d.ContainsKey e.reaction |> not then d.Add(e.reaction, e.rateData)) |> ignore
        similar |> List.map (fun e -> if d.ContainsKey (getEnantiomer e.reaction) |> not then d.Add(getEnantiomer e.reaction, e.rateData)) |> ignore


    let updateRelatedReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (r : 'R)
        (x : RelatedReactions<'R>) =

        updatePrimaryReactions d getEnantiomer x.primary r
        updateSimilarReactions d getEnantiomer x.similar
        x.primary


    let getRatesImpl<'R> 
        (rateDictionary : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (calculateRates : 'R -> RelatedReactions<'R>)
        (reaction : 'R)  =
        match rateDictionary.TryGetValue reaction with
        | true, rates -> rates
        | false, _ ->
            calculateRates reaction
            |> updateRelatedReactions rateDictionary getEnantiomer reaction


    //let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
    //    | None -> (None, None)
    //
    //
    //let inline getModelRates2<'M, 'R when 'M : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (t : RateGenerationType) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option)) (m, t, r))
    //    | None -> (None, None)


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


    type CatRatesSimInfo<'A, 'R, 'C, 'RC> =
        {
            reaction : 'R
            catalyst : 'C
            aminoAcids : list<'A>
            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC
            simReactionCreator : 'A -> 'R
            getCatReactEnantiomer : 'RC -> 'RC
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            getBaseCatRates : 'RC -> RateData // Get rates of underlying catalyzed reaction.
            simParams : CatRatesSimilarityParam
            eeParams : CatRatesEeParam
            rateDictionary : Dictionary<'RC, RateData>
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

        member i.toCatRatesInfo r c e =
            {
                reaction = r
                catalyst = c
                getCatEnantiomer = i.getCatEnantiomer
                catReactionCreator = i.catReactionCreator
                getBaseRates = i.getBaseRates
                eeParams = e
                rateGenerationType = i.rateGenerationType
                rnd = i.rnd
            }


    let calculateSimRates<'R, 'C, 'RC> (i : CatRatesSimInfo<AminoAcid, 'R, 'C, 'RC>) =
        let r = (i.reaction, i.catalyst) |> i.catReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator
        let br = i.getBaseRates i.reaction // (bf, bb)
        let cr = r |> i.getBaseCatRates // (f, b)

        let calculateCatRates s c e =
            let reaction = (s, c) |> i.catReactionCreator
            let related = i.toCatRatesInfo s c e |> calculateCatRates
            updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer reaction related

        match (cr.forwardRate, cr.backwardRate) with
        | None, None ->
            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a)
            |> List.map (fun e -> calculateCatRates e i.catalyst CatRatesEeParam.defaultValue)
            |> ignore
        | _ ->
            let cre = re |> i.getBaseCatRates

            let rateMult =
                match cr.forwardRate, cre.forwardRate, cr.backwardRate, cre.backwardRate with
                | Some (ReactionRate a), Some (ReactionRate b), _, _ ->
                    match br.forwardRate with
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #1..."
                | _, _, Some (ReactionRate a), Some (ReactionRate b) ->
                    match br.backwardRate with
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #2..."
                | _ -> failwith "calculateSimRates::calculateCatRates::FUBAR #3..."

            let getEeParams d =
                match d with
                | true ->
                    {
                        rateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                        eeForwardDistribution = i.simParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
                        eeBackwardDistribution = i.simParams.getBackwardEeDistr.getDistr cr.backwardRate cre.backwardRate
                    }
                | false -> CatRatesEeParam.defaultValue

            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a, i.simParams.simBaseDistribution.isDefined i.rnd)
            |> List.map (fun (e, b) -> calculateCatRates e i.catalyst (getEeParams b))
            |> ignore

        cr


    [<Literal>]
    let FoodCreationParamName = "FoodCreationParam"

    type FoodCreationParam =
        {
            foodCreationRate : double
        }

        member this.name = FoodCreationParamName

        static member allNames =
            [
                FoodCreationParamName
            ]


    let getAllRatesImpl (d : Dictionary<'R, RateData>) =
        d
        |> Seq.map (|KeyValue|)
        |> List.ofSeq
        |> List.map (fun (r, d) -> { reaction = r; rateData = d })


    [<AbstractClass>]
    type RateModel<'P, 'R when 'R : equality> (p : 'P) =
        let rateDictionaryImpl = new Dictionary<'R, RateData>()
        member __.rateDictionary = rateDictionaryImpl
        member __.inputParams = p

        member __.getAllRates() = getAllRatesImpl rateDictionaryImpl


    type FoodCreationModel (p : FoodCreationParam) =
        inherit RateModel<FoodCreationParam, FoodCreationReaction>(p)
        let calculateRates _ = getRates (Some p.foodCreationRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    [<Literal>]
    let WasteRemovalParamName = "WasteRemovalParam"

    type WasteRemovalParam =
        {
            wasteRemovalRate : double
        }

        member this.name = WasteRemovalParamName

        static member allNames =
            [
                WasteRemovalParamName
            ]


    type WasteRemovalModel (p : WasteRemovalParam) =
        inherit RateModel<WasteRemovalParam, WasteRemovalReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRemovalRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    [<Literal>]
    let WasteRecyclingParamName = "WasteRecyclingParam"

    type WasteRecyclingParam =
        {
            wasteRecyclingRate : double
        }

        member this.name = WasteRecyclingParamName

        static member allNames =
            [
                WasteRecyclingParamName
            ]


    type WasteRecyclingModel (p : WasteRecyclingParam) =
        inherit RateModel<WasteRecyclingParam, WasteRecyclingReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRecyclingRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    [<Literal>]
    let SynthesisRandomParamName = "SynthesisRandomParam"

    type SynthesisRandomParam =
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        member this.name = SynthesisRandomParamName

        static member allNames =
            [
                SynthesisRandomParamName
            ]


    [<Literal>]
    let SynthesisParamName = "SynthesisParam"


    [<Literal>]
    let SynthRndParamName = "SynthRndParam"


    type SynthesisParam =
        | SynthRndParam of SynthesisRandomParam

        member this.name =
            match this with
            | SynthRndParam _ -> SynthRndParamName

        static member allNames =
            [
                SynthRndParamName
            ]


    type SynthesisRandomModel (p : SynthesisRandomParam) =
        inherit RateModel<SynthesisRandomParam, SynthesisReaction>(p)

        let calculateRates rnd _ =
            let d = p.synthesisDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type SynthesisModel =
        | SynthRndModel of SynthesisRandomModel

        member model.getRates rnd r =
            match model with
            | SynthRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SynthRndModel m -> m.inputParams |> SynthRndParam

        member model.getAllRates() =
            match model with
            | SynthRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | SynthRndParam q -> SynthesisRandomModel q |> SynthRndModel


    [<Literal>]
    let CatalyticSynthesisRandomParamName = "CatalyticSynthesisRandomParam"

    type CatalyticSynthesisRandomParam =
        {
            catSynthRndEeParams : CatRatesEeParam
        }

        member this.name = CatalyticSynthesisRandomParamName

        static member allNames =
            [
                CatalyticSynthesisRandomParamName
            ]


    [<Literal>]
    let CatalyticSynthesisParamName = "CatalyticSynthesisParam"


    [<Literal>]
    let CatSynthRndParamName = "CatSynthRndParam"


    [<Literal>]
    let CatSynthSimParamName = "CatSynthSimParam"


    type CatalyticSynthesisParam =
        | CatSynthRndParam of CatalyticSynthesisRandomParam
        | CatSynthSimParam of CatRatesSimilarityParam

        member this.name =
            match this with
            | CatSynthRndParam _ -> CatSynthRndParamName
            | CatSynthSimParam _ -> CatSynthSimParamName

        static member allNames =
            [
                CatSynthRndParamName
                CatSynthSimParamName
            ]


    type CatalyticSynthesisRandomParamWithModel =
        {
            catSynthRndParam : CatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type CatalyticSynthesisRandomModel (p : CatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<CatalyticSynthesisRandomParamWithModel, CatalyticSynthesisReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                eeParams = p.catSynthRndParam.catSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticSynthesisSimilarParamWithModel =
        {
            catSynthModel : CatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisParamWithModel =
        | CatSynthRndParamWithModel of CatalyticSynthesisRandomParamWithModel
        | CatSynthSimParamWithModel of CatalyticSynthesisSimilarParamWithModel


    type CatalyticSynthesisSimilarModel (p : CatalyticSynthesisSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> SynthesisReaction)
                getBaseRates = p.catSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.catSynthModel.getRates rnd t
                simParams = p.catSynthSimParam
                eeParams = p.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams
                rateDictionary = p.catSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catSynthModel.rateDictionary


    type CatalyticSynthesisModel =
        | CatSynthRndModel of CatalyticSynthesisRandomModel
        | CatSynthSimModel of CatalyticSynthesisSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatSynthRndModel m -> m.getRates rnd t r
            | CatSynthSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatSynthRndModel m -> m.inputParams |> CatSynthRndParamWithModel
            | CatSynthSimModel m -> m.inputParams |> CatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatSynthRndModel m -> m.getAllRates()
            | CatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatSynthRndParamWithModel q -> CatalyticSynthesisRandomModel q |> CatSynthRndModel
            | CatSynthSimParamWithModel q -> CatalyticSynthesisSimilarModel q |> CatSynthSimModel


    [<Literal>]
    let DestructionRandomParamName = "DestructionRandomParam"

    type DestructionRandomParam =
        {
            destructionDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        member this.name = DestructionRandomParamName

        static member allNames =
            [
                DestructionRandomParamName
            ]


    [<Literal>]
    let DestructionParamName = "DestructionParam"


    [<Literal>]
    let DestrRndParamName = "DestrRndParam"


    type DestructionParam =
        | DestrRndParam of DestructionRandomParam

        member this.name =
            match this with
            | DestrRndParam _ -> DestrRndParamName

        static member allNames =
            [
                DestrRndParamName
            ]


    type DestructionRandomModel (p : DestructionRandomParam) =
        inherit RateModel<DestructionRandomParam, DestructionReaction>(p)

        let calculateRates rnd _ =
            let d = p.destructionDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type DestructionModel =
        | DestrRndModel of DestructionRandomModel

        member model.getRates rnd r =
            match model with
            | DestrRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | DestrRndModel m -> m.inputParams |> DestrRndParam

        member model.getAllRates() =
            match model with
            | DestrRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | DestrRndParam q -> DestructionRandomModel q |> DestrRndModel


    [<Literal>]
    let CatalyticDestructionRandomParamName = "CatalyticDestructionRandomParam"

    type CatalyticDestructionRandomParam =
        {
            catDestrRndEeParams : CatRatesEeParam
        }

        member this.name = CatalyticDestructionRandomParamName


    [<Literal>]
    let CatalyticDestructionParamName = "CatalyticDestructionParam"


    [<Literal>]
    let CatDestrRndParamName = "CatDestrRndParam"


    [<Literal>]
    let CatDestrSimParamName = "CatDestrSimParam"


    type CatalyticDestructionParam =
        | CatDestrRndParam of CatalyticDestructionRandomParam
        | CatDestrSimParam of CatRatesSimilarityParam

        member this.name =
            match this with
            | CatDestrRndParam _ -> CatDestrRndParamName
            | CatDestrSimParam _ -> CatDestrSimParamName

        static member allNames =
            [
                CatDestrRndParamName
                CatDestrSimParamName
            ]


    type CatalyticDestructionRandomParamWithModel =
        {
            catDestrRndParam : CatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type CatalyticDestructionRandomModel (p : CatalyticDestructionRandomParamWithModel) =
        inherit RateModel<CatalyticDestructionRandomParamWithModel, CatalyticDestructionReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                eeParams = p.catDestrRndParam.catDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticDestructionSimilarParamWithModel =
        {
            catDestrSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catDestrModel : CatalyticDestructionRandomModel
        }


    type CatalyticDestructionParamWithModel =
        | CatDestrRndParamWithModel of CatalyticDestructionRandomParamWithModel
        | CatDestrSimParamWithModel of CatalyticDestructionSimilarParamWithModel


    type CatalyticDestructionSimilarModel (p : CatalyticDestructionSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticDestructionReaction (s, c)) = 
            let (DestructionReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> DestructionReaction)
                getBaseRates = p.catDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.catDestrModel.getRates rnd t
                simParams = p.catDestrSimParam
                eeParams = p.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams
                rateDictionary = p.catDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catDestrModel.rateDictionary


    type CatalyticDestructionModel =
        | CatDestrRndModel of CatalyticDestructionRandomModel
        | CatDestrSimModel of CatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatDestrRndModel m -> m.getRates rnd t r
            | CatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatDestrRndModel m -> m.inputParams |> CatDestrRndParamWithModel
            | CatDestrSimModel m -> m.inputParams |> CatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatDestrRndModel m -> m.getAllRates()
            | CatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatDestrRndParamWithModel q -> CatalyticDestructionRandomModel q |> CatDestrRndModel
            | CatDestrSimParamWithModel q -> CatalyticDestructionSimilarModel q |> CatDestrSimModel


    [<Literal>]
    let SedimentationDirectRandomParamName = "SedimentationDirectRandomParam"

    type SedimentationDirectRandomParam =
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }

        member this.name = SedimentationDirectRandomParamName

        static member allNames =
            [
                SedimentationDirectRandomParamName
            ]


    [<Literal>]
    let SedimentationDirectParamName = "SedimentationDirectParam"


    [<Literal>]
    let SedDirRndParamName = "SedDirRndParam"


    type SedimentationDirectParam =
        | SedDirRndParam of SedimentationDirectRandomParam

        member this.name =
            match this with
            | SedDirRndParam _ -> SedDirRndParamName

        static member allNames =
            [
                SedDirRndParamName
            ]


    type SedimentationDirectRandomModel (p : SedimentationDirectRandomParam) =
        inherit RateModel<SedimentationDirectRandomParam, SedimentationDirectReaction>(p)

        let calculateRates rnd t _ =
            let k =
                match t with
                | BruteForce -> p.sedimentationDirectDistribution.nextDoubleOpt rnd
                | RandomChoice -> p.sedimentationDirectDistribution.nextDouble rnd |> Some
            getForwardRates (p.forwardScale, k)

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd t) r


    type SedimentationDirectModel =
        | SedDirRndModel of SedimentationDirectRandomModel

        member model.getRates rnd t r =
            match model with
            | SedDirRndModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | SedDirRndModel m -> m.inputParams |> SedDirRndParam

        member model.getAllRates() =
            match model with
            | SedDirRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | SedDirRndParam q -> SedimentationDirectRandomModel q |> SedDirRndModel


    [<Literal>]
    let SedimentationAllRandomParamName = "SedimentationAllRandomParam"

    type SedimentationAllRandomParam =
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }

        member this.name = SedimentationAllRandomParamName

        static member allNames =
            [
                SedimentationAllRandomParamName
            ]


    [<Literal>]
    let SedimentationAllParamName = "SedimentationAllParam"


    [<Literal>]
    let SedAllRndParamName = "SedAllRndParam"


    type SedimentationAllParam =
        | SedAllRndParam of SedimentationAllRandomParam

        member this.name =
            match this with
            | SedAllRndParam _ -> SedAllRndParamName

        static member allNames =
            [
                SedAllRndParamName
            ]


    type SedimentationAllRandomModel (p : SedimentationAllRandomParam) =
        inherit RateModel<SedimentationAllRandomParam, SedimentationAllReaction>(p)
        let calculateRates rnd _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble rnd |> Some)
        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type SedimentationAllModel =
        | SedAllRndModel of SedimentationAllRandomModel

        member model.getRates rnd r =
            match model with
            | SedAllRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SedAllRndModel m -> m.inputParams |> SedAllRndParam

        member model.getAllRates() =
            match model with
            | SedAllRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | SedAllRndParam q -> SedimentationAllRandomModel q |> SedAllRndModel


    [<Literal>]
    let LigationRandomParamName = "LigationRandomParam"

    type LigationRandomParam =
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        member this.name = LigationRandomParamName

        static member allNames =
            [
                LigationRandomParamName
            ]


    [<Literal>]
    let LigationParamName = "LigationParam"


    [<Literal>]
    let LigRndParamName = "LigRndParam"


    type LigationParam =
        | LigRndParam of LigationRandomParam

        member this.name =
            match this with
            | LigRndParam _ -> LigRndParamName

        static member allNames =
            [
                LigRndParamName
            ]


    type LigationRandomModel (p : LigationRandomParam) =
        inherit RateModel<LigationRandomParam, LigationReaction>(p)

        let calculateRates rnd _ =
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type LigationModel =
        | LigRndModel of LigationRandomModel

        member model.getRates rnd r =
            match model with
            | LigRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | LigRndModel m -> m.inputParams |> LigRndParam

        member model.getAllRates() =
            match model with
            | LigRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | LigRndParam q -> LigationRandomModel q |> LigRndModel


    [<Literal>]
    let CatalyticLigationRandomParamName = "CatalyticLigationRandomParam"

    type CatalyticLigationRandomParam =
        {
            catLigRndEeParams : CatRatesEeParam
        }

        member this.name = CatalyticLigationRandomParamName

        static member allNames =
            [
                CatalyticLigationRandomParamName
            ]


    [<Literal>]
    let CatalyticLigationParamName = "CatalyticLigationParam"


    [<Literal>]
    let CatLigRndParamName = "CatLigRndParam"


    type CatalyticLigationParam =
        | CatLigRndParam of CatalyticLigationRandomParam

        member this.name =
            match this with
            | CatLigRndParam _ -> CatLigRndParamName

        static member allNames =
            [
                CatLigRndParamName
            ]


    type CatalyticLigationRandomParamWithModel =
        {
            catLigationParam : CatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type CatalyticLigationParamWithModel =
        | CatLigRndParamWithModel of CatalyticLigationRandomParamWithModel

        member p.catLigationParam =
            match p with 
            | CatLigRndParamWithModel q -> q.catLigationParam


    type CatalyticLigationRandomModel (p : CatalyticLigationRandomParamWithModel) =
        inherit RateModel<CatalyticLigationRandomParamWithModel, CatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticLigationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                eeParams = p.catLigationParam.catLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticLigationModel =
        | CatLigRndModel of CatalyticLigationRandomModel

        member model.getRates rnd t r =
            match model with
            | CatLigRndModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatLigRndModel m -> m.inputParams |> CatLigRndParamWithModel

        member model.getAllRates() =
            match model with
            | CatLigRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatLigRndParamWithModel q -> CatalyticLigationRandomModel q |> CatLigRndModel


    [<Literal>]
    let RacemizationRandomParamName = "RacemizationRandomParam"

    type RacemizationRandomParam =
        {
            racemizationDistribution : Distribution
            forwardScale : double option
        }

        member this.name = RacemizationRandomParamName

        static member allNames =
            [
                RacemizationRandomParamName
            ]


    [<Literal>]
    let RacemizationParamName = "RacemizationParam"


    [<Literal>]
    let RacemRndParamName = "RacemRndParam"


    type RacemizationParam =
        | RacemRndParam of RacemizationRandomParam

        member this.name =
            match this with
            | RacemRndParam _ -> RacemRndParamName

        static member allNames =
            [
                RacemRndParamName
            ]


    type RacemizationRandomModel (p : RacemizationRandomParam) =
        inherit RateModel<RacemizationRandomParam, RacemizationReaction>(p)

        let calculateRates rnd _ =
            let d = p.racemizationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type RacemizationModel =
        | RacemRndModel of RacemizationRandomModel

        member model.getRates rnd r =
            match model with
            | RacemRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | RacemRndModel m -> m.inputParams |> RacemRndParam

        member model.getAllRates() =
            match model with
            | RacemRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | RacemRndParam q -> RacemizationRandomModel q |> RacemRndModel


    [<Literal>]
    let CatalyticRacemizationRandomParamName = "CatalyticRacemizationRandomParam"

    type CatalyticRacemizationRandomParam =
        {
            catRacemRndEeParams : CatRatesEeParam
        }

        member this.name = CatalyticRacemizationRandomParamName

        static member allNames =
            [
                CatalyticRacemizationRandomParamName
            ]


    [<Literal>]
    let CatalyticRacemizationParamName = "CatalyticRacemizationParam"

    [<Literal>]
    let CatRacemRndParamName = "CatRacemRndParam"

    [<Literal>]
    let CatRacemSimParamName = "CatRacemSimParam"


    type CatalyticRacemizationParam =
        | CatRacemRndParam of CatalyticRacemizationRandomParam
        | CatRacemSimParam of CatRatesSimilarityParam

        member this.name =
            match this with
            | CatRacemRndParam _ -> CatRacemRndParamName
            | CatRacemSimParam _ -> CatRacemSimParamName

        static member allNames =
            [
                CatRacemRndParamName
                CatRacemSimParamName
            ]


    type CatalyticRacemizationRandomParamWithModel =
        {
            catRacemRndParam : CatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
            aminoAcids : list<AminoAcid>
        }


    type CatalyticRacemizationRandomModel (p : CatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<CatalyticRacemizationRandomParamWithModel, CatalyticRacemizationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates rnd
                eeParams = p.catRacemRndParam.catRacemRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticRacemizationSimilarParamWithModel =
        {
            catRacemSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catRacemModel : CatalyticRacemizationRandomModel
        }


    type CatalyticRacemizationParamWithModel =
        | CatRacemRndParamWithModel of CatalyticRacemizationRandomParamWithModel
        | CatRacemSimParamWithModel of CatalyticRacemizationSimilarParamWithModel


    type CatalyticRacemizationSimilarModel (p : CatalyticRacemizationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> RacemizationReaction)
                getBaseRates = p.catRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.catRacemModel.getRates rnd t
                simParams = p.catRacemSimParam
                eeParams = p.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams
                rateDictionary = p.catRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catRacemModel.rateDictionary


    type CatalyticRacemizationModel =
        | CatRacemRndModel of CatalyticRacemizationRandomModel
        | CatRacemSimModel of CatalyticRacemizationSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatRacemRndModel m -> m.getRates rnd t r
            | CatRacemSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatRacemRndModel m -> m.inputParams |> CatRacemRndParamWithModel
            | CatRacemSimModel m -> m.inputParams |> CatRacemSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatRacemRndModel m -> m.getAllRates()
            | CatRacemSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatRacemRndParamWithModel q -> CatalyticRacemizationRandomModel q |> CatRacemRndModel
            | CatRacemSimParamWithModel q -> CatalyticRacemizationSimilarModel q |> CatRacemSimModel


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


    [<Literal>]
    let ReactionRateModelParamName = "ReactionRateModelParam"

    [<Literal>]
    let FoodCreationRateParamName = "FoodCreationRateParam"

    [<Literal>]
    let WasteRemovalRateParamName = "WasteRemovalRateParam"

    [<Literal>]
    let WasteRecyclingRateParamName = "WasteRecyclingRateParam"

    [<Literal>]
    let SynthesisRateParamName = "SynthesisRateParam"

    [<Literal>]
    let DestructionRateParamName = "DestructionRateParam"

    [<Literal>]
    let CatalyticSynthesisRateParamName = "CatalyticSynthesisRateParam"

    [<Literal>]
    let CatalyticDestructionRateParamName = "CatalyticDestructionRateParam"

    [<Literal>]
    let LigationRateParamName = "LigationRateParam"

    [<Literal>]
    let CatalyticLigationRateParamName = "CatalyticLigationRateParam"

    [<Literal>]
    let SedimentationDirectRateParamName = "SedimentationDirectRateParam"

    [<Literal>]
    let SedimentationAllRateParamName = "SedimentationAllRateParam"

    [<Literal>]
    let RacemizationRateParamName = "RacemizationRateParam"

    [<Literal>]
    let CatalyticRacemizationRateParamName = "CatalyticRacemizationRateParam"


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

        member this.name =
            match this with
            | FoodCreationRateParam _ -> FoodCreationRateParamName
            | WasteRemovalRateParam _ -> WasteRemovalRateParamName
            | WasteRecyclingRateParam _ -> WasteRecyclingRateParamName
            | SynthesisRateParam _ -> SynthesisRateParamName
            | DestructionRateParam _ -> DestructionRateParamName
            | CatalyticSynthesisRateParam _ -> CatalyticSynthesisRateParamName
            | CatalyticDestructionRateParam _ -> CatalyticDestructionRateParamName
            | LigationRateParam _ -> LigationRateParamName
            | CatalyticLigationRateParam _ -> CatalyticLigationRateParamName
            | SedimentationDirectRateParam _ -> SedimentationDirectRateParamName
            | SedimentationAllRateParam _ -> SedimentationAllRateParamName
            | RacemizationRateParam _ -> RacemizationRateParamName
            | CatalyticRacemizationRateParam _ -> CatalyticRacemizationRateParamName

        member this.paramName =
            match this with
            | FoodCreationRateParam p -> p.name
            | WasteRemovalRateParam p -> p.name
            | WasteRecyclingRateParam p -> p.name
            | SynthesisRateParam p -> p.name
            | DestructionRateParam p -> p.name
            | CatalyticSynthesisRateParam p -> p.name
            | CatalyticDestructionRateParam p -> p.name
            | LigationRateParam p -> p.name
            | CatalyticLigationRateParam p -> p.name
            | SedimentationDirectRateParam p -> p.name
            | SedimentationAllRateParam p -> p.name
            | RacemizationRateParam p -> p.name
            | CatalyticRacemizationRateParam p -> p.name

        member this.variableName = this.name |> toVariableName
        member this.variableParamName = this.paramName |> toVariableName

        static member allNames =
            [
                FoodCreationRateParamName
                WasteRemovalRateParamName
                WasteRecyclingRateParamName
                SynthesisRateParamName
                DestructionRateParamName
                CatalyticSynthesisRateParamName
                CatalyticDestructionRateParamName
                LigationRateParamName
                CatalyticLigationRateParamName
                SedimentationDirectRateParamName
                SedimentationAllRateParamName
                RacemizationRateParamName
                CatalyticRacemizationRateParamName
            ]

        static member allVariableNames =
            ReactionRateModelParam.allNames |> List.map (fun e -> toVariableName e)

        static member allParamNames =
            [
                FoodCreationParam.allNames
                WasteRemovalParam.allNames
                WasteRecyclingParam.allNames
                SynthesisParam.allNames
                DestructionParam.allNames
                CatalyticSynthesisParam.allNames
                CatalyticDestructionParam.allNames
                LigationParam.allNames
                CatalyticLigationParam.allNames
                SedimentationDirectParam.allNames
                SedimentationAllParam.allNames
                RacemizationParam.allNames
                CatalyticRacemizationParam.allNames
            ]
            |> List.concat

        static member allVariableParamNames =
            ReactionRateModelParam.allParamNames |> List.map (fun e -> toVariableName e)


    [<Literal>]
    let ReactionRateModelParamUsageName = "ReactionRateModelParamUsage"

    [<Literal>]
    let PrimaryParamName = "PrimaryParam"

    [<Literal>]
    let DependsOnParamName = "DependsOnParam"

    type ReactionRateModelParamUsage =
        | PrimaryParam
        | DependsOnParam

        member this.name =
            match this with
            | PrimaryParam -> PrimaryParamName
            | DependsOnParam -> DependsOnParamName


    type ReactionRateModelParamWithUsage =
        {
            modelParam : ReactionRateModelParam
            usage : ReactionRateModelParamUsage
        }


    [<CustomEquality>]
    [<CustomComparison>]
    type ReactionRateModel =
        | FoodCreationRateModel of FoodCreationModel
        | WasteRemovalRateModel of WasteRemovalModel
        | WasteRecyclingRateModel of WasteRecyclingModel
        | SynthesisRateModel of SynthesisModel
        | DestructionRateModel of DestructionModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | CatalyticDestructionRateModel of CatalyticDestructionModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel
        | RacemizationRateModel of RacemizationModel
        | CatalyticRacemizationRateModel of CatalyticRacemizationModel

        member rm.name =
            match rm with
            | FoodCreationRateModel _ -> "FoodCreationRateModel"
            | WasteRemovalRateModel _ -> "WasteRemovalRateModel"
            | WasteRecyclingRateModel _ -> "WasteRecyclingRateModel"
            | SynthesisRateModel _ -> "SynthesisRateModel"
            | DestructionRateModel _ -> "DestructionRateModel"
            | CatalyticSynthesisRateModel v ->
                match v with
                | CatSynthRndModel _ -> "CatSynthRndModel"
                | CatSynthSimModel _ -> "CatSynthSimModel"
            | CatalyticDestructionRateModel v ->
                match v with
                | CatDestrRndModel _ -> "CatDestrRndModel"
                | CatDestrSimModel _ -> "CatDestrSimModel"
            | LigationRateModel _ -> "LigationRateModel"
            | CatalyticLigationRateModel v ->
                match v with
                | CatLigRndModel _ -> "CatLigRndModel"
            | SedimentationDirectRateModel _ -> "SedimentationDirectRateModel"
            | SedimentationAllRateModel _ -> "SedimentationAllRateModel"
            | RacemizationRateModel _ -> "RacemizationRateModel"
            | CatalyticRacemizationRateModel v ->
                match v with
                | CatRacemRndModel _ -> "CatRacemRndModel"
                | CatRacemSimModel _ -> "CatRacemSimModel"

        member rm.inputParams =
            match rm with
            | FoodCreationRateModel m -> m.inputParams |> FoodCreationRateParam
            | WasteRemovalRateModel m -> m.inputParams |> WasteRemovalRateParam
            | WasteRecyclingRateModel m -> m.inputParams |> WasteRecyclingRateParam
            | SynthesisRateModel m -> m.inputParams |> SynthesisRateParam
            | DestructionRateModel m -> m.inputParams |> DestructionRateParam
            | CatalyticSynthesisRateModel v ->
                match v with 
                | CatSynthRndModel m -> m.inputParams.catSynthRndParam |> CatSynthRndParam |> CatalyticSynthesisRateParam
                | CatSynthSimModel m -> m.inputParams.catSynthSimParam |> CatSynthSimParam |> CatalyticSynthesisRateParam
            | CatalyticDestructionRateModel v ->
                match v with 
                | CatDestrRndModel m -> m.inputParams.catDestrRndParam |> CatDestrRndParam |> CatalyticDestructionRateParam
                | CatDestrSimModel m -> m.inputParams.catDestrSimParam |> CatDestrSimParam |> CatalyticDestructionRateParam
            | LigationRateModel m -> m.inputParams |> LigationRateParam
            | CatalyticLigationRateModel v ->
                match v with 
                | CatLigRndModel m -> m.inputParams.catLigationParam |> CatLigRndParam |> CatalyticLigationRateParam
            | SedimentationDirectRateModel m -> m.inputParams |> SedimentationDirectRateParam
            | SedimentationAllRateModel m -> m.inputParams |> SedimentationAllRateParam
            | RacemizationRateModel m -> m.inputParams |> RacemizationRateParam
            | CatalyticRacemizationRateModel v ->
                match v with 
                | CatRacemRndModel m -> m.inputParams.catRacemRndParam |> CatRacemRndParam |> CatalyticRacemizationRateParam
                | CatRacemSimModel m -> m.inputParams.catRacemSimParam |> CatRacemSimParam |> CatalyticRacemizationRateParam

        member rm.dependsOn =
            match rm with
            | FoodCreationRateModel _ -> []
            | WasteRemovalRateModel _ -> []
            | WasteRecyclingRateModel _ -> []
            | SynthesisRateModel _ -> []
            | DestructionRateModel _ -> []
            | CatalyticSynthesisRateModel v ->
                match v with
                | CatSynthRndModel m -> [ m.inputParams.synthesisModel |> SynthesisRateModel ]
                | CatSynthSimModel m -> [ m.inputParams.catSynthModel |> CatSynthRndModel |> CatalyticSynthesisRateModel ]
            | CatalyticDestructionRateModel v ->
                match v with
                | CatDestrRndModel m -> [ m.inputParams.destructionModel |> DestructionRateModel ]
                | CatDestrSimModel m -> [ m.inputParams.catDestrModel |> CatDestrRndModel |> CatalyticDestructionRateModel ]
            | LigationRateModel _ -> []
            | CatalyticLigationRateModel v -> 
                match v with
                | CatLigRndModel m -> [ m.inputParams.ligationModel |> LigationRateModel ]
            | SedimentationDirectRateModel _ -> []
            | SedimentationAllRateModel _ -> []
            | RacemizationRateModel _ -> []
            | CatalyticRacemizationRateModel v ->
                match v with
                | CatRacemRndModel m -> [ m.inputParams.racemizationModel |> RacemizationRateModel ]
                | CatRacemSimModel m -> [ m.inputParams.catRacemModel |> CatRacemRndModel |> CatalyticRacemizationRateModel ]

        member rm.getAllRates() =
            match rm with
            | FoodCreationRateModel m -> m.getAllRates() |> FoodCreationRates
            | WasteRemovalRateModel m -> m.getAllRates() |> WasteRemovalRates
            | WasteRecyclingRateModel m -> m.getAllRates() |> WasteRecyclingRates
            | SynthesisRateModel m -> m.getAllRates() |> SynthesisRates
            | DestructionRateModel m -> m.getAllRates() |> DestructionRates
            | CatalyticSynthesisRateModel m -> m.getAllRates() |> CatalyticSynthesisRates
            | CatalyticDestructionRateModel m -> m.getAllRates() |> CatalyticDestructionRates
            | LigationRateModel m -> m.getAllRates() |> LigationRates
            | CatalyticLigationRateModel m -> m.getAllRates() |> CatalyticLigationRates
            | SedimentationDirectRateModel m -> m.getAllRates() |> SedimentationDirectRates
            | SedimentationAllRateModel m -> m.getAllRates() |> SedimentationAllRates
            | RacemizationRateModel m -> m.getAllRates() |> RacemizationRates
            | CatalyticRacemizationRateModel m -> m.getAllRates() |> CatalyticRacemizationRates

        override this.Equals (o: obj) =
            match o with
            | :? ReactionRateModel as rm -> this.inputParams = rm.inputParams
            | _ -> false

        override this.GetHashCode() = hash (this.name, this.inputParams)

        interface IEquatable<ReactionRateModel> with
            member this.Equals(that : ReactionRateModel) = this.Equals(that)

        interface IComparable with
            member this.CompareTo(thatObj) =
                match thatObj with
                | :? ReactionRateModel as that ->
                    compare (this.name, this.inputParams) (that.name, that.inputParams)
                | _ ->
                    raise <| ArgumentException("Can't compare instances of different types.")


    let rec allDep (rm : ReactionRateModel) (acc : list<ReactionRateModel>) =
        match rm.dependsOn with 
        | [] -> acc
        | l -> l |> List.fold (fun a r -> allDep r (r :: a)) acc


    type ReactionRateModelWithUsage =
        {
            model : ReactionRateModel
            usage : ReactionRateModelParamUsage
        }


    type ReactionRateProviderParams =
        {
            rateModels: list<ReactionRateModel>
        }

        member p.tryFindFoodCreationModel() = p.rateModels |> List.tryPick (fun e -> match e with | FoodCreationRateModel m -> Some m | _ -> None)
        member p.tryFindWasteRemovalModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRemovalRateModel m -> Some m | _ -> None)
        member p.tryFindWasteRecyclingModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRecyclingRateModel m -> Some m | _ -> None)
        member p.tryFindSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | SynthesisRateModel m -> Some m | _ -> None)
        member p.tryFindDestructionModel() = p.rateModels |> List.tryPick (fun e -> match e with | DestructionRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticSynthesisRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticDestructionModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticDestructionRateModel m -> Some m | _ -> None)
        member p.tryFindLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | LigationRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticLigationRateModel m -> Some m | _ -> None)
        member p.tryFindSedimentationDirectModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationDirectRateModel m -> Some m | _ -> None)
        member p.tryFindSedimentationAllModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationAllRateModel m -> Some m | _ -> None)
        member p.tryFindRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | RacemizationRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticRacemizationRateModel m -> Some m | _ -> None)

        member p.allModels =
            let prim = p.rateModels |> Set.ofList
            let dep = Set.difference (p.rateModels |> List.map (fun e -> allDep e []) |> List.concat |> Set.ofList) prim

            (prim |> Set.map (fun e -> { model = e; usage = PrimaryParam }))
            |> Set.union (dep |> Set.map (fun e -> { model = e; usage = DependsOnParam }))
            |> Set.toList

        member p.allParams = p.allModels |> List.map (fun e -> { modelParam = e.model.inputParams; usage = e.usage }) |> List.sort


    let bind f xOpt =
        match xOpt with
        | Some x -> f x
        | _ -> { forwardRate = None; backwardRate = None }


    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl rnd t a =
            match a with
            | FoodCreation r -> p.tryFindFoodCreationModel() |> bind (fun m -> m.getRates r)
            | WasteRemoval r -> p.tryFindWasteRemovalModel() |> bind (fun m -> m.getRates r)
            | WasteRecycling r -> p.tryFindWasteRecyclingModel() |> bind (fun m -> m.getRates r)
            | Synthesis r -> p.tryFindSynthesisModel() |> bind (fun m -> m.getRates rnd r)
            | Destruction r -> p.tryFindDestructionModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticSynthesis r -> p.tryFindCatalyticSynthesisModel() |> bind (fun m -> m.getRates rnd t r)
            | CatalyticDestruction r -> p.tryFindCatalyticDestructionModel() |> bind (fun m -> m.getRates rnd t r)
            | Ligation r -> p.tryFindLigationModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticLigation r -> p.tryFindCatalyticLigationModel() |> bind (fun m -> m.getRates rnd t r)
            | SedimentationDirect r -> p.tryFindSedimentationDirectModel() |> bind (fun m -> m.getRates rnd t r)
            | SedimentationAll r -> p.tryFindSedimentationAllModel() |> bind (fun m -> m.getRates rnd r)
            | Racemization r -> p.tryFindRacemizationModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticRacemization r -> p.tryFindCatalyticRacemizationModel() |> bind (fun m -> m.getRates rnd t r)

        let getModelImpl n =
            match n with
            | FoodCreationName -> p.tryFindFoodCreationModel() |> Option.bind(fun e -> FoodCreationRateModel e |> Some)
            | WasteRemovalName -> p.tryFindWasteRemovalModel() |> Option.bind(fun e -> WasteRemovalRateModel e |> Some)
            | WasteRecyclingName -> p.tryFindWasteRecyclingModel() |> Option.bind(fun e -> WasteRecyclingRateModel e |> Some)
            | SynthesisName -> p.tryFindSynthesisModel() |> Option.bind(fun e -> SynthesisRateModel e |> Some)
            | DestructionName -> p.tryFindDestructionModel() |> Option.bind(fun e -> DestructionRateModel e |> Some)
            | CatalyticSynthesisName -> p.tryFindCatalyticSynthesisModel() |> Option.bind(fun e -> CatalyticSynthesisRateModel e |> Some)
            | CatalyticDestructionName -> p.tryFindCatalyticDestructionModel() |> Option.bind(fun e -> CatalyticDestructionRateModel e |> Some)
            | LigationName -> p.tryFindLigationModel() |> Option.bind(fun e -> LigationRateModel e |> Some)
            | CatalyticLigationName -> p.tryFindCatalyticLigationModel() |> Option.bind(fun e -> CatalyticLigationRateModel e |> Some)
            | SedimentationDirectName -> p.tryFindSedimentationDirectModel() |> Option.bind(fun e -> SedimentationDirectRateModel e |> Some)
            | SedimentationAllName -> p.tryFindSedimentationAllModel() |> Option.bind(fun e -> SedimentationAllRateModel e |> Some)
            | RacemizationName -> p.tryFindRacemizationModel() |> Option.bind(fun e -> RacemizationRateModel e |> Some)
            | CatalyticRacemizationName -> p.tryFindCatalyticRacemizationModel() |> Option.bind(fun e -> CatalyticRacemizationRateModel e |> Some)

        member __.providerParams = p
        member __.getRates rnd a = getRatesImpl rnd a
        member __.getModel n = getModelImpl n
        member __.getAllRates() = p.rateModels |> List.map (fun m -> m.getAllRates())

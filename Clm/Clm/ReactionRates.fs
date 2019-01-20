namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionTypes
open ClmSys.GeneralData

module ReactionRates = 

    type RelatedReactions<'R> =
        {
            primary : (ReactionRate option * ReactionRate option)
            similar : list<'R * (ReactionRate option * ReactionRate option)>
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
            primary = (g fo rf, g bo rb)
            similar = s
        }


    let getRates (fo, rf) (bo, rb) = getRatesWithSimilar (fo, rf) (bo, rb) []
    let getForwardRates (fo, rf) = getRates (fo, rf) (None, None)


    let updatePrimaryReactions<'R> 
        (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>)
        (getEnantiomer : 'R -> 'R)
        (primary : (ReactionRate option * ReactionRate option))
        (r : 'R) =

        let enantiomer = getEnantiomer r
        if d.ContainsKey r |> not then d.Add(r, primary)
        if d.ContainsKey enantiomer |> not then d.Add(enantiomer, primary)


    let updateSimilarReactions<'R> 
        (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) 
        (getEnantiomer : 'R -> 'R)
        (similar : list<'R * (ReactionRate option * ReactionRate option)>) =

        similar |> List.map (fun (i, e) -> if d.ContainsKey i |> not then d.Add(i, e)) |> ignore
        similar |> List.map (fun (i, e) -> if d.ContainsKey (getEnantiomer i) |> not then d.Add(getEnantiomer i, e)) |> ignore


    let updateRelatedReactions<'R> 
        (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) 
        (getEnantiomer : 'R -> 'R)
        (x : RelatedReactions<'R>)
        (r : 'R) =

        updatePrimaryReactions d getEnantiomer x.primary r
        updateSimilarReactions d getEnantiomer x.similar
        x.primary


    let getRatesImpl<'R>
        (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) 
        (getEnantiomer : 'R -> 'R)
        (calculateRates : 'R -> RelatedReactions<'R>)
        (r : 'R) = 

        match d.TryGetValue r with 
        | true, rates -> rates
        | false, _ -> updateRelatedReactions d getEnantiomer (calculateRates r) r


    let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
        (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) = 
        match mo with 
        | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
        | None -> (None, None)


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
            getBaseRates : 'R -> (ReactionRate option * ReactionRate option) // Get rates of base (not catalyzed) reaction.
            eeParams : CatRatesEeParam
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
            match i.eeParams.rateMultiplierDistr.nextDoubleOpt(), i.eeParams.eeForwardDistribution with
            | Some k0, Some df ->
                let (sf0, sb0) = i.getBaseRates i.reaction
                let fEe = df.nextDouble()

                let bEe = 
                    match i.eeParams.eeBackwardDistribution with 
                    | Some d -> d.nextDouble()
                    | None -> fEe

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)
                let kb = k0 * (1.0 + bEe)
                let kbe = k0 * (1.0 - bEe)

                let (rf, rfe) = 
                    match sf0 with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) = 
                    match sb0 with
                    | Some (ReactionRate sb) -> (kb * sb |> ReactionRate |> Some, kbe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe)
            | _ -> (None, None, None, None)

        {
            primary = (rf, rb)
            similar = [ (re, (rfe, rbe)) ]
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
            getBaseRates : 'R -> (ReactionRate option * ReactionRate option) // Get rates of base (not catalyzed) reaction.
            getBaseCatRates : 'RC -> (ReactionRate option * ReactionRate option) // Get rates of underlying catalyzed reaction.
            simParams : CatRatesSimilarityParam
            eeParams : CatRatesEeParam
            rateDictionary : Dictionary<'RC, (ReactionRate option * ReactionRate option)>
        }

        member i.toCatRatesInfo r c e = 
            {
                reaction = r
                catalyst = c
                getCatEnantiomer = i.getCatEnantiomer
                catReactionCreator = i.catReactionCreator
                getBaseRates = i.getBaseRates
                eeParams = e
            }


    let calculateSimRates<'R, 'C, 'RC> (i : CatRatesSimInfo<AminoAcid, 'R, 'C, 'RC>) = 
        let r = (i.reaction, i.catalyst) |> i.catReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator
        let (bf, bb) = i.getBaseRates i.reaction
        let (f, b) = r |> i.getBaseCatRates

        let calculateCatRates s c e = 
            let reaction = (s, c) |> i.catReactionCreator
            let related = i.toCatRatesInfo s c e |> calculateCatRates
            updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer related reaction

        match (f, b) with
        | None, None -> 
            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a)
            |> List.map (fun e -> calculateCatRates e i.catalyst CatRatesEeParam.defaultValue)
            |> ignore
        | _ -> 
            let nextSeed = i.simParams.simBaseDistribution.nextSeed
            let (fe, be) = re |> i.getBaseCatRates
            let rateMult = 
                match f, fe, b, be with 
                | Some (ReactionRate a), Some (ReactionRate b), _, _ -> 
                    match bf with 
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #1..."
                | _, _, Some (ReactionRate a), Some (ReactionRate b) -> 
                    match bb with 
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #2..."
                | _ -> failwith "calculateSimRates::calculateCatRates::FUBAR #3..."

            let getEeParams d = 
                match d with 
                | true -> 
                    {
                        rateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr nextSeed None rateMult
                        eeForwardDistribution = i.simParams.getForwardEeDistr.getDistr nextSeed f fe
                        eeBackwardDistribution = i.simParams.getBackwardEeDistr.getDistr nextSeed b be
                    }
                | false -> CatRatesEeParam.defaultValue

            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a, i.simParams.simBaseDistribution.isDefined())
            |> List.map (fun (e, b) -> calculateCatRates e i.catalyst (getEeParams b))
            |> ignore

        (f, b)


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


    type FoodCreationModel (p : FoodCreationParam) = 
        let rateDictionary = new Dictionary<FoodCreationReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getRates (Some p.foodCreationRate, Some 1.0) (None, None)
        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


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
        let rateDictionary = new Dictionary<WasteRemovalReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getRates (Some p.wasteRemovalRate, Some 1.0) (None, None)
        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


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
        let rateDictionary = new Dictionary<WasteRecyclingReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getRates (Some p.wasteRecyclingRate, Some 1.0) (None, None)
        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


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
        let rateDictionary = new Dictionary<SynthesisReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.synthesisDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


    type SynthesisModel =
        | SynthRndModel of SynthesisRandomModel

        member model.getRates r =
            match model with
            | SynthRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | SynthRndModel m -> m.inputParams |> SynthRndParam

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
        let rateDictionaryImpl = new Dictionary<CatalyticSynthesisReaction, (ReactionRate option * ReactionRate option)>()

        let calculateCatSynthRates (CatalyticSynthesisReaction (s, c)) = 
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates
                eeParams = p.catSynthRndParam.catSynthRndEeParams
            }
            |> calculateCatRates

        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateCatSynthRates r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl


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
        let calculateSimRatesImpl (CatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> SynthesisReaction)
                getBaseRates = p.catSynthModel.inputParams.synthesisModel.getRates
                getBaseCatRates = p.catSynthModel.getRates
                simParams = p.catSynthSimParam
                eeParams = p.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams
                rateDictionary = p.catSynthModel.rateDictionary
            }
            |> calculateSimRates

        member __.getRates r = calculateSimRatesImpl r
        member __.inputParams = p


    type CatalyticSynthesisModel =
        | CatSynthRndModel of CatalyticSynthesisRandomModel
        | CatSynthSimModel of CatalyticSynthesisSimilarModel

        member model.getRates r = 
            match model with
            | CatSynthRndModel m -> m.getRates r
            | CatSynthSimModel m -> m.getRates r

        member model.inputParams = 
            match model with
            | CatSynthRndModel m -> m.inputParams |> CatSynthRndParamWithModel
            | CatSynthSimModel m -> m.inputParams |> CatSynthSimParamWithModel

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
        let rateDictionary = new Dictionary<DestructionReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ =
            let d = p.destructionDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


    type DestructionModel =
        | DestrRndModel of DestructionRandomModel

        member model.getRates r =
            match model with
            | DestrRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | DestrRndModel m -> m.inputParams |> DestrRndParam

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
        let rateDictionaryImpl = new Dictionary<CatalyticDestructionReaction, (ReactionRate option * ReactionRate option)>()

        let calculateCatSynthRates (CatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates
                eeParams = p.catDestrRndParam.catDestrRndEeParams
            }
            |> calculateCatRates

        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateCatSynthRates r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl


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
        let calculateSimRatesImpl (CatalyticDestructionReaction (s, c)) = 
            let (DestructionReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> DestructionReaction)
                getBaseRates = p.catDestrModel.inputParams.destructionModel.getRates
                getBaseCatRates = p.catDestrModel.getRates
                simParams = p.catDestrSimParam
                eeParams = p.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams
                rateDictionary = p.catDestrModel.rateDictionary
            }
            |> calculateSimRates

        member __.getRates r = calculateSimRatesImpl r
        member __.inputParams = p


    type CatalyticDestructionModel =
        | CatDestrRndModel of CatalyticDestructionRandomModel
        | CatDestrSimModel of CatalyticDestructionSimilarModel

        member model.getRates r =
            match model with
            | CatDestrRndModel m -> m.getRates r
            | CatDestrSimModel m -> m.getRates r

        member model.inputParams = 
            match model with
            | CatDestrRndModel m -> m.inputParams |> CatDestrRndParamWithModel
            | CatDestrSimModel m -> m.inputParams |> CatDestrSimParamWithModel

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
        let rateDictionaryImpl = new Dictionary<SedimentationDirectReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationDirectDistribution.nextDoubleOpt())
        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateRates r
        member __.inputParams = p


    type SedimentationDirectModel =
        | SedDirRndModel of SedimentationDirectRandomModel

        member model.getRates r =
            match model with
            | SedDirRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | SedDirRndModel m -> m.inputParams |> SedDirRndParam

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
        let rateDictionaryImpl = new Dictionary<SedimentationAllReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble() |> Some)
        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateRates r
        member __.inputParams = p


    type SedimentationAllModel =
        | SedAllRndModel of SedimentationAllRandomModel

        member model.getRates r =
            match model with
            | SedAllRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | SedAllRndModel m -> m.inputParams |> SedAllRndParam

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
        let rateDictionaryImpl = new Dictionary<LigationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ =
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateRates r
        member __.inputParams = p


    type LigationModel =
        | LigRndModel of LigationRandomModel

        member model.getRates r =
            match model with
            | LigRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | LigRndModel m -> m.inputParams |> LigRndParam

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
        let rateDictionaryImpl = new Dictionary<CatalyticLigationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateCatSynthRates (CatalyticLigationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates
                eeParams = p.catLigationParam.catLigRndEeParams
            }
            |> calculateCatRates

        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateCatSynthRates r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl


    type CatalyticLigationModel =
        | CatLigRndModel of CatalyticLigationRandomModel

        member model.getRates r =
            match model with
            | CatLigRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | CatLigRndModel m -> m.inputParams |> CatLigRndParamWithModel

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
        let rateDictionary = new Dictionary<RacemizationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.racemizationDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (None, None)

        member __.getRates r = getRatesImpl rateDictionary getEnantiomer calculateRates r
        member __.inputParams = p


    type RacemizationModel =
        | RacemRndModel of RacemizationRandomModel

        member model.getRates r =
            match model with
            | RacemRndModel m -> m.getRates r

        member model.inputParams =
            match model with
            | RacemRndModel m -> m.inputParams |> RacemRndParam

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
        let rateDictionaryImpl = new Dictionary<CatalyticRacemizationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateCatSynthRates (CatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates
                eeParams = p.catRacemRndParam.catRacemRndEeParams
            }
            |> calculateCatRates

        member __.getRates r = getRatesImpl rateDictionaryImpl getEnantiomer calculateCatSynthRates r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl


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
        let calculateSimRatesImpl (CatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> RacemizationReaction)
                getBaseRates = p.catRacemModel.inputParams.racemizationModel.getRates
                getBaseCatRates = p.catRacemModel.getRates
                simParams = p.catRacemSimParam
                eeParams = p.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams
                rateDictionary = p.catRacemModel.rateDictionary
            }
            |> calculateSimRates

        member __.getRates r = calculateSimRatesImpl r
        member __.inputParams = p


    type CatalyticRacemizationModel =
        | CatRacemRndModel of CatalyticRacemizationRandomModel
        | CatRacemSimModel of CatalyticRacemizationSimilarModel

        member model.getRates r =
            match model with
            | CatRacemRndModel m -> m.getRates r
            | CatRacemSimModel m -> m.getRates r

        member model.inputParams =
            match model with
            | CatRacemRndModel m -> m.inputParams |> CatRacemRndParamWithModel
            | CatRacemSimModel m -> m.inputParams |> CatRacemSimParamWithModel

        static member create p =
            match p with 
            | CatRacemRndParamWithModel q -> CatalyticRacemizationRandomModel q |> CatRacemRndModel
            | CatRacemSimParamWithModel q -> CatalyticRacemizationSimilarModel q |> CatRacemSimModel


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

        member p.tryFindSFoodCreationModel() = p.rateModels |> List.tryPick (fun e -> match e with | FoodCreationRateModel m -> Some m | _ -> None)
        member p.tryFindSWasteRemovalModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRemovalRateModel m -> Some m | _ -> None)
        member p.tryFindSWasteRecyclingModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRecyclingRateModel m -> Some m | _ -> None)
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

    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl (a : Reaction) =
            match a with 
            | FoodCreation r -> getModelRates (p.tryFindSFoodCreationModel()) r
            | WasteRemoval r -> getModelRates (p.tryFindSWasteRemovalModel()) r
            | WasteRecycling r -> getModelRates (p.tryFindSWasteRecyclingModel()) r
            | Synthesis r -> getModelRates (p.tryFindSynthesisModel()) r
            | Destruction r -> getModelRates (p.tryFindDestructionModel()) r
            | CatalyticSynthesis r -> getModelRates (p.tryFindCatalyticSynthesisModel()) r
            | CatalyticDestruction r -> getModelRates (p.tryFindCatalyticDestructionModel()) r
            | Ligation r -> getModelRates (p.tryFindLigationModel()) r
            | CatalyticLigation r ->  getModelRates (p.tryFindCatalyticLigationModel()) r
            | SedimentationDirect r -> getModelRates (p.tryFindSedimentationDirectModel()) r
            | SedimentationAll r -> getModelRates (p.tryFindSedimentationAllModel()) r
            | Racemization r -> getModelRates (p.tryFindRacemizationModel()) r
            | CatalyticRacemization r -> getModelRates (p.tryFindCatalyticRacemizationModel()) r

        member __.providerParams = p
        member __.getRates (a : Reaction) = getRatesImpl a

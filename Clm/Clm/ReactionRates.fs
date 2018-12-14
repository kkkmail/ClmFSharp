namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionTypes

module ReactionRates = 

    type ReactionRate = 
        | ReactionRate of double


    type RelatedReactions<'T> = 
        {
            primary : (ReactionRate option * ReactionRate option)
            similar : list<'T * (ReactionRate option * ReactionRate option)>
        }


    let noRates = 
        {
            primary = (None, None)
            similar = []
        }


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


    let inline updatePrimaryReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (primary : (ReactionRate option * ReactionRate option))
        (r : 'T) =

        let enantiomer = getEnantiomer r
        //printfn "updatePrimaryReactions: r = %A, enantiomer = %A" r enantiomer

        if d.ContainsKey r |> not 
            then 
                //printfn "    updatePrimaryReactions: r = %A, primary = %A" r primary
                d.Add(r, primary)

        if d.ContainsKey enantiomer |> not 
            then 
                //printfn "    updatePrimaryReactions: enantiomer = %A, primary = %A" enantiomer primary
                d.Add(enantiomer, primary)


    let inline updateSimilarReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (similar : list<'T * (ReactionRate option * ReactionRate option)>) =

        //printfn "updateSimilarReactions: similar = %A" similar

        similar |> List.map (fun (i, e) -> 
                    //printfn "    updateSimilarReactions: i = %A" i
                    if d.ContainsKey i |> not 
                    then 
                        //printfn "        updateSimilarReactions: Adding: i = %A, e = %A" i e
                        d.Add(i, e)
                        ) |> ignore

        similar |> List.map (fun (i, e) -> 
                    //printfn "    updateSimilarReactions: (getEnantiomer i) = %A" (getEnantiomer i)
                    if d.ContainsKey (getEnantiomer i) |> not 
                    then 
                        //printfn "        updateSimilarReactions: Adding: (getEnantiomer i) = %A, e = %A" (getEnantiomer i) e
                        d.Add(getEnantiomer i, e)
                        ) |> ignore


    let inline updateRelatedReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (x : RelatedReactions<'T>)
        (r : 'T) =

        //printfn "updateRelatedReactions: r = %A, x = %A" r x

        updatePrimaryReactions d x.primary r
        updateSimilarReactions d x.similar
        x.primary


    let inline getRatesImpl<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (calculateRates : 'T -> RelatedReactions<'T>)
        (r : 'T) = 

        match d.TryGetValue r with 
        | true, rates -> rates
        | false, _ -> updateRelatedReactions d (calculateRates r) r


    let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
        (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) = 
        match mo with 
        | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
        | None -> (None, None)


    type CatRatesInfo<'R, 'C, 'RC when 'C : (member enantiomer : 'C) and 'C : equality> = 
        {
            reaction : 'R
            catalyst : 'C
            distribution : Distribution
            catReactionCreator : ('R * 'C) -> 'RC
            rateCoeff : double option
            getRates : 'R -> (ReactionRate option * ReactionRate option)
            maxEe : double
            multiplier : double
        }


    let inline calculateCatRates<'R, 'C, 'RC when 'C : (member enantiomer : 'C) and 'C : equality> (i : CatRatesInfo<'R, 'C, 'RC>) = 
        let re = (i.reaction, getEnantiomer i.catalyst) |> i.catReactionCreator

        let rf, rb, rfe, rbe = 
            match i.rateCoeff with 
            | Some k0 ->
                let (sf0, sb0) = i.getRates i.reaction
                let ee = i.maxEe * (i.distribution.nextDoubleFromZeroToOne() - 0.5)
                let k = k0 * i.multiplier * (1.0 + ee)
                let ke = k0 * i.multiplier * (1.0 - ee)

                let (rf, rfe) = 
                    match sf0 with
                    | Some (ReactionRate sf) -> (k * sf |> ReactionRate |> Some, ke * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) = 
                    match sb0 with
                    | Some (ReactionRate sb) -> (k * sb |> ReactionRate |> Some, ke * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe)
            | None -> (None, None, None, None)

        {
            primary = (rf, rb)
            similar = [ (re, (rfe, rbe)) ]
        }


    type SynthesisRandomParam = 
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SynthesisParam = 
        | SynthRndParam of SynthesisRandomParam


    type SynthesisRandomModel (p : SynthesisRandomParam) =
        let rateDictionary = new Dictionary<SynthesisReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.synthesisDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionary calculateRates r
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


    type CatalyticSynthesisRandomParam = 
        {
            catSynthDistribution : Distribution
            multiplier : double
            maxEe : double
        }


    type CatalyticSynthesisSimilarParam =
        {
            simSynthDistribution : Distribution
            aminoAcids : list<AminoAcid>
        }


    type CatalyticSynthesisParam = 
        | CatSynthRndParam of CatalyticSynthesisRandomParam
        | CatSynthSimParam of CatalyticSynthesisSimilarParam


    type CatalyticSynthesisRandomParamWithModel = 
        {
            catSynthRndParam : CatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type CatalyticSynthesisRandomModel (p : CatalyticSynthesisRandomParamWithModel) = 
        let rateDictionaryImpl = new Dictionary<CatalyticSynthesisReaction, (ReactionRate option * ReactionRate option)>()
        let distr = p.catSynthRndParam.catSynthDistribution

        let calculateCatSynthRates s (c : SynthCatalyst) k = 
            {
                reaction = s
                catalyst = c
                distribution = distr
                catReactionCreator = CatalyticSynthesisReaction
                rateCoeff = k
                getRates = p.synthesisModel.getRates
                maxEe = p.catSynthRndParam.maxEe
                multiplier = p.catSynthRndParam.multiplier
            }
            |> calculateCatRates

        let calculateOptionalRatesImpl (CatalyticSynthesisReaction (s, c)) = calculateCatSynthRates s c (distr.nextDoubleOpt())
        let calculatelRatesImpl (CatalyticSynthesisReaction (s, c)) = calculateCatSynthRates s c (distr.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionaryImpl calculateOptionalRatesImpl r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl
        member __.calculatelRates r = getRatesImpl rateDictionaryImpl calculatelRatesImpl r


    type CatalyticSynthesisSimilarParamWithModel = 
        {
            catSynthSimParam : CatalyticSynthesisSimilarParam
            catSynthModel : CatalyticSynthesisRandomModel
        }


    type CatalyticSynthesisParamWithModel = 
        | CatSynthRndParamWithModel of CatalyticSynthesisRandomParamWithModel
        | CatSynthSimParamWithModel of CatalyticSynthesisSimilarParamWithModel


    type CatalyticSynthesisSimilarModel (p : CatalyticSynthesisSimilarParamWithModel) = 
        let catSynthRndModel = p.catSynthModel

        let calculateSimilarRates (CatalyticSynthesisReaction ((SynthesisReaction (h, a)), c)) = 
            p.catSynthSimParam.aminoAcids
            |> List.map (fun e -> CatalyticSynthesisReaction ((h, a.createSameChirality e) |> SynthesisReaction, c))
            |> List.filter (fun _ -> p.catSynthSimParam.simSynthDistribution.isDefined())
            |> List.map (fun r -> catSynthRndModel.calculatelRates r)

        let calculateRatesImpl r = 
            let rates = catSynthRndModel.getRates r

            match rates with
            | None, None -> ignore()
            | _ -> calculateSimilarRates r |> ignore

            rates

        member __.getRates r = calculateRatesImpl r
        member __.inputParams = p
        member __.rateDictionary = catSynthRndModel.rateDictionary


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

        member model.rateDictionary = 
            match model with
            | CatSynthRndModel m -> m.rateDictionary
            | CatSynthSimModel m -> m.rateDictionary


    type SedimentationDirectRandomParam = 
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationDirectParam = 
        | SedDirRndParam of SedimentationDirectRandomParam


    type SedimentationDirectRandomModel (p : SedimentationDirectRandomParam) =
        let rateDictionary = new Dictionary<SedimentationDirectReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationDirectDistribution.nextDoubleOpt())
        member __.getRates r = getRatesImpl rateDictionary calculateRates r
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


    type SedimentationAllRandomParam = 
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationAllParam = 
        | SedAllRndParam of SedimentationAllRandomParam


    type SedimentationRandomAllModel (p : SedimentationAllRandomParam) =
        let rateDictionary = new Dictionary<SedimentationAllReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble() |> Some)
        member __.getRates r = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type SedimentationAllModel = 
        | SedAllRndModel of SedimentationRandomAllModel

        member model.getRates r = 
            match model with
            | SedAllRndModel m -> m.getRates r

        member model.inputParams = 
            match model with
            | SedAllRndModel m -> m.inputParams |> SedAllRndParam

        static member create p = 
            match p with 
            | SedAllRndParam q -> SedimentationRandomAllModel q |> SedAllRndModel


    type LigationRandomParam = 
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type LigationParam = 
        | LigRndParam of LigationRandomParam


    type LigationRandomModel (p : LigationRandomParam) = 
        let rateDictionary = new Dictionary<LigationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionary calculateRates r
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


    type CatalyticLigationRandomParam = 
        {
            catLigationDistribution : Distribution
            multiplier : double
            maxEe : double
        }


    type CatalyticLigationParam = 
        | CatLigRndParam of CatalyticLigationRandomParam

        member p.catLigationDistribution = 
            match p with 
            | CatLigRndParam q -> q.catLigationDistribution

        member p.maxEe = 
            match p with 
            | CatLigRndParam q -> q.maxEe

        member p.multiplier = 
            match p with 
            | CatLigRndParam q -> q.multiplier


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
        let rateDictionary = new Dictionary<CatalyticLigationReaction, (ReactionRate option * ReactionRate option)>()
        let distr = p.catLigationParam.catLigationDistribution

        let calculateCatLigRates s (c : LigCatalyst) k = 
            {
                reaction = s
                catalyst = c
                distribution = distr
                catReactionCreator = CatalyticLigationReaction
                rateCoeff = k
                getRates = p.ligationModel.getRates
                maxEe = p.catLigationParam.maxEe
                multiplier = p.catLigationParam.multiplier
            }
            |> calculateCatRates

        let calculateOptionalRatesImpl (CatalyticLigationReaction (s, c)) = calculateCatLigRates s c (distr.nextDoubleOpt())

        member __.getRates r = getRatesImpl rateDictionary calculateOptionalRatesImpl r
        member __.inputParams = p


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


    type RacemizationRandomParam = 
        {
            racemizationDistribution : Distribution
            forwardScale : double option
        }


    type RacemizationParam = 
        | RacemRndParam of RacemizationRandomParam


    type RacemizationRandomModel (p : RacemizationRandomParam) =
        let rateDictionary = new Dictionary<RacemizationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.racemizationDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (None, None)

        member __.getRates r = getRatesImpl rateDictionary calculateRates r
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


    type CatalyticRacemizationRandomParam = 
        {
            catRacemDistribution : Distribution
            multiplier : double
            maxEe : double
        }


    type CatalyticRacemizationSimilarParam =
        {
            simRacemDistribution : Distribution
            aminoAcids : list<AminoAcid>
        }


    type CatalyticRacemizationParam = 
        | CatRacemRndParam of CatalyticRacemizationRandomParam
        | CatRacemSimParam of CatalyticRacemizationSimilarParam


    type CatalyticRacemizationRandomParamWithModel = 
        {
            catRacemRndParam : CatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
        }


    type CatalyticRacemizationRandomModel (p : CatalyticRacemizationRandomParamWithModel) = 
        let rateDictionaryImpl = new Dictionary<CatalyticRacemizationReaction, (ReactionRate option * ReactionRate option)>()
        let distr = p.catRacemRndParam.catRacemDistribution

        let calculateCatRacemRates s (c : RacemizationCatalyst) k = 
            {
                reaction = s
                catalyst = c
                distribution = distr
                catReactionCreator = CatalyticRacemizationReaction
                rateCoeff = k
                getRates = p.racemizationModel.getRates
                maxEe = p.catRacemRndParam.maxEe
                multiplier = p.catRacemRndParam.multiplier
            }
            |> calculateCatRates

        let calculateOptionalRatesImpl (CatalyticRacemizationReaction (s, c)) = calculateCatRacemRates s c (distr.nextDoubleOpt())
        let calculatelRatesImpl (CatalyticRacemizationReaction (s, c)) = calculateCatRacemRates s c (distr.nextDouble() |> Some)

        member __.getRates r = getRatesImpl rateDictionaryImpl calculateOptionalRatesImpl r
        member __.inputParams = p
        member __.rateDictionary = rateDictionaryImpl
        member __.calculatelRates r = getRatesImpl rateDictionaryImpl calculatelRatesImpl r


    type CatalyticRacemizationSimilarParamWithModel = 
        {
            catRacemSimParam : CatalyticRacemizationSimilarParam
            catRacemModel : CatalyticRacemizationRandomModel
        }


    type CatalyticRacemizationParamWithModel = 
        | CatRacemRndParamWithModel of CatalyticRacemizationRandomParamWithModel
        | CatRacemSimParamWithModel of CatalyticRacemizationSimilarParamWithModel


    type CatalyticRacemizationSimilarModel (p : CatalyticRacemizationSimilarParamWithModel) = 
        let catRacemRndModel = p.catRacemModel

        let calculateSimilarRates (CatalyticRacemizationReaction ((RacemizationReaction a), c)) = 
            p.catRacemSimParam.aminoAcids
            |> List.map (fun e -> CatalyticRacemizationReaction (a.createSameChirality e |> RacemizationReaction, c))
            |> List.filter (fun _ -> p.catRacemSimParam.simRacemDistribution.isDefined())
            |> List.map (fun r -> catRacemRndModel.calculatelRates r)

        let calculateRatesImpl r = 
            let rates = catRacemRndModel.getRates r

            match rates with
            | None, None -> ignore()
            | _ -> calculateSimilarRates r |> ignore

            rates

        member __.getRates r = calculateRatesImpl r
        member __.inputParams = p
        member __.rateDictionary = catRacemRndModel.rateDictionary


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

        member model.rateDictionary = 
            match model with
            | CatRacemRndModel m -> m.rateDictionary
            | CatRacemSimModel m -> m.rateDictionary


    type ReactionRateModelParam = 
        | SynthesisRateParam of SynthesisParam
        | CatalyticSynthesisRateParam of CatalyticSynthesisParam
        | LigationRateParam of LigationParam
        | CatalyticLigationRateParam of CatalyticLigationParam
        | SedimentationDirectRateParam of SedimentationDirectParam
        | SedimentationAllRateParam of SedimentationAllParam
        | RacemizationRateParam of RacemizationParam
        | CatalyticRacemizationRateParam of CatalyticRacemizationParam


    type ReactionRateModel = 
        | SynthesisRateModel of SynthesisModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel
        | RacemizationRateModel of RacemizationModel
        | CatalyticRacemizationRateModel of CatalyticRacemizationModel

        member rm.inputParams = 
            match rm with
            | SynthesisRateModel m -> m.inputParams |> SynthesisRateParam
            | CatalyticSynthesisRateModel v ->
                match v with 
                | CatSynthRndModel m -> m.inputParams.catSynthRndParam |> CatSynthRndParam |> CatalyticSynthesisRateParam
                | CatSynthSimModel m -> m.inputParams.catSynthSimParam |> CatSynthSimParam |> CatalyticSynthesisRateParam
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


    type ReactionRateProviderParams = 
        {
            rateModels: list<ReactionRateModel>
        }

        member p.tryFindSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | SynthesisRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticSynthesisRateModel m -> Some m | _ -> None)
        member p.tryFindLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | LigationRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticLigationRateModel m -> Some m | _ -> None)
        member p.tryFindSedimentationDirectModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationDirectRateModel m -> Some m | _ -> None)
        member p.tryFindSedimentationAllModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationAllRateModel m -> Some m | _ -> None)
        member p.tryFindRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | RacemizationRateModel m -> Some m | _ -> None)
        member p.tryFindCatalyticRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticRacemizationRateModel m -> Some m | _ -> None)


    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl (a : Reaction) = 
            match a with 
            | Synthesis r -> getModelRates (p.tryFindSynthesisModel()) r
            | CatalyticSynthesis r -> getModelRates (p.tryFindCatalyticSynthesisModel()) r
            | Ligation r -> getModelRates (p.tryFindLigationModel()) r
            | CatalyticLigation r ->  getModelRates (p.tryFindCatalyticLigationModel()) r
            | SedimentationDirect r -> getModelRates (p.tryFindSedimentationDirectModel()) r
            | SedimentationAll r -> getModelRates (p.tryFindSedimentationAllModel()) r
            | Racemization r -> getModelRates (p.tryFindRacemizationModel()) r
            | CatalyticRacemization r -> getModelRates (p.tryFindCatalyticRacemizationModel()) r

        member __.providerParams = p
        member __.getRates (a : Reaction) = getRatesImpl a

        static member defaultSynthRndModel (rnd : Random) (forward, backward) =
            {
                synthesisDistribution = DeltaDistribution(rnd.Next(), { threshold = None }) |> Delta
                //synthesisDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SynthRndParam
            |> SynthesisModel.create

        static member defaultCatSynthRndParams (rnd : Random) (m, threshold, mult) =
            {
                catSynthRndParam = 
                    {
                        catSynthDistribution = TriangularDistribution(rnd.Next(), { threshold = threshold }) |> Triangular
                        multiplier  = mult
                        maxEe = 0.05
                    }
                synthesisModel = m
            }

        static member defaultCatSynthRndModel (rnd : Random) (m, threshold, mult) = 
            ReactionRateProvider.defaultCatSynthRndParams rnd (m, threshold, mult)
            |> CatSynthRndParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultCatSynthSimModel (rnd : Random) (m, threshold, mult) (simThreshold, n) =
            let aminoAcids = AminoAcid.getAminoAcids n

            {
                catSynthSimParam = 
                    {
                        simSynthDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold }) |> Uniform
                        aminoAcids = aminoAcids
                    }
                catSynthModel = ReactionRateProvider.defaultCatSynthRndParams rnd (m, threshold, mult) |> CatalyticSynthesisRandomModel
            }
            |> CatSynthSimParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultLigRndModel (rnd : Random) (forward, backward) =
            {
                ligationDistribution = DeltaDistribution(rnd.Next(), { threshold = None }) |> Delta
                //ligationDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigRndParam
            |> LigationModel.create

        static member defaultCatLigRndModel (rnd : Random) (m, threshold, mult) =
            {
                catLigationParam = 
                    {
                        catLigationDistribution = TriangularDistribution(rnd.Next(), { threshold = threshold }) |> Triangular
                        multiplier  = mult
                        maxEe = 0.05
                    }
                ligationModel = m
            }
            |> CatLigRndParamWithModel
            |> CatalyticLigationModel.create

        static member defaultSedDirRndModel (rnd : Random) (threshold, mult) =
            {
                sedimentationDirectDistribution = TriangularDistribution(rnd.Next(), { threshold = Some threshold }) |> Triangular
                forwardScale = Some mult
            }
            |> SedDirRndParam
            |> SedimentationDirectModel.create

        static member defaultSedAllRndModel (rnd : Random) mult =
            {
                sedimentationAllDistribution = TriangularDistribution(rnd.Next(), { threshold = None }) |> Triangular
                forwardScale = Some mult
            }
            |> SedAllRndParam
            |> SedimentationAllModel.create

        static member defaultRacemRndModel (rnd : Random) forward =
            {
                racemizationDistribution = DeltaDistribution(rnd.Next(), { threshold = None }) |> Delta
                //racemizationDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
            }
            |> RacemRndParam
            |> RacemizationModel.create

        static member defaultCatRacemRndParams (rnd : Random) (m, threshold, mult) =
            {
                catRacemRndParam = 
                    {
                        catRacemDistribution = TriangularDistribution(rnd.Next(), { threshold = threshold }) |> Triangular
                        multiplier  = mult
                        maxEe = 0.05
                    }
                racemizationModel = m
            }

        static member defaultCatRacemRndModel (rnd : Random) (m, threshold, mult) = 
            ReactionRateProvider.defaultCatRacemRndParams rnd (m, threshold, mult)
            |> CatRacemRndParamWithModel
            |> CatalyticRacemizationModel.create

        static member defaultCatRacemSimModel (rnd : Random) (m, threshold, mult) (simThreshold, n) =
            let aminoAcids = AminoAcid.getAminoAcids n

            {
                catRacemSimParam = 
                    {
                        simRacemDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold }) |> Uniform
                        aminoAcids = aminoAcids
                    }
                catRacemModel = ReactionRateProvider.defaultCatRacemRndParams rnd (m, threshold, mult) |> CatalyticRacemizationRandomModel
            }
            |> CatRacemSimParamWithModel
            |> CatalyticRacemizationModel.create

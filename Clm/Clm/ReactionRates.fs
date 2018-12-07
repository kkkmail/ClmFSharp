namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections

open Clm.Distributions
open Clm.ReactionTypes

module ReactionRates = 
    open Substances

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
    let inline getEnantiomer i = ((^T) : (member enantiomer : 'T) (i))

    let inline updatePrimaryReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (primary : (ReactionRate option * ReactionRate option))
        (r : 'T) =

        let enantiomer = getEnantiomer r
        if d.ContainsKey r |> not then d.Add(r, primary)
        if d.ContainsKey enantiomer |> not then d.Add(enantiomer, primary)


    let inline updateSimilarReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (similar : list<'T * (ReactionRate option * ReactionRate option)>) =

        similar |> List.map (fun (i, e) -> if d.ContainsKey i |> not then d.Add(i, e)) |> ignore
        similar |> List.map (fun (i, e) -> if d.ContainsKey (getEnantiomer i) |> not then d.Add(getEnantiomer i, e)) |> ignore


    let inline updateRelatedReactions<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (x : RelatedReactions<'T>)
        (r : 'T) =

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
            similarityDistribution : Distribution
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

        let calculateCatSynthRates s (c : SynthCatalyst) k0 = 
            let (sf0, sb0) = p.synthesisModel.getRates s
            let ee = p.catSynthRndParam.maxEe * (distr.nextDoubleFromZeroToOne() - 0.5)
            let k = k0 * p.catSynthRndParam.multiplier * (1.0 + ee)
            let ke = k0 * p.catSynthRndParam.multiplier * (1.0 - ee)

            let (rf, rfe) = 
                match sf0 with
                | Some (ReactionRate sf) -> (k * sf |> ReactionRate |> Some, ke * sf |> ReactionRate |> Some)
                | None -> (None, None)

            let (rb, rbe) = 
                match sb0 with
                | Some (ReactionRate sb) -> (k * sb |> ReactionRate |> Some, ke * sb |> ReactionRate |> Some)
                | None -> (None, None)

            let re = (s, c.enantiomer) |> CatalyticSynthesisReaction

            {
                primary = (rf, rb)
                similar = [ (re, (rfe, rbe)) ]
            }


        let calculateOptionalRatesImpl (CatalyticSynthesisReaction (s, c)) = 
            match distr.nextDoubleOpt() with 
            | Some k0 -> calculateCatSynthRates s c k0
            | None -> noRates

        let calculatelRatesImpl (CatalyticSynthesisReaction (s, c)) = calculateCatSynthRates s c (distr.nextDouble())

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

        let calculateSimilarRates (CatalyticSynthesisReaction ((SynthesisReaction a), c)) = 
            p.catSynthSimParam.aminoAcids
            |> List.map (fun e -> CatalyticSynthesisReaction (a.createSameChirality e |> SynthesisReaction, c))
            |> List.filter (fun _ -> p.catSynthSimParam.similarityDistribution.isDefined())
            |> List.map (fun r -> catSynthRndModel.calculatelRates r)

        let calculateRatesImpl r = 
            let rates = catSynthRndModel.getRates r

            match rates with
            | None, None -> ignore()
            | _ -> calculateSimilarRates r |> ignore

            rates

        member __.getRates r = calculateRatesImpl r
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

        let calculateRates (CatalyticLigationReaction (s, c)) = 
            let distr = p.catLigationParam.catLigationDistribution
            match distr.nextDoubleOpt() with 
            | Some k0 -> 
                let (sf0, sb0) = p.ligationModel.getRates s
                let ee = p.catLigationParam.maxEe * (distr.nextDoubleFromZeroToOne() - 0.5)
                let k = k0 * p.catLigationParam.multiplier * (1.0 + ee)
                let ke = k0 * p.catLigationParam.multiplier * (1.0 - ee)

                let (rf, rfe) = 
                    match sf0 with
                    | Some (ReactionRate sf) -> (k * sf |> ReactionRate |> Some, ke * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) = 
                    match sb0 with
                    | Some (ReactionRate sb) -> (k * sb |> ReactionRate |> Some, ke * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                let re = (s, c.enantiomer) |> CatalyticLigationReaction

                {
                    primary = (rf, rb)
                    similar = [ (re, (rfe, rbe)) ]
                }
            | None -> noRates

        member __.getRates r = getRatesImpl rateDictionary calculateRates r
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


    type ReactionRateModelParam = 
        | SynthesisRateParam of SynthesisParam
        | CatalyticSynthesisRateParam of CatalyticSynthesisParam
        | LigationRateParam of LigationParam
        | CatalyticLigationRateParam of CatalyticLigationParam
        | SedimentationDirectRateParam of SedimentationDirectParam
        | SedimentationAllRateParam of SedimentationAllParam


    type ReactionRateModel = 
        | SynthesisRateModel of SynthesisModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel

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


    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl (a : Reaction) = 
            match a with 
            | Synthesis r -> getModelRates (p.tryFindSynthesisModel()) r
            | CatalyticSynthesis r -> getModelRates (p.tryFindCatalyticSynthesisModel()) r
            | Ligation r -> getModelRates (p.tryFindLigationModel()) r
            | CatalyticLigation r ->  getModelRates (p.tryFindCatalyticLigationModel()) r
            | SedimentationDirect r -> getModelRates (p.tryFindSedimentationDirectModel()) r
            | SedimentationAll r -> getModelRates (p.tryFindSedimentationAllModel()) r

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

        static member defaultCatSynthSimModel (rnd : Random) (m, threshold, mult) (simThreshold, aa) =
            {
                catSynthSimParam = 
                    {
                        similarityDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold }) |> Uniform
                        aminoAcids = aa
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

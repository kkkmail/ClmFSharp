namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open Clm.ReactionTypes

module ReactionRates = 

    [<Literal>]
    let Nl = "\r\n"


    let doubleFSharpString (d : double) = 
        let s = d.ToString()
        match s.Contains(".") with
        | true -> s
        | false -> s + ".0"


    let toFloat (s : string) = 
        match s.Contains(".") with 
        | true -> s
        | false -> 
            match s.ToUpper().Contains("E+") || s.ToUpper().Contains("E-") with
            | true -> s
            | false -> s + ".0"


    let doubleOptFSharpString (d : double option) = 
        match d with 
        | Some v -> "Some " + (v.ToString() |> toFloat)
        | None -> "None"


    type ReactionRate = 
        | ReactionRate of double


    type DistributionParams = 
        {
            threshold : double option
        }

        member this.toFSharpCode = "{ threshold = " + (doubleOptFSharpString this.threshold) + " }"

        static member defaultValue = 
            {
                threshold = None
            }


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())
        let isDefined() = 
            match p.threshold with
            | Some t -> if rndBool.NextDouble() < t then true else false
            | None -> true

        let nextDoubleImpl() = d(rnd)
        let nextDoubleFromZeroToOneImpl() = rnd.NextDouble()

        member __.seedValue = seed
        member __.distributionParams = p
        member __.nextDouble = nextDoubleImpl
        member __.nextDoubleFromZeroToOne = nextDoubleFromZeroToOneImpl

        member __.nextDoubleOpt() = 
            match isDefined() with 
            | true -> nextDoubleImpl() |> Some
            | false -> None


    type DeltaDistribution (p : DistributionParams) = 
        inherit DistributionBase (0, p, fun _ -> 1.0)

        member distr.toFSharpCode = "DeltaDistribution(" + p.toFSharpCode + ")"


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())

        member distr.toFSharpCode = "UniformDistribution(" + seed.ToString() + ", " + p.toFSharpCode + ")"
        new (seed : int) = UniformDistribution(seed, DistributionParams.defaultValue)


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))

        member distr.toFSharpCode = "TriangularDistribution(" + seed.ToString() + ", " + p.toFSharpCode + ")"


    type Distribution =
        | Delta of DeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt

        member this.nextDoubleFromZeroToOne =
            match this with
            | Delta d -> d.nextDoubleFromZeroToOne
            | Uniform d -> d.nextDoubleFromZeroToOne
            | Triangular d -> d.nextDoubleFromZeroToOne

        member this.toFSharpCode =
            match this with
            | Delta d -> d.toFSharpCode + " |> Delta"
            | Uniform d -> d.toFSharpCode + " |> Uniform"
            | Triangular d -> d.toFSharpCode + " |> Triangular"


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


    let inline getRatesImpl<'T when 'T : (member enantiomer : 'T) and 'T : equality> 
        (d : Dictionary<'T, (ReactionRate option * ReactionRate option)>) 
        (calculateRates : 'T -> RelatedReactions<'T>)
        (r : 'T) = 

        match d.TryGetValue r with 
        | true, rates -> rates
        | false, _ -> 
            let x = calculateRates r
            let getEnantiomer i = ((^T) : (member enantiomer : 'T) (i))
            let enantiomer = getEnantiomer r
            d.Add(r, x.primary)
            if d.ContainsKey enantiomer |> not then d.Add(enantiomer, x.primary)
            x.similar |> List.map (fun (i, e) -> if d.ContainsKey i |> not then d.Add(i, e)) |> ignore
            x.similar |> List.map (fun (i, e) -> if d.ContainsKey (getEnantiomer i) |> not then d.Add(getEnantiomer i, e)) |> ignore
            x.primary


    type SyntethisParam = 
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                synthesisDistribution = " + p.synthesisDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type SyntethisModel (p : SyntethisParam) =
        let rateDictionary = new Dictionary<SynthesisReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.synthesisDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates (r : SynthesisReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p

    type CatalyticSynthesisParam = 
        {
            catSynthDistribution : Distribution
            multiplier : double
            maxEe : double
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catSynthDistribution = " + p.catSynthDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type CatalyticSynthesisParamWithModel = 
        {
            catSynthParam : CatalyticSynthesisParam
            synthesisModel : SyntethisModel
        }


    type CatalyticSynthesisModel (p : CatalyticSynthesisParamWithModel) = 
        let rateDictionary = new Dictionary<CatalyticSynthesisReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates (CatalyticSynthesisReaction (s, c)) = 
            let distr = p.catSynthParam.catSynthDistribution
            match distr.nextDoubleOpt() with 
            | Some k0 -> 
                let (sf0, sb0) = p.synthesisModel.getRates s
                let ee = p.catSynthParam.maxEe * (distr.nextDoubleFromZeroToOne() - 0.5)
                let k = k0 * p.catSynthParam.multiplier * (1.0 + ee)
                let ke = k0 * p.catSynthParam.multiplier * (1.0 - ee)

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
            | None -> noRates

        member __.getRates (r : CatalyticSynthesisReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type SedimentationDirectParam = 
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationDirectDistribution = " + p.sedimentationDirectDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type SedimentationDirectModel (p : SedimentationDirectParam) =
        let rateDictionary = new Dictionary<SedimentationDirectReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationDirectDistribution.nextDoubleOpt())
        member __.getRates (r : SedimentationDirectReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type SedimentationAllParam = 
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationAllDistribution = " + p.sedimentationAllDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type SedimentationAllModel (p : SedimentationAllParam) =
        let rateDictionary = new Dictionary<SedimentationAllReaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble() |> Some)
        member __.getRates (r : SedimentationAllReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type LigationParam = 
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                ligationDistribution = " + p.ligationDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type LigationModel (p : LigationParam) = 
        let rateDictionary = new Dictionary<LigationReaction, (ReactionRate option * ReactionRate option)>()

        let calculateRates _ = 
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)

        member __.getRates (r : LigationReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type CatalyticLigationParam = 
        {
            catLigationDistribution : Distribution
            multiplier : double
            maxEe : double
        }

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catLigationDistribution = " + p.catLigationDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type CatalyticLigationParamWithModel = 
        {
            catLigationParam : CatalyticLigationParam
            ligationModel : LigationModel
        }


    type CatalyticLigationModel (p : CatalyticLigationParamWithModel) = 
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

        member __.getRates (r : CatalyticLigationReaction) = getRatesImpl rateDictionary calculateRates r
        member __.inputParams = p


    type ReactionRateModelParam = 
        | SynthesisRateParam of SyntethisParam
        | CatalyticSynthesisRateParam of CatalyticSynthesisParam
        | LigationRateParam of LigationParam
        | CatalyticLigationRateParam of CatalyticLigationParam
        | SedimentationDirectRateParam of SedimentationDirectParam
        | SedimentationAllRateParam of SedimentationAllParam

        member rm.toFSharpCode (shift : string) = 
            match rm with 
            | SynthesisRateParam m -> (m.toFSharpCode shift) + shift + "            |> SynthesisRateParam" + Nl
            | CatalyticSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "            |> CatalyticSynthesisRateParam" + Nl
            | LigationRateParam m -> (m.toFSharpCode shift) + shift + "            |> LigationRateParam" + Nl
            | CatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "            |> CatalyticLigationRateParam" + Nl
            | SedimentationDirectRateParam m -> (m.toFSharpCode shift) + shift + "            |> SedimentationDirectRateParam" + Nl
            | SedimentationAllRateParam m -> (m.toFSharpCode shift) + shift + "            |> SedimentationAllRateParam" + Nl


    type ReactionRateModel = 
        | SynthesisRateModel of SyntethisModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel

        member rm.inputParams = 
            match rm with
            | SynthesisRateModel m -> m.inputParams |> SynthesisRateParam
            | CatalyticSynthesisRateModel m -> m.inputParams.catSynthParam |> CatalyticSynthesisRateParam
            | LigationRateModel m -> m.inputParams |> LigationRateParam
            | CatalyticLigationRateModel m -> m.inputParams.catLigationParam |> CatalyticLigationRateParam
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


    let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
        (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) = 
        match mo with 
        | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
        | None -> (None, None)


    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl (a : Reaction) = 
            match a with 
            | Synthesis r -> getModelRates (p.tryFindSynthesisModel()) r
            | CatalyticSynthesis r -> getModelRates (p.tryFindCatalyticSynthesisModel()) r
            | Ligation r -> getModelRates (p.tryFindLigationModel()) r
            | CatalyticLigation r ->  getModelRates (p.tryFindCatalyticLigationModel()) r
            | SedimentationDirect r -> getModelRates (p.tryFindSedimentationDirectModel()) r
            | SedimentationAll r -> getModelRates (p.tryFindSedimentationAllModel()) r

        member __.getRates (a : Reaction) = getRatesImpl a
        member __.toParamFSharpCode shift = p.rateModels |> List.map (fun e -> e.inputParams.toFSharpCode shift) |> String.concat Nl

        static member defaultSynthesisModel (rnd : Random) forward backward =
            {
                synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                //synthesisDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SyntethisModel

        static member defaultCatalyticSynthesisModel (rnd : Random) m threshold mult =
            {
                catSynthParam = 
                    {
                        catSynthDistribution = TriangularDistribution(rnd.Next(), { threshold = threshold }) |> Triangular
                        multiplier  = mult
                        maxEe = 0.05
                    }
                synthesisModel = m
            }
            |> CatalyticSynthesisModel

        static member defaultLigationModel (rnd : Random) forward backward =
            {
                ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                //ligationDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigationModel

        static member defaultCatalyticLigationModel (rnd : Random) m threshold mult =
            {
                catLigationParam = 
                    {
                        catLigationDistribution = TriangularDistribution(rnd.Next(), { threshold = threshold }) |> Triangular
                        multiplier  = mult
                        maxEe = 0.05
                    }
                ligationModel = m
            }
            |> CatalyticLigationModel

        static member defaultSedimentationDirectModel (rnd : Random) threshold mult =
            {
                sedimentationDirectDistribution = TriangularDistribution(rnd.Next(), { threshold = Some threshold }) |> Triangular
                forwardScale = Some mult
            }
            |> SedimentationDirectModel

        static member defaultSedimentationAllModel (rnd : Random) mult =
            {
                sedimentationAllDistribution = TriangularDistribution(rnd.Next(), { threshold = None }) |> Triangular
                forwardScale = Some mult
            }
            |> SedimentationAllModel

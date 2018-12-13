namespace ClmGenerator

open Clm.Distributions
open Clm.ReactionRates

module FSharpCodeExt = 

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


    type DistributionParams

        with
        member this.toFSharpCode = "{ threshold = " + (doubleOptFSharpString this.threshold) + " }"


    type DeltaDistribution
        with

        member distr.toFSharpCode = "DeltaDistribution(" + distr.seedValue.ToString() + ", " + distr.distributionParams.toFSharpCode + ")"


    type UniformDistribution
        with

        member distr.toFSharpCode = "UniformDistribution(" + distr.seedValue.ToString() + ", " + distr.distributionParams.toFSharpCode + ")"


    type TriangularDistribution
        with

        member distr.toFSharpCode = "TriangularDistribution(" + distr.seedValue.ToString() + ", " + distr.distributionParams.toFSharpCode + ")"


    type Distribution
        with

        member this.toFSharpCode =
            match this with
            | Delta d -> d.toFSharpCode + " |> Delta"
            | Uniform d -> d.toFSharpCode + " |> Uniform"
            | Triangular d -> d.toFSharpCode + " |> Triangular"


    type SynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                synthesisDistribution = " + p.synthesisDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type SynthesisParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | SynthRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "SynthRndParam" + Nl)


    type CatalyticSynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catSynthDistribution = " + p.catSynthDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type CatalyticSynthesisSimilarParam
        with

        member p.toFSharpCode (shift : string) (aminoAcidsCode : string) = 
            shift + "            {" + Nl +
            shift + "                simSynthDistribution = " + p.simSynthDistribution.toFSharpCode + Nl +
            shift + "                aminoAcids = " + aminoAcidsCode + Nl +
            shift + "            }" + Nl


    type CatalyticSynthesisParam
        with 

        member p.toFSharpCode (shift : string) (aminoAcidsCode : string) = 
            match p with 
            | CatSynthRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatSynthRndParam" + Nl)
            | CatSynthSimParam q -> (q.toFSharpCode shift aminoAcidsCode) + (shift + "            |> " + "CatSynthSimParam" + Nl)


    type SedimentationDirectRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationDirectDistribution = " + p.sedimentationDirectDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type SedimentationDirectParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | SedDirRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "SedDirRndParam" + Nl)


    type SedimentationAllRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationAllDistribution = " + p.sedimentationAllDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type SedimentationAllParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | SedAllRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "SedAllRndParam" + Nl)


    type LigationRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                ligationDistribution = " + p.ligationDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type LigationParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | LigRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "LigRndParam" + Nl)


    type CatalyticLigationRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catLigationDistribution = " + p.catLigationDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type CatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) = 
            match p with 
            | CatLigRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatLigRndParam" + Nl)


    type RacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                racemizationDistribution = " + p.racemizationDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type RacemizationParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | RacemRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "RacemRndParam" + Nl)


    type CatalyticRacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catRacemDistribution = " + p.catRacemDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type CatalyticRacemizationSimilarParam
        with

        member p.toFSharpCode (shift : string) (aminoAcidsCode : string) = 
            shift + "            {" + Nl +
            shift + "                simRacemDistribution = " + p.simRacemDistribution.toFSharpCode + Nl +
            shift + "                aminoAcids = " + aminoAcidsCode + Nl +
            shift + "            }" + Nl


    type CatalyticRacemizationParam
        with 

        member p.toFSharpCode (shift : string) (aminoAcidsCode : string) = 
            match p with 
            | CatRacemRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatRacemRndParam" + Nl)
            | CatRacemSimParam q -> (q.toFSharpCode shift aminoAcidsCode) + (shift + "            |> " + "CatRacemSimParam" + Nl)


    type FSharpCodeParams = 
        {
            shift : string
            aminoAcidsCode : string
        }


    type ReactionRateModelParam
        with

        member rm.toFSharpCode (p : FSharpCodeParams) = 
            match rm with 
            | SynthesisRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SynthesisRateParam" + Nl
            | CatalyticSynthesisRateParam m -> (m.toFSharpCode p.shift p.aminoAcidsCode) + p.shift + "            |> CatalyticSynthesisRateParam" + Nl
            | LigationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> LigationRateParam" + Nl
            | CatalyticLigationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> CatalyticLigationRateParam" + Nl
            | SedimentationDirectRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SedimentationDirectRateParam" + Nl
            | SedimentationAllRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SedimentationAllRateParam" + Nl
            | RacemizationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> RacemizationRateParam" + Nl
            | CatalyticRacemizationRateParam m -> (m.toFSharpCode p.shift p.aminoAcidsCode) + p.shift + "            |> CatalyticRacemizationRateParam" + Nl


    type ReactionRateProvider
        with

        member rrp.toParamFSharpCode shift = rrp.providerParams.rateModels |> List.map (fun e -> e.inputParams.toFSharpCode shift) |> String.concat Nl

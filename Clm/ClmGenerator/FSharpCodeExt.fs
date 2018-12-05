namespace Clm

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


    type SyntethisParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                synthesisDistribution = " + p.synthesisDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type CatalyticSynthesisParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catSynthDistribution = " + p.catSynthDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl


    type SedimentationDirectParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationDirectDistribution = " + p.sedimentationDirectDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type SedimentationAllParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                sedimentationAllDistribution = " + p.sedimentationAllDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "            }" + Nl


    type LigationParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                ligationDistribution = " + p.ligationDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type CatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catLigationDistribution = " + p.catLigationDistribution.toFSharpCode + Nl +
            shift + "                multiplier = " + (doubleFSharpString p.multiplier) + Nl +
            shift + "                maxEe = " + (doubleFSharpString p.maxEe) + Nl +
            shift + "            }" + Nl

    type ReactionRateModelParam
        with

        member rm.toFSharpCode (shift : string) = 
            match rm with 
            | SynthesisRateParam m -> (m.toFSharpCode shift) + shift + "            |> SynthesisRateParam" + Nl
            | CatalyticSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "            |> CatalyticSynthesisRateParam" + Nl
            | LigationRateParam m -> (m.toFSharpCode shift) + shift + "            |> LigationRateParam" + Nl
            | CatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "            |> CatalyticLigationRateParam" + Nl
            | SedimentationDirectRateParam m -> (m.toFSharpCode shift) + shift + "            |> SedimentationDirectRateParam" + Nl
            | SedimentationAllRateParam m -> (m.toFSharpCode shift) + shift + "            |> SedimentationAllRateParam" + Nl

    type ReactionRateProvider
        with

        member rrp.toParamFSharpCode shift = rrp.providerParams.rateModels |> List.map (fun e -> e.inputParams.toFSharpCode shift) |> String.concat Nl

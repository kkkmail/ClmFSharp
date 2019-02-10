namespace Clm.Generator

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open ClmSys.GeneralData

module FSharpCodeExt = 

    let increaseShift shift = shift + "    "
    let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray


    let fold f state (arr: 'a [,]) =
        Seq.cast<'a> arr
        |> Seq.fold f state


    let toFloat (s : string) = 
        match s.Contains(".") with 
        | true -> s
        | false -> 
            match s.ToUpper().Contains("E+") || s.ToUpper().Contains("E-") with
            | true -> s
            | false -> s + ".0"


    let doubleFSharpString (d : double) = d.ToString() |> toFloat


    let doubleOptFSharpString (d : double option) = 
        match d with 
        | Some v -> "Some " + (v.ToString() |> toFloat)
        | None -> "None"


    let arrayToFSharpString (a : double[]) (shift : string) = 
        let s = 
            a
            |> Array.map (fun e -> doubleFSharpString e)
            |> String.concat "; "
        shift + "[| " + s + " |]"


    let array2DToFSharpString (a : double[,]) (shift : string) = 
        let arrayShift = shift |> increaseShift

        let s = 
            [| for i in 0..((Array2D.length1 a) - 1) -> i |]
            |> Array.map (fun i -> a.[i,*])
            |> Array.map (fun e -> arrayToFSharpString e arrayShift)
            |> String.concat Nl

        shift + "[| " + Nl + 
        s + Nl + 
        shift + "|]" + Nl


    type Substance
        with
            member this.toFSharpCode shift = shift + (this.ToString())


    type DistributionParams

        with

        member this.toFSharpCode =
            "{ " +
            "threshold = " + (doubleOptFSharpString this.threshold) + "; " +
            "scale = " + (doubleOptFSharpString this.scale) + "; " +
            "shift = " + (doubleOptFSharpString this.shift) + 
            " }"


    type DistributionParamsWithType
        with

        member this.toFSharpCode =
            "{ " +
            "distributionType = " + this.distributionType.ToString() + "; " +
            "distributionParams = " + this.distributionParams.toFSharpCode +
            " }"


    type Distribution
        with

        member this.toFSharpCode = this.value.toFSharpCode + " |> " + "Distribution"


    type EeDistribution
        with

        member distr.toFSharpCode = 
            let (EeDistribution d) = distr
            d.toFSharpCode + " |> " + "EeDistribution"


    let toEeDistrOpt (distr : EeDistribution option) = 
        match distr with 
        | Some d -> d.toFSharpCode + " |> " + "Some"
        | None -> "None"


    type EeDistributionGetter
        with 
        member distr.toFSharpCode = 
            match distr with 
            | NoneEeGetter -> "NoneEeGetter"
            | DeltaEeDistributionGetter -> "DeltaEeDistributionGetter"
            | CenteredEeDistributionGetter -> "CenteredEeDistributionGetter"


    type RateMultiplierDistribution
        with

        member distr.toFSharpCode = 
            match distr with 
            | RateMultDistr d -> d.toFSharpCode + " |> " + "RateMultDistr"
            | NoneRateMult -> "NoneRateMult"


    type RateMultiplierDistributionGetter
        with
        member distr.toFSharpCode = 
            match distr with 
            | NoneRateMultDistrGetter -> "NoneRateMultDistrGetter"
            | DeltaRateMultDistrGetter -> "DeltaRateMultDistrGetter"
            | TriangularRateMultDistrGetter -> "TriangularRateMultDistrGetter"
            | SymmetricTriangularRateMultDistrGetter -> "SymmetricTriangularRateMultDistrGetter"


    type CatRatesEeParam
        with 
        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                rateMultiplierDistr = " + (p.rateMultiplierDistr.toFSharpCode) + Nl +
            shift + "                eeForwardDistribution = " + (toEeDistrOpt p.eeForwardDistribution) + Nl +
            shift + "                eeBackwardDistribution = " + (toEeDistrOpt p.eeBackwardDistribution) + Nl +
            shift + "            }" + Nl


    type FoodCreationParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                foodCreationRate = " + (doubleFSharpString p.foodCreationRate) + Nl +
            shift + "            }" + Nl


    type WasteRemovalParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                wasteRemovalRate = " + (doubleFSharpString p.wasteRemovalRate) + Nl +
            shift + "            }" + Nl


    type WasteRecyclingParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                wasteRecyclingRate = " + (doubleFSharpString p.wasteRecyclingRate) + Nl +
            shift + "            }" + Nl


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
            shift + "                catSynthRndEeParams = " + Nl + (p.catSynthRndEeParams.toFSharpCode (increaseShift shift)) +
            shift + "            }" + Nl


    type CatRatesSimilarityParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                simBaseDistribution = " + p.simBaseDistribution.toFSharpCode + Nl +
            shift + "                getRateMultiplierDistr = " + p.getRateMultiplierDistr.toFSharpCode + Nl +
            shift + "                getForwardEeDistr = " + p.getForwardEeDistr.toFSharpCode + Nl +
            shift + "                getBackwardEeDistr = " + p.getBackwardEeDistr.toFSharpCode + Nl +
            shift + "            }" + Nl


    type CatalyticSynthesisSimilarParam
        with
        member p.toFSharpCode (shift : string) = @"failwith ""CatalyticSynthesisSimilarParam.toFSharpCode - is not implemented."""


    type CatalyticSynthesisParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | CatSynthRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatSynthRndParam" + Nl)
            | CatSynthSimParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatSynthSimParam" + Nl)


    type DestructionRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                destructionDistribution = " + p.destructionDistribution.toFSharpCode + Nl +
            shift + "                forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "                backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "            }" + Nl


    type DestructionParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | DestrRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "DestrRndParam" + Nl)


    type CatalyticDestructionRandomParam
        with

        member p.toFSharpCode (shift : string) = 
            shift + "            {" + Nl +
            shift + "                catDestrRndEeParams = " + Nl + (p.catDestrRndEeParams.toFSharpCode (increaseShift shift)) +
            shift + "            }" + Nl


    type CatalyticDestructionSimilarParam
        with
        member p.toFSharpCode (shift : string) = @"failwith ""CatalyticDestructionSimilarParam.toFSharpCode - is not implemented."""


    type CatalyticDestructionParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | CatDestrRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatDestrRndParam" + Nl)
            | CatDestrSimParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatDestrSimParam" + Nl)


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
            shift + "                catLigRndEeParams = " + Nl + (p.catLigRndEeParams.toFSharpCode (increaseShift shift)) +
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
            shift + "                catRacemRndEeParams = " + Nl + (p.catRacemRndEeParams.toFSharpCode (increaseShift shift)) +
            shift + "            }" + Nl


    type CatalyticRacemizationSimilarParam
        with
        member p.toFSharpCode (shift : string) = @"failwith ""CatalyticRacemizationSimilarParam.toFSharpCode - is not implemented."""


    type CatalyticRacemizationParam
        with 

        member p.toFSharpCode (shift : string) = 
            match p with 
            | CatRacemRndParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatRacemRndParam" + Nl)
            | CatRacemSimParam q -> (q.toFSharpCode shift) + (shift + "            |> " + "CatRacemSimParam" + Nl)


    type FSharpCodeParams =
        {
            shift : string
            aminoAcidsCode : string
        }


    type ReactionRateModelParam
        with

        member rm.toFSharpCode (p : FSharpCodeParams) = 
            match rm with 
            | FoodCreationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> FoodCreationRateParam" + Nl
            | WasteRemovalRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> WasteRemovalRateParam" + Nl
            | WasteRecyclingRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> WasteRecyclingRateParam" + Nl
            | SynthesisRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SynthesisRateParam" + Nl
            | DestructionRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> DestructionRateParam" + Nl
            | CatalyticSynthesisRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> CatalyticSynthesisRateParam" + Nl
            | CatalyticDestructionRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> CatalyticDestructionRateParam" + Nl
            | LigationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> LigationRateParam" + Nl
            | CatalyticLigationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> CatalyticLigationRateParam" + Nl
            | SedimentationDirectRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SedimentationDirectRateParam" + Nl
            | SedimentationAllRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> SedimentationAllRateParam" + Nl
            | RacemizationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> RacemizationRateParam" + Nl
            | CatalyticRacemizationRateParam m -> (m.toFSharpCode p.shift) + p.shift + "            |> CatalyticRacemizationRateParam" + Nl


    type ReactionRateModelParamWithUsage
        with

        member rrmp.toFSharpCode (p : FSharpCodeParams) =
            p.shift + "            {" + Nl +
            p.shift + "                modelParam = " + Nl + (rrmp.modelParam.toFSharpCode { p with shift = p.shift |> increaseShift |> increaseShift } ) +
            p.shift + "                usage = " + rrmp.usage.ToString() + Nl +
            p.shift + "            }" + Nl


    type ReactionRateProviderParams
        with

        member rrp.toParamFSharpCode (p : FSharpCodeParams) =
            rrp.allParams
            |> List.map (fun e -> e.toFSharpCode p) |> String.concat Nl

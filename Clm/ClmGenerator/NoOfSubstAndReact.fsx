#r @"..\packages\FSharp.Data.SqlClient.2.0.1\lib\net40\FSharp.Data.SqlClient.dll"
#r @"..\packages\Argu.5.2.0\lib\net45\Argu.dll"
#load @"..\ClmSys\VersionInfo.fs"
#load @"..\ClmSys\GeneralData.fs"
#load @"..\Clm\Substances.fs"
#load @"..\Clm\ReactionTypes.fs"
#load @"..\Clm\Distributions.fs"
#load @"..\Clm\ReactionRates.fs"
#load @"..\Clm\Reactions.fs"
#load @"..\Clm\DataLocation.fs"
#load @"..\Clm\CommandLine.fs"
#load @"..\Clm\ModelParams.fs"
#load "FSharpCodeExt.fs"
#load @"..\ClmDefaults\DefaultValuesExt.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_000.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_001.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_002.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_003.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_004.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_005.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_006.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_007.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_008.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_009.fs"
#load @"..\ClmDefaults\DefaultValues\Defaults_010.fs"
#load @"..\ClmDefaults\AllDefaults.fs"
#load "ReactionRatesExt.fs"
#load "ClmModelData.fs"
//#load "ClmModel.fs"

open System
open ClmSys.VersionInfo
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation
open Clm.ReactionTypes
open Clm.Generator.ClmModelData
open ClmDefaults.AllDefaults

//open Clm.Generator.ClmModel
//===========================================================
let numberOfAminoAcids = NumberOfAminoAcids.SixAminoAcids
let maxPeptideLength = MaxPeptideLength.ThreeMax
let reactionName = ReactionName.CatalyticDestructionName
let seed = 1
//===========================================================
printfn "Starting..."

let getInfo maxPeptideLength numberOfAminoAcids =
    let si = SubstInfo.create maxPeptideLength numberOfAminoAcids

    printfn "numberOfAminoAcids = %A" numberOfAminoAcids
    printfn "aminoAcids.Length = %A" si.aminoAcids.Length
    printfn "chiralAminoAcids.Length = %A" si.chiralAminoAcids.Length
    printfn "peptides.Length = %A" si.peptides.Length
    printfn "allChains.Length = %A" si.allChains.Length
    printfn "allSubst.Length = %A" si.allSubst.Length
    printfn "destrCatalysts.Length = %A" si.destrCatalysts.Length
    printfn "ligationPairs.Length = %A" si.ligationPairs.Length
    printfn "\n***\n"
    si

//let a =
//    [
//        OneAminoAcid
//        TwoAminoAcids
//        ThreeAminoAcids
//        FourAminoAcids
//        FiveAminoAcids
//        SixAminoAcids
//        SevenAminoAcids
//        EightAminoAcids
//        NineAminoAcids
//        TenAminoAcids
//
//        ElevenAminoAcids
//        TwelveAminoAcids
//        ThirteenAminoAcids
//        FourteenAminoAcids
//        FifteenAminoAcids
//        SixteenAminoAcids
//
//        SeventeenAminoAcids
//        EighteenAminoAcids
//        NineteenAminoAcids
//        TwentyAminoAcids
//
//        //TwentyOneAminoAcids
//        //TwentyTwoAminoAcids
//        //TwentyThreeAminoAcids
//        //TwentyFourAminoAcids
//        //TwentyFiveAminoAcids
//        //TwentySixAminoAcids
//        //TwentySevemnAminoAcids
//        //TwentyEightAminoAcids
//        //TwentyNineAminoAcids
//        //ThirtyAminoAcids
//        //ThirtyOneAminoAcids
//        //ThirtyTwoAminoAcids
//    ]
//
//a |> List.map(fun n -> getInfo maxPeptideLength n)

#time
let si = getInfo maxPeptideLength numberOfAminoAcids
#time

let rndBF = new Random(seed)
let rndRC = new Random(seed)

let getRateProvider rnd =
    (getDefaultValues 1 |> fst).getDefaultRateModels rnd numberOfAminoAcids 
    |> ReactionRateProvider

let rateProviderBF = getRateProvider rndBF
let rateProviderRC = getRateProvider rndRC

#time
printfn "BruteForce"
let bf = RateGenerationData.create BruteForce rateProviderBF si
let bfr = bf.getReactions rateProviderBF reactionName
printfn "bfr.Length = %A" bfr.Length
#time

#time
printfn "RandomChoice"
let rc = RateGenerationData.create RandomChoice rateProviderRC si
let rcr = rc.getReactions rateProviderRC reactionName
printfn "rcr.Length = %A" rcr.Length
#time


printfn "... completed."
//===========================================================

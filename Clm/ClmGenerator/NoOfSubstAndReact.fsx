﻿#r @"..\packages\FSharp.Data.SqlClient.2.0.1\lib\net40\FSharp.Data.SqlClient.dll"
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
//#load @"..\ClmDefaults\AllDefaults.fs"
//#load "ClmModel.fs"

open System
open ClmSys.VersionInfo
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation
//open Clm.Generator.ClmModel
//===========================================================
let numberOfAminoAcids = NumberOfAminoAcids.ElevenAminoAcids
let maxPeptideLength = MaxPeptideLength.ThreeMax
//===========================================================
printfn "Starting..."

let getInfo maxPeptideLength numberOfAminoAcids =
    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids
    let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))
    let allLigChains = allChains |> List.filter(fun a -> a.Length < maxPeptideLength.length)

    //let allChainsSeq = allChains |> Seq.ofList

    //let allPairsSeq =
    //    Seq.allPairs allChainsSeq allChainsSeq
    //    |> Seq.map (fun (a, b) -> orderPairs (a, b))
    //    |> Seq.filter (fun (a, _) -> a.Head.isL)
    //    |> Seq.distinct

    //let ligationPairs =
    //    allPairsSeq
    //    |> Seq.filter (fun (a, b) -> a.Length + b.Length <= maxPeptideLength.length)
    //    |> Seq.toList

    let ligationPairs2 =
        List.allPairs allLigChains allLigChains
        |> List.filter (fun (a, b) -> a.Length + b.Length <= maxPeptideLength.length)
        |> List.map (fun (a, b) -> orderPairs (a, b))
        |> List.filter (fun (a, _) -> a.Head.isL)
        |> List.distinct

    printfn "numberOfAminoAcids = %A" numberOfAminoAcids
    printfn "aminoAcids.Length = %A" aminoAcids.Length
    printfn "chiralAminoAcids.Length = %A" chiralAminoAcids.Length
    printfn "peptides.Length = %A" peptides.Length
    printfn "allChains.Length = %A" allChains.Length
    printfn "allLigChains.Length = %A" allLigChains.Length
    //printfn "ligationPairs.Length = %A" ligationPairs.Length
    printfn "ligationPairs2.Length = %A" ligationPairs2.Length
    //printfn "(ligationPairs = ligationPairs2) = %A" (ligationPairs = ligationPairs2)
    printfn "\n***\n"

let a =
    [
        OneAminoAcid
        TwoAminoAcids
        ThreeAminoAcids
        FourAminoAcids
        FiveAminoAcids
        SixAminoAcids
        SevenAminoAcids
        EightAminoAcids
        NineAminoAcids
        TenAminoAcids

        ElevenAminoAcids
        TwelveAminoAcids
        ThirteenAminoAcids
        FourteenAminoAcids
        FifteenAminoAcids
        SixteenAminoAcids

        SeventeenAminoAcids
        EighteenAminoAcids
        NineteenAminoAcids
        TwentyAminoAcids

        //TwentyOneAminoAcids
        //TwentyTwoAminoAcids
        //TwentyThreeAminoAcids
        //TwentyFourAminoAcids
        //TwentyFiveAminoAcids
        //TwentySixAminoAcids
        //TwentySevemnAminoAcids
        //TwentyEightAminoAcids
        //TwentyNineAminoAcids
        //ThirtyAminoAcids
        //ThirtyOneAminoAcids
        //ThirtyTwoAminoAcids
    ]

a |> List.map(fun n -> getInfo maxPeptideLength n)

printfn "... completed."
//===========================================================

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

    let allChainsSeq = allChains |> Seq.ofList

    let allPairsSeq =
        Seq.allPairs allChainsSeq allChainsSeq
        |> Seq.map (fun (a, b) -> orderPairs (a, b))
        |> Seq.filter (fun (a, _) -> a.Head.isL)
        |> Seq.distinct

    let ligationPairs =
        allPairsSeq
        |> Seq.filter (fun (a, b) -> a.Length + b.Length <= maxPeptideLength.length)
        |> Seq.toList

    printfn "numberOfAminoAcids = %A" numberOfAminoAcids
    printfn "aminoAcids.Length = %A" aminoAcids.Length
    printfn "chiralAminoAcids.Length = %A" chiralAminoAcids.Length
    printfn "peptides.Length = %A" peptides.Length
    printfn "allChains.Length = %A" allChains.Length
    printfn "ligationPairs.Length = %A" ligationPairs.Length
    printfn "\n***\n"

let a =
    [
        ElevenAminoAcids
        TwelveAminoAcids
        ThirteenAminoAcids
        FourteenAminoAcids
        FifteenAminoAcids
        SixteenAminoAcids

        //SeventeenAminoAcids
        //EighteenAminoAcids
        //NineteenAminoAcids
        //TwentyAminoAcids
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

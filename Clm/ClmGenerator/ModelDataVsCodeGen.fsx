//--quiet
printfn "0"
//#r @"..\SolverRunner\bin\Debug\FSharp.Core.dll"
open System

#r @"..\packages\FSharp.Data.SqlClient.2.0.2\lib\net40\FSharp.Data.SqlClient.dll"
#r @"..\packages\Newtonsoft.Json.12.0.1\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\Argu.5.2.0\lib\net45\Argu.dll"

//#r @"..\SolverRunner\bin\Debug\FSharp.Data.SqlClient.dll"
//#r @"..\SolverRunner\bin\Debug\Newtonsoft.Json.dll"
//#r @"..\SolverRunner\bin\Debug\Argu.dll"

#r @"..\SolverRunner\bin\Debug\ClmSys.dll"
#r @"..\SolverRunner\bin\Debug\Clm.dll"
#r @"..\SolverRunner\bin\Debug\DbData.dll"
printfn "0.1"
#r @"..\SolverRunner\bin\Debug\Model.dll"

printfn "1"

//#load @"..\ClmSys\VersionInfo.fs"
//#load @"..\ClmSys\GeneralData.fs"
//#load @"..\Clm\Substances.fs"
//#load @"..\Clm\ReactionTypes.fs"
//#load @"..\Clm\Distributions.fs"
//#load @"..\Clm\ReactionRates.fs"
//#load @"..\Clm\Reactions.fs"
//#load @"..\Clm\DataLocation.fs"
//#load @"..\Clm\CommandLine.fs"
//#load @"..\Clm\ModelParams.fs"
//#load @"..\Clm\CalculationData.fs"

//#load @"..\DbData\Configuration.fs"
//#load @"..\DbData\DatabaseTypes.fs"

//#load "FSharpCodeExt.fs"
//#load @"..\ClmDefaults\DefaultValuesExt.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_000.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_001.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_002.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_003.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_004.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_005.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_006.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_007.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_008.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_009.fs"
//#load @"..\ClmDefaults\DefaultValues\Defaults_010.fs"
//#load @"..\ClmDefaults\AllDefaults.fs"
//#load "ReactionRatesExt.fs"
//#load "ClmModelData.fs"
//#load "ClmModel.fs"


open ClmSys.VersionInfo
open ClmSys.GeneralData
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.CalculationData
open Clm.DataLocation
open Clm.ReactionTypes
open Clm.Reactions
open Clm.ModelParams

//open Clm.Generator.ClmModelData
//open ClmDefaults.AllDefaults
open DbData.DatabaseTypes
open DbData.Configuration
printfn "2"

//#load @"..\Model\ModelData.fs"
open Clm.Model.ModelData
printfn "3"

//open Clm.Generator.ClmModel
//===========================================================
//let numberOfAminoAcids = NumberOfAminoAcids.TwentyAminoAcids
//let maxPeptideLength = MaxPeptideLength.ThreeMax
//let reactionName = ReactionName.CatalyticDestructionName
//let seed = 5
//let defaultIdx = 2
//===========================================================
printfn "Starting..."
//===========================================================
let cgModelDataParamsWithExtraData = modelDataParamsWithExtraData
let cgGetTotalSubst = getTotalSubst
let cgGetTotals = getTotals
let cgUpdate = update
//===========================================================
printfn "Trying to load..."
let modelDataId = ModelDataId cgModelDataParamsWithExtraData.modelDataParams.modelInfo.modelDataId
let mdo = tryLoadModelData modelDataId clmConnectionString
let modelSettings = loadModelSettings modelDataId clmConnectionString

let rnd = new Random()
let x = [| for _ in 1..cgModelDataParamsWithExtraData.allSubst.Length -> rnd.NextDouble() |]

match mdo with
| Some md ->
    let mdGetTotalSubst = md.modelData.calculationData.getTotalSubst
    let mdGetTotals = md.modelData.calculationData.getTotals
    let mdUpdate = md.modelData.calculationData.getDerivative

    let cgTotalSubst = cgGetTotalSubst x
    let mdTotalSubst = mdGetTotalSubst x
    printfn "cgTotalSubst = %A, mdTotalSubst = %A, diff = %A" cgTotalSubst mdTotalSubst (cgTotalSubst - mdTotalSubst)

    let cgGetTotals = cgGetTotals x
    let mdGetTotals = mdGetTotals x
    let diffTotals = 
        Array.zip cgGetTotals mdGetTotals
        |> Array.map(fun ((a1, b1), (a2, b2)) -> (a1 - a2) * (a1 - a2) + (b1 - b2) * (b1 - b2))
        |> Array.sum

    printfn "diffTotals = %A" diffTotals

    let cgUpdate = cgUpdate x
    let mdUpdate = mdUpdate x
    let diffUpdate = 
        Array.zip cgUpdate mdUpdate
        |> Array.map(fun (a, b) -> (a - b) * (a - b))
        |> Array.sum

    printfn "diffUpdate = %A" diffUpdate
| None -> printfn "Failed to load model data."

let mdUpdate = mdo.Value.modelData.calculationData.getDerivative

let repetitions = [for i in 1..1_000 -> i ]

printfn "\n\nCG"
#time
repetitions |> List.map (fun _ -> cgUpdate x |> ignore)
#time
printfn "CG completed\n\n"

printfn "MD"
#time
repetitions |> List.map (fun _ -> mdUpdate x |> ignore)
#time
printfn "MD completed\n\n"

//printfn "x = %A" x
//===========================================================

//let getInfo maxPeptideLength numberOfAminoAcids =
//    let si = SubstInfo.create maxPeptideLength numberOfAminoAcids

//    printfn "numberOfAminoAcids = %A" numberOfAminoAcids
//    printfn "aminoAcids.Length = %A" si.aminoAcids.Length
//    printfn "chiralAminoAcids.Length = %A" si.chiralAminoAcids.Length
//    printfn "peptides.Length = %A" si.peptides.Length
//    printfn "allChains.Length = %A" si.allChains.Length
//    printfn "allSubst.Length = %A" si.allSubst.Length
//    printfn "destrCatalysts.Length = %A" si.destrCatalysts.Length
//    printfn "ligationPairs.Length = %A" si.ligationPairs.Length
//    printfn "\n***\n"
//    si

//#time
//let si = getInfo maxPeptideLength numberOfAminoAcids
//#time

//let rndBF = new Random(seed)
//let rndRC = new Random(seed)

//let getRateProvider rnd =
//    (getDefaultValues defaultIdx |> fst).getDefaultRateModels rnd numberOfAminoAcids 
//    |> ReactionRateProvider

//let normalizeReactions rr =
//    rr
//    |> List.distinct
//    |> List.map (fun e -> match e with | Reversible r -> Some r | _ -> None)
//    |> List.choose id
//    |> List.map (fun e ->
//                    match e.reaction with 
//                    | CatalyticDestruction r -> Some (r, { forwardRate = Some e.forwardRate; backwardRate = Some e.backwardRate })
//                    | _ -> None)
//    |> List.choose id
//    |> List.sortBy (fun (r, _) -> r)


//let verify (rcm : list<(CatalyticDestructionReaction * RateData)>) =
//    printfn "Verifying..."
//    let rm = rcm |> Map.ofList

//    let failed =
//        rcm
//        |> List.map (fun (r, d) ->
//                        match rm.TryFind r.enantiomer with
//                        | Some ed ->
//                            match d = ed with
//                            | false -> Some r
//                            | true -> None
//                        | None -> Some r)
//        |> List.choose id

//    printfn "failed.Length = %A" failed.Length
////===========================================================
//printfn "RandomChoice"
//let rateProviderRC = getRateProvider rndRC

//#time
//let rc = RateGenerationData.create RandomChoice rateProviderRC si
//let rcr = rc.getReactions rateProviderRC reactionName
//#time

//printfn "rcr.Length = %A" rcr.Length
//printfn "(rcr |> List.distinct).Length = %A" (rcr |> List.distinct).Length

//rcr
//|> normalizeReactions
//|> verify
////===========================================================
//printfn "... completed."
////===========================================================

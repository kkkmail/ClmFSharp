printfn "Loading..."
open System

#r @"..\packages\FSharp.Data.SqlClient.2.0.2\lib\net40\FSharp.Data.SqlClient.dll"
#r @"..\packages\Newtonsoft.Json.12.0.1\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\Argu.5.2.0\lib\net45\Argu.dll"

//#r @"..\SolverRunner\bin\Debug\FSharp.Data.SqlClient.dll"
//#r @"..\SolverRunner\bin\Debug\Newtonsoft.Json.dll"
//#r @"..\SolverRunner\bin\Debug\Argu.dll"

#r @"..\Model\bin\Debug\ClmSys.dll"
#r @"..\Model\bin\Debug\Clm.dll"
#r @"..\Model\bin\Debug\DbData.dll"
#r @"..\Model\bin\Debug\Model.dll"

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
open Clm.ReactionTypes
open Clm.Reactions
open Clm.ModelParams

//open Clm.Generator.ClmModelData
//open ClmDefaults.AllDefaults
open DbData.DatabaseTypes
open DbData.Configuration
open Clm.Model.ModelData
//===========================================================
printfn "Starting..."
//===========================================================
let cgModelDataParamsWithExtraData = modelDataParamsWithExtraData
let cgGetTotalSubst = getTotalSubst
let cgGetTotals = getTotals
let cgUpdate = update
//===========================================================
printfn "Trying to load..."
let modelDataId = cgModelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
let mdo = tryLoadModelData modelDataId clmConnectionString

let rnd = new Random()
let x = [| for _ in 1..cgModelDataParamsWithExtraData.regularParams.allSubst.Length -> rnd.NextDouble() |]

match mdo with
| Some md ->
    let paramEq = (md.modelData.modelDataParams = cgModelDataParamsWithExtraData.regularParams.modelDataParams)
    printfn "(md.modelData.modelDataParams = cgModelDataParamsWithExtraData.modelDataParams) =\n    %A\n" paramEq

    let mdModelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
    let allParamEq = (mdModelDataParamsWithExtraData.regularParams = cgModelDataParamsWithExtraData.regularParams)
    printfn "(mdAllParam.regularParams = cgModelDataParamsWithExtraData.regularParams) =\n    %A\n" allParamEq

    let mdGetTotalSubst = md.modelData.modelBinaryData.calculationData.getTotalSubst
    let mdGetTotals = md.modelData.modelBinaryData.calculationData.getTotals
    let mdUpdate = md.modelData.modelBinaryData.calculationData.getDerivative

    let cgTotalSubst = cgGetTotalSubst x
    let mdTotalSubst = mdGetTotalSubst x
    printfn "diff = %A" (cgTotalSubst - mdTotalSubst)

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

let mdUpdate = mdo.Value.modelData.modelBinaryData.calculationData.getDerivative
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
//===========================================================
printfn "... completed."
//===========================================================

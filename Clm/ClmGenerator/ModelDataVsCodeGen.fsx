printfn "Loading..."
open System

#r @"..\packages\FSharp.Data.SqlClient.2.0.2\lib\net40\FSharp.Data.SqlClient.dll"
#r @"..\packages\Newtonsoft.Json.12.0.1\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\Argu.5.2.0\lib\net45\Argu.dll"

#r @"..\Model\bin\Debug\ClmSys.dll"
#r @"..\Model\bin\Debug\Clm.dll"
#r @"..\Model\bin\Debug\DbData.dll"
#r @"..\Model\bin\Debug\Model.dll"

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
printfn ".. loaded."

let rnd = new Random()
let x = [| for _ in 1..cgModelDataParamsWithExtraData.regularParams.allSubstData.allSubst.Length -> rnd.NextDouble() |]

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

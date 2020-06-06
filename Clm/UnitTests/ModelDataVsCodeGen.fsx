printfn "Loading..."
open System

#r @"..\packages\FSharp.Data.SqlClient.2.0.6\lib\net40\FSharp.Data.SqlClient.dll"
#r @"..\packages\Newtonsoft.Json.12.0.3\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\Argu.6.0.0\lib\netstandard2.0\Argu.dll"

#r @"..\Model\bin\Release\ClmSys.dll"
#r @"..\Model\bin\Release\Clm.dll"
#r @"..\Model\bin\Release\DbData.dll"
#r @"..\Model\bin\Release\Model.dll"

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
let mdo = loadModelData clmConnectionString modelDataId
printfn ".. loaded."

let rnd = new Random()
let x = [| for _ in 1..cgModelDataParamsWithExtraData.regularParams.allSubstData.allSubst.Length -> rnd.NextDouble() |]

let md =
    match mdo with
    | Ok md ->
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
        printfn "diff (must be close to 0.0) = %A" (cgTotalSubst - mdTotalSubst)

        let cgGetTotals = cgGetTotals x
        let mdGetTotals = mdGetTotals x
        let diffTotals =
            Array.zip cgGetTotals mdGetTotals
            |> Array.map(fun ((a1, b1), (a2, b2)) -> (a1 - a2) * (a1 - a2) + (b1 - b2) * (b1 - b2))
            |> Array.sum

        printfn "diffTotals (must be close to 0.0) = %A" diffTotals

        let cgUpdate = cgUpdate x
        let mdUpdate = mdUpdate x

        let diffUpdate =
            Array.zip cgUpdate mdUpdate
            |> Array.map(fun (a, b) -> (a - b) * (a - b))
            |> Array.sum

        printfn "diffUpdate (must be close to 0.0) = %A" diffUpdate

        if diffUpdate > 0.000001
        then
            Array.zip cgUpdate mdUpdate
            |> Array.mapi(fun i (a, b) -> i, (a, b, abs (a - b)))
            |> Array.filter (fun (i, (a, b, c)) -> c > 0.000001)
            |> Array.sortByDescending (fun (i, (a, b, c)) -> c, i)
            |> Array.map (fun (i, (a, b, c)) -> printfn "i = %A, s = %A, cg = %A, md = %A, diff = %A" i (allSubst.[i]) a b c)
            |> ignore

        md
    | Error e ->
        printfn "Failed to load model data with error: %A." e
        failwith "! Error occurred !"

let mdUpdate = md.modelData.modelBinaryData.calculationData.getDerivative
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

namespace ClmDefaults

open AllDefaults
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.ModelParams

module ClmDefaultsTasks =

    let saveAllDefaults() =
        printfn "Saving all defaults."

        let saveDefault (_, (d : ClmDefaultValue)) =
            printfn "    %A" d.clmDefaultValueId
            tryUpsertClmDefaultValue d clmConnectionString |> ignore

        defaultValues
        |> Map.toList
        |> List.map saveDefault
        |> ignore

        printfn "Completed."

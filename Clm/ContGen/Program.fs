open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.Settings
open ContGen.SettingsExt
open System
open System.Data.SqlClient


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let seed = (new Random()).Next()
    let rnd = new Random(seed)
    let rnd1 = new Random(seed)

    let rates = 
        ReactionRateProvider.getDefaultRates rnd TwoAminoAcids
        |> List.map (fun e -> e.inputParams)

    let settings = rates |> List.fold (fun acc e -> e.setValue [ (e.name, 0) ] acc) []

    use conn = new SqlConnection(ClmConnectionString)
    openConnIfClosed conn

    use truncateSettingTbl = new TruncateSettingTbl(conn)
    truncateSettingTbl.Execute() |> ignore

    let settingTable = new SettingTable()
    settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
    let inserted = settingTable.Update(conn)
    printfn "inserted = %A" inserted

    let m = loadSettings ClmConnectionString

    let loaded = 
        ReactionRateModelParam.allNames
        |> List.map (fun e -> ReactionRateModelParam.tryGet m rnd1.Next [ (e, 0) ] )
        |> List.choose id

    printfn "loaded.Length = %A" (loaded.Length)

    0

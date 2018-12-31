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

    let rnd = new Random()
    let rates = ReactionRateProvider.getDefaultRates rnd TwoAminoAcids
    let settings = rates |> List.fold (fun acc e -> e.inputParams.setValue [ (e.inputParams.name, 0) ] acc) []

    use conn = new SqlConnection(ClmConnectionString)
    let settingTable = new SettingTable()
    settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
    let inserted = settingTable.Update(conn)
    printfn "inserted = %A" inserted

    0

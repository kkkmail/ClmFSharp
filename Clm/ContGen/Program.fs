open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.Settings
open ContGen.SettingsExt
open System
open System.Data.SqlClient


let saveSettings (settings : list<Setting>) conn = 
    let settingTable = new SettingTable()
    settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
    let inserted = settingTable.Update(conn)
    printfn "inserted = %A" inserted


let testAll conn = 
    let rnd = new Random()

    let rates = 
        ReactionRateProvider.getDefaultRates rnd TwoAminoAcids
        |> List.map (fun e -> e.inputParams)
        |> List.sort

    let settings = rates |> List.fold (fun acc e -> e.setValue [ (e.name, 0) ] acc) []
    saveSettings settings conn
    let m = loadSettings ClmConnectionString

    let loaded = 
        ReactionRateModelParam.allNames
        |> List.map (fun e -> ReactionRateModelParam.tryGet m rnd.Next [ (e, 0) ] )
        |> List.choose id
        |> List.sort

    printfn "loaded.Length = %A" (loaded.Length)

    let check = rates = loaded
    printfn "check= %A" check


let testDistr conn (rnd : Random) = 
    let d = TriangularDistribution (rnd.Next(), { threshold = Some 0.7; scale = Some 1.5; shift = Some 0.5 }) |> Triangular
    let settings = d.setValue [ (d.name, 0) ] []
    saveSettings settings conn
    let m = loadSettings ClmConnectionString
    let d1 = Distribution.tryGet m rnd.Next [ (d.name, 0) ]

    match d1 with 
    | Some dd -> 
        printfn "d = %A" d
        printfn "dd = %A" dd
        printfn "(d = dd) = %A" (d = dd)
    | None -> printfn "not found..."


let testSynthesisParam conn (rnd : Random) = 
    let d = (ReactionRateProvider.defaultSynthRndModel rnd (0.1, 0.01)).inputParams
    let settings = d.setValue [] []
    saveSettings settings conn
    let m = loadSettings ClmConnectionString
    let d1 = SynthesisParam.tryGet m rnd.Next []

    match d1 with 
    | Some dd -> 
        printfn "d = %A" d
        printfn "dd = %A" dd
        printfn "(d = dd) = %A" (d = dd)
    | None -> printfn "not found..."


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rnd = new Random()

    use conn = new SqlConnection(ClmConnectionString)
    openConnIfClosed conn

    use truncateSettingTbl = new TruncateSettingTbl(conn)
    truncateSettingTbl.Execute() |> ignore
    testSynthesisParam conn rnd

    0

﻿open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.Settings
open ContGen.SettingsExt
open System
open System.Data.SqlClient

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel


let saveSettings (settings : list<Setting>) conn = 
    let settingTable = new SettingTable()
    settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
    let inserted = settingTable.Update(conn)
    printfn "inserted = %A" inserted


let testAll conn (rnd : Random) = 
    let rates = (ReactionRateProvider.getDefaultRateModels rnd TwoAminoAcids).allParams |> List.sort
    let settings = ReactionRateModelParamWithUsage.setAll rates []
    saveSettings settings conn

    let m = loadSettings ClmConnectionString
    let loaded = ReactionRateModelParamWithUsage.getAll m rnd.Next

    printfn "loaded.Length = %A" (loaded.Length)

    let check = rates = loaded
    printfn "check = %A" check


let testModelGenerationParams conn (rnd : Random) =
    let numberOfAminoAcids = TwoAminoAcids
    let rates = ReactionRateProvider.getDefaultRateModels rnd TwoAminoAcids

    let modelGenerationParams = 
        {
            fileStructureVersionNumber = FileStructureVersionNumber
            versionNumber = VersionNumber
            seedValue = rnd.Next() |> Some
            numberOfAminoAcids = numberOfAminoAcids
            maxPeptideLength = ThreeMax
            reactionRateModels = rates.rateModels
            updateFuncType = UseFunctions
            modelLocationData = ModelLocationInputData.defaultValue
            updateAllModels = false
        }

    let settings = modelGenerationParams.setValue []
    saveSettings settings conn

    let m = loadSettings ClmConnectionString
    let loaded = ModelGenerationParams.tryGet m rnd.Next

    match loaded with
    | Some l ->
        printfn  "Loaded."
        let check = rates.allParams = { rateModels = l.reactionRateModels }.allParams
        printfn "check = %A" check
    | None -> printfn "Failed to load."


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
    //testAll conn rnd
    testModelGenerationParams conn rnd

    0

﻿namespace ContGen

open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates
open Clm.Generator.DefaultValuesExt
open Clm.GeneralData
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.SettingsExt
open Clm.Generator.SettingGenExt
open System
open System.Data.SqlClient

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel
open ContGen

open AsyncRun
open Runner
open System.Threading

module QuickTests =

    let testAll conn (rnd : Random) =
        let rates = (ReactionRateProvider.getDefaultRateModels rnd TwoAminoAcids).allParams |> List.sort
        let settings = ReactionRateModelParamWithUsage.setAll rates [] []
        saveSettings conn settings

        let m = loadSettings conn
        let loaded = ReactionRateModelParamWithUsage.getAll m (getRandomSeeder rnd) []

        printfn "loaded.Length = %A" (loaded.Length)

        let check = rates = loaded
        printfn "check = %A" check


    let testModelGenerationParams conn (rnd : Random) =
        let numberOfAminoAcids = TwoAminoAcids
        let rates = ReactionRateProvider.getDefaultRateModels rnd numberOfAminoAcids

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

        let settings = modelGenerationParams.setValue [] []
        saveSettings conn settings

        let m = loadSettings conn
        let loaded = ModelGenerationParams.tryGet m (getRandomSeeder rnd) []

        match loaded with
        | Some l ->
            printfn  "Loaded."
            let check = rates.allParams = { rateModels = l.reactionRateModels }.allParams
            printfn "check = %A" check
        | None -> printfn "Failed to load."


    let testDistr conn (rnd : Random) = 
        let d = TriangularDistribution (rnd.Next(), { threshold = Some 0.7; scale = Some 1.5; shift = Some 0.5 }) |> Triangular
        let settings = d.setValue [ (d.name, 0) ] []
        saveSettings conn settings
        let m = loadSettings conn
        let d1 = Distribution.tryGet m (getRandomSeeder rnd) [ (d.name, 0) ]

        match d1 with 
        | Some dd -> 
            printfn "d = %A" d
            printfn "dd = %A" dd
            printfn "(d = dd) = %A" (d = dd)
        | None -> printfn "not found..."


    let testSynthesisParam conn (rnd : Random) = 
        let d = (ReactionRateProvider.defaultSynthRndModel rnd (0.1, 0.01)).inputParams
        let settings = d.setValue [] []
        saveSettings conn settings
        let m = loadSettings conn
        let d1 = SynthesisParam.tryGet m (getRandomSeeder rnd) []

        match d1 with 
        | Some dd -> 
            printfn "d = %A" d
            printfn "dd = %A" dd
            printfn "(d = dd) = %A" (d = dd)
        | None -> printfn "not found..."

    let runMain argv = 
        printfn "%A" argv
        let rnd = new Random()

        use conn = new SqlConnection(ClmConnectionString)
        openConnIfClosed conn

        ////truncateSettings conn

        ////testAll conn rnd
        ////testModelGenerationParams conn rnd

        //let modelId = getNewModelDataId conn

        //use d = new ModelDataTableData(conn)
        //let t1 = new ModelDataTable()
        //d.Execute(modelId = modelId) |> t1.Load
        //let r1 = t1.Rows |> Seq.find (fun e -> e.modelId = modelId)
        //r1.fileStructureVersion <- FileStructureVersionNumber
        //t1.Update(conn) |> ignore

        //let runner = Runner("")
        //runner.run() |> ignore
        //let results = runProc @"C:\Temp\WTF\SolverRunner.exe" "10000 10" None

        saveDefaults conn FourAminoAcids ThreeMax |> ignore

        let a = createRunner ModelRunnerParam.defaultValue
        a.startGenerate()

        while a.getState().shuttingDown |> not do
            Thread.Sleep(30000)
            let state = a.getState()
            printfn "a.getState() = %A" state
            if state.queue.Length = 0 then a.startGenerate()

        0

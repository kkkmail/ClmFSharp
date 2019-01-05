﻿open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates
open Clm.Generator.FSharpCodeExt
open Clm.Generator.ReactionRatesExt
open Clm.Generator.ModelCommandLineParamExt

open DbData.Configuration
open DbData.DatabaseTypes
open ContGen.SettingsExt
open System
open System.Data.SqlClient

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel
open Clm.ModelParams
open ContGen

open AsyncRun
open Runner
open System.Threading
open Newtonsoft.Json

let seeder (rnd : Random) (seed : int option) = rnd.Next ()


let saveDefaults conn numberOfAminoAcids =
    let rnd = new Random()
    truncateSettings conn
    let p = AllParams.getDefaultValue rnd numberOfAminoAcids ThreeMax

    //let x =
    //    [
    //        AminoAcid.A01 |> ChiralAminoAcid.L |> Chiral
    //        Abundant |> Simple
    //        AminoAcid.A02 |> ChiralAminoAcid.R |> Chiral
    //        [ AminoAcid.A01 |> ChiralAminoAcid.L; AminoAcid.A02 |> ChiralAminoAcid.R ] |> Peptide |> PeptideChain
    //    ]

    //let y = x |> List.map (fun e -> e.toFSharpCode "")
    //let z = PeptideChain (Peptide [L A01; R A02])

    //let json = JsonConvert.SerializeObject x
    //let d = JsonConvert.DeserializeObject<list<Substance>>(json)
    //printfn "(x = d) = %A" (x = d)


    //let json = JsonConvert.SerializeObject modelGenerationParams
    //let d = JsonConvert.DeserializeObject<ModelGenerationParams>(json)
    //printfn "(modelGenerationParams = d) = %A" (modelGenerationParams = d)

    let settings =
        []
        |> p.modelGenerationParams.setValue []
        |> ModelCommandLineParam.setValues p.modelCommandLineParams []

    saveSettings settings conn
    0


let testAll conn (rnd : Random) =
    let rates = (ReactionRateProvider.getDefaultRateModels rnd TwoAminoAcids).allParams |> List.sort
    let settings = ReactionRateModelParamWithUsage.setAll rates [] []
    saveSettings settings conn

    let m = loadSettings conn
    let loaded = ReactionRateModelParamWithUsage.getAll m (seeder rnd) []

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
    saveSettings settings conn

    let m = loadSettings conn
    let loaded = ModelGenerationParams.tryGet m (seeder rnd) []

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
    let m = loadSettings conn
    let d1 = Distribution.tryGet m (seeder rnd) [ (d.name, 0) ]

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
    let m = loadSettings conn
    let d1 = SynthesisParam.tryGet m (seeder rnd) []

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

    saveDefaults conn FourAminoAcids |> ignore

    let a = createRunner ModelRunnerParam.defaultValue
    a.startGenerate()

    while a.getState().shuttingDown |> not do
        let x = rnd.Next()
        Thread.Sleep(10000)

    0

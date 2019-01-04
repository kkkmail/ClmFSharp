namespace ContGen

open System
open Clm.DataLocation
open Clm.ModelParams

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.SettingsExt
open System.Data.SqlClient
open System.Text
open Clm.Generator.ClmModel
open Clm.Generator
open AsyncRun

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Runner =

    type ModelRunnerParam =
        {
            connectionString : string
            rootBuildFolder : string
            buildTarget : string
            exeName : string
        }

        static member defaultValue =
            {
                connectionString = ClmConnectionString
                rootBuildFolder = @"C:\Temp\Clm\"
                buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"
                exeName = @"SolverRunner.exe"
            }


    type ModelRunner (p : ModelRunnerParam) =
        let rnd = new Random()
        let getBuildDir modelId = p.rootBuildFolder + (toModelName modelId) + @"\"
        let getExeName modelId = p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName
        let getRandomSeeder (seed : int option) = rnd.Next ()

        let getDeterministicSeeder (seed : int option) = 
            match seed with 
            | Some s -> s 
            | None -> rnd.Next ()


        let getModelId () =
            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn
            getNewModelDataId conn


        let loadParams seeder modelId =
            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn
            let m = loadSettings conn

            match ModelGenerationParams.tryGet m seeder with
            | Some q ->
                (
                    { q with
                        modelLocationData =
                            { q.modelLocationData with
                                modelName = ConsecutiveName modelId
                                useDefaultModeData = true
                            }
                    },
                    ModelCommandLineParam.getValues m
                )
                |> Some
            | None -> None


        let generateModel modelGenerationParams =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams
            let code = model.generateCode()
            printfn "... completed."
            code


        let saveModel (code : list<string>) (pm : ModelGenerationParams) modelId =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

            let m =
                {
                    modelId = modelId
                    numberOfAminoAcids = pm.numberOfAminoAcids
                    maxPeptideLength = pm.maxPeptideLength
                    seedValue = pm.seedValue
                    fileStructureVersion = pm.fileStructureVersionNumber
                    modelData = sb.ToString()
                }

            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn
            tryUpdateModelData conn m


        let compileModel modelId =
            let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
            Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

            // Properties
            let buildDir = getBuildDir modelId

            // Targets
            Target.create "Clean" (fun _ ->
              Shell.cleanDir buildDir
            )

            Target.create "BuildApp" (fun _ ->
              [ p.buildTarget ]
                |> MSBuild.runRelease id buildDir "Build"
                |> Trace.logItems "AppBuild-Output: "
            )

            Target.create "Default" (fun _ ->
              Trace.trace "Built completed."
            )

            "Clean"
              ==> "BuildApp"
              ==> "Default"
              |> ignore

            Target.runOrDefault "Default"


        let runModel (p : ModelCommandLineParam) modelId =
            let exeName = getExeName modelId
            let commandLineParams = p.ToString()
            runProc exeName commandLineParams None |> ignore
            modelId


        let generate() =
            try
                let modelId = getModelId ()

                match loadParams getRandomSeeder modelId with
                | Some (p, r) ->
                    let code = generateModel p
                    match saveModel code p modelId with
                    | true ->
                        compileModel modelId
                        r |> List.map (fun e -> { run = runModel e; modelId = modelId })
                    | false ->
                        printfn "Cannot save modelId: %A." modelId
                        []
                | None ->
                    printfn "Cannot load parameters for modelId: %A." modelId
                    []
            with
                | e ->
                    printfn "Exception: %A" e
                    []


        let createGeneratorImpl() =
            {
                generate = generate
            }


        member __.createGenerator = createGeneratorImpl


    let createRunner p =
        let r = ModelRunner p
        let a = r.createGenerator() |> AsyncRunner

        a

namespace ContGen

open System
open System.Diagnostics
open System.Threading
open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.SettingsExt
open System.Data.SqlClient
open System.Text

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder =
    open Clm.Generator

    type X (connStr) =
        let getModelId () = 
            use conn = new SqlConnection (connStr)
            openConnIfClosed conn
            getNewModelDataId conn


        //let modelId = getModelId ()


        let loadParams seeder modelId =
            use conn = new SqlConnection (connStr)
            openConnIfClosed conn

            let m = loadSettings conn

            match ModelGenerationParams.tryGet m seeder with
            | Some m ->
                { m with modelLocationData = { m.modelLocationData with modelName = ConsecutiveName modelId; useDefaultModeData = true } }
                |> Some
            | None -> None


        let generateModel modelGenerationParams =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams
            let code = model.generateCode()
            printfn "... completed."
            code


        let saveModel (code : list<string>) (p : ModelGenerationParams) modelId =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

            let m =
                {
                    modelId = modelId
                    numberOfAminoAcids = p.numberOfAminoAcids
                    maxPeptideLength = p.maxPeptideLength
                    seedValue = p.seedValue
                    fileStructureVersion = p.fileStructureVersionNumber
                    modelData = sb.ToString()
                }

            use conn = new SqlConnection (connStr)
            openConnIfClosed conn
            tryUpdateModelData conn m


        let compile modelId =
            let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
            Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

            // Properties
            let buildDir = @"C:\Temp\Clm\" + (toModelName modelId) + @"\"
            let buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"

            // Targets
            Target.create "Clean" (fun _ ->
              Shell.cleanDir buildDir
            )

            Target.create "BuildApp" (fun _ ->
              [ buildTarget ]
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


        // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.exited?view=netframework-4.7.2
        let runModel modelId =
            0


        member fake.sucks() = 0


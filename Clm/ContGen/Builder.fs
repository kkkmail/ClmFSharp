namespace ContGen

open System
open System.Diagnostics
open System.Threading
open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.SettingsExt
open System.Data.SqlClient
open System.Text

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel

//open Fake.DotNet
//open Fake.Core
//open Fake.IO
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder = 
    open Clm.Generator

    //// Properties
    //let buildDir = "./build/"

    //// Targets
    //Target.create "Clean" (fun _ ->
    //  Shell.cleanDir buildDir
    //)

    //Target.create "BuildApp" (fun _ ->
    //  !! "src/app/**/*.csproj"
    //    |> MSBuild.runRelease id buildDir "Build"
    //    |> Trace.logItems "AppBuild-Output: "
    //)


    type X (connStr) =

        let loadParams seeder =
            use conn = new SqlConnection (connStr)
            openConnIfClosed conn

            let n = getNewModelDataId conn
            let m = loadSettings conn

            match ModelGenerationParams.tryGet m seeder with
            | Some m ->
                { m with modelLocationData = { m.modelLocationData with modelName = ConsecutiveName n; useDefaultModeData = true } }
                |> Some
            | None -> None


        let generateModel modelGenerationParams =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams
            let code = model.generateCode()
            printfn "... completed."
            code


        let saveModel (conn : SqlConnection) (code : list<string>) (p : ModelGenerationParams) =
            match p.modelLocationData.modelName with
            | ConsecutiveName n ->
                let sb = new StringBuilder()
                code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

                let m =
                    {
                        modelId = n
                        numberOfAminoAcids = p.numberOfAminoAcids
                        maxPeptideLength = p.maxPeptideLength
                        seedValue = p.seedValue
                        fileStructureVersion = p.fileStructureVersionNumber
                        modelData = sb.ToString()
                    }

                tryUpdateModelData conn m
            | GenerateName -> false


        let compile() = 
            failwith ""

        let copyToExecLocation() : unit = failwith ""

        // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.exited?view=netframework-4.7.2
        let runModel() : unit = failwith ""


        member fake.sucks() = 0


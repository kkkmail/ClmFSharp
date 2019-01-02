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


    type X () = 

        let loadParams() : unit = failwith ""

        let generateModel seeder =
            let m = loadSettings ClmConnectionString
            match ModelGenerationParams.tryGet m seeder with
            | Some m ->
                printfn "Creating model..."
                printfn "Starting at: %A" DateTime.Now

                let modelGenerationParams = 
                    { m with modelLocationData = { m.modelLocationData with modelName = Some ""; useDefaultModeData = true } }

                let model = ClmModel modelGenerationParams
                let code = model.generateCode()
                printfn "... completed."
                Some code
            | None ->
                printfn "Failed to generate model."
                None


        let saveModel (conn : SqlConnection) (code : list<string>) =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

            0

        let compile() : unit = failwith ""

        let copyToExecLocation() : unit = failwith ""

        // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.exited?view=netframework-4.7.2
        let runModel() : unit = failwith ""


        member fake.sucks() = 0


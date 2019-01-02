namespace ContGen

open System
open System.Diagnostics
open System.Threading

//open Fake.DotNet
//open Fake.Core
//open Fake.IO
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder = 
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

        let generateModel() : unit = failwith ""

        let saveModel() : unit = failwith ""

        let compile() : unit = failwith ""

        let copyToExecLocation() : unit = failwith ""

        // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.exited?view=netframework-4.7.2
        let runModel() : unit = failwith ""


        member fake.sucks() = 0


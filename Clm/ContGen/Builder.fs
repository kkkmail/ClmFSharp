namespace ContGen

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder = 
    // Properties
    let buildDir = "./build/"

    // Targets
    Target.create "Clean" (fun _ ->
      Shell.cleanDir buildDir
    )

    Target.create "BuildApp" (fun _ ->
      !! "src/app/**/*.csproj"
        |> MSBuild.runRelease id buildDir "Build"
        |> Trace.logItems "AppBuild-Output: "
    )


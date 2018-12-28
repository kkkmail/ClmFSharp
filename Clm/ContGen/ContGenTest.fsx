#r @"C:\GitHub\ClmFSharp\Clm\packages\FAKE.5.8.4\tools\FakeLib.dll"

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing

// Properties
let buildDir = @"C:\Temp\WTF\"

// Targets
Target.create "Clean" (fun _ ->
  Shell.cleanDir buildDir
)

Target.create "BuildApp" (fun _ ->
  //!! "src/app/**/*.csproj"
  !! @"..\SolverRunner\SolverRunner.fsproj"
    |> MSBuild.runRelease id buildDir "Build"
    |> Trace.logItems "AppBuild-Output: "
)

//Target.create "Default" (fun _ ->
//  Trace.trace "Hello World from FAKE"
//)

//open Fake.Core.TargetOperators

//"Clean"
//  ==> "BuildApp"
//  ==> "Default"

// start build
Target.runOrDefault "Default"


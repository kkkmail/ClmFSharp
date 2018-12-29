#r @"C:\GitHub\ClmFSharp\Clm\packages\FAKE.5.8.4\tools\FakeLib.dll"
#r @"C:\GitHub\ClmFSharp\Clm\packages\FAKE.5.8.4\tools\System.Reactive.dll"

open System.IO
open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators

let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

// Properties
let buildDir = @"C:\Temp\WTF\"

// Targets
Target.create "Clean" (fun _ ->
  Shell.cleanDir buildDir
)

Target.create "BuildApp" (fun _ ->
  !! @"..\SolverRunner\SolverRunner.fsproj"
    |> MSBuild.runRelease id buildDir "Build"
    |> Trace.logItems "AppBuild-Output: "
)

Target.create "Default" (fun _ ->
  Trace.trace "Hello World from FAKE"
)

open Fake.Core.TargetOperators

"Clean"
  ==> "BuildApp"
  ==> "Default"

Target.runOrDefault "Default"

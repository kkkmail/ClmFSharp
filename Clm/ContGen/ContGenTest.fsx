#r @"C:\GitHub\ClmFSharp\Clm\packages\FAKE.5.8.4\tools\FakeLib.dll"
#r @"C:\GitHub\ClmFSharp\Clm\packages\FAKE.5.8.4\tools\System.Reactive.dll"

open System.IO


//let execContext = Fake.Core.Context.FakeExecutionContext.Create false "path/to/script.fsx" []
//Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
//let execContext = Fake.Core.Context.FakeExecutionContext.Create false (Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)) []
//Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

#if !FAKE
let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing

// Properties
let buildDir = @"C:\Temp\WTF\"

printfn "1"

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

Target.create "Default" (fun _ ->
  Trace.trace "Hello World from FAKE"
)

printfn "2"

open Fake.Core.TargetOperators

"Clean"
  ==> "BuildApp"
  ==> "Default"

printfn "3"
// start build
Target.runOrDefault "Default"


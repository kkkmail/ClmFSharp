// ! Do not delete !
//open Fake.DotNet
//open Fake.Core
//open Fake.IO
//open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Fake

    // kk:20190208 - Do not delete. It was not straightforward to tweak all the parameters.
    //let compileModel modelId =
    //    let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
    //    Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
    //
    //    // Properties
    //    let buildDir = getBuildDir modelId
    //
    //    // Targets
    //    Target.create "Clean" (fun _ ->
    //      Shell.cleanDir buildDir
    //    )
    //
    //    Target.create "BuildApp" (fun _ ->
    //      [ p.buildTarget ]
    //        |> MSBuild.runRelease (fun p -> { p with Properties = [ "platform", "x64" ] } ) buildDir "Build"
    //        |> Trace.logItems "AppBuild-Output: "
    //    )
    //
    //    Target.create "Default" (fun _ ->
    //      Trace.trace "Built completed."
    //    )
    //
    //    "Clean"
    //      ==> "BuildApp"
    //      ==> "Default"
    //      |> ignore
    //
    //    Target.runOrDefault "Default"

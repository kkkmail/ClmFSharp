﻿open ContGen
open ContGenTasks
open Argu

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<ContGenArguments>(programName = "ContGen.exe")
    let results = parser.Parse argv

    match results.GetAllResults() |> ContGenTask.tryCreate with
    | Some task -> task.run()
    | None ->
        printfn "%s" (parser.PrintUsage())
        -1


open ContGen
open ContGenTasks
open ClmSys.ExitErrorCodes
open Argu

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<ContGenArguments>(programName = ContGenAppName)
        let results = parser.Parse argv

        match results.GetAllResults() |> ContGenTask.tryCreate with
        | Some task -> task.run()
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

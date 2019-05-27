open ContGenAdm.ContGenServiceResponse
open Argu
open ContGenAdm.ContGenAdmTasks
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let service = new ContGenResponseHandler()
        let parser = ArgumentParser.Create<ContGenAdmArguments>(programName = ContGenAdmAppName)
        let results = parser.Parse argv

        match results.GetAllResults() |> ContGenAdmTask.tryCreate service.contGenService with
        | Some task -> task.run()
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

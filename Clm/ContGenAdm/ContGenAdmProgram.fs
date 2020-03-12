open Argu
open ContGenAdm.AdmCommandLine
open ContGenAdm.ContGenAdmTasks
open ClmSys.ExitErrorCodes


[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<ContGenAdmArguments>(programName = ContGenAdmAppName)
        let results = (parser.Parse argv).GetAllResults()
        let i = getServiceAccessInfo results

        match results |> ContGenAdmTask.tryCreate with
        | Some task -> task.run ()
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs
    with
    | exn ->
        printfn "%s" exn.Message
        UnknownException

open Argu
open ContGenAdm.ContGenServiceResponse
open ContGenAdm.AdmCommandLine
open ContGenAdm.ContGenAdmTasks
open ClmSys.ExitErrorCodes
open ClmSys.SolverRunnerData


[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<ContGenAdmArguments>(programName = ContGenAdmAppName)
        let results = (parser.Parse argv).GetAllResults()
        let i = getServiceAccessInfo results
        let service = new ContGenResponseHandler(i)

        match results |> ContGenAdmTask.tryCreate service.contGenService with
        | Some task -> i |> ContGenSvcAccessInfo |> task.run
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs
    with
    | exn ->
        printfn "%s" exn.Message
        UnknownException

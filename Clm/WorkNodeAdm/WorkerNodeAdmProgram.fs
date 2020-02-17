open Argu
open ProgressNotifierClient.ServiceResponse
open WorkerNodeAdm.AdmCommandLine
open WorkerNodeAdm.WorkerNodeAdmTasks
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<WorkerNodeAdmArgs>(programName = WrkAdmAppName)
        let results = (parser.Parse argv).GetAllResults()
        match getServiceAccessInfo results with
        | Some i ->
            let service = new WorkerNodeResponseHandler(i)

            match WrkAdmTask.tryCreateTask service.workerNodeService i results with
            | Some task -> task.run()
            | None -> printfn "Nothing to do!"

            CompletedSuccessfully
        | None ->
            printfn "Worker node name was not specified."
            InvalidCommandLineArgs
    with
    | exn ->
        printfn "%s" exn.Message
        UnknownException

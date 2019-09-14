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
        let i = getServiceAccessInfo results
        let service = new WorkerNodeResponseHandler(i)

        match WrkAdmTask.tryCreateTask service.workerNodeService i results with
        | Some task -> task.run()
        | None -> printfn "Nothing to do!"

        CompletedSuccessfully
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

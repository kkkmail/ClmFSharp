open Argu
open Messaging.ServiceResponse
open MessagingAdm.AdmCommandLine
open MessagingAdm.MsgAdmTasks
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<MsgAdmRunArgs>(programName = MsgAdmAppName)
        let results = (parser.Parse argv).GetAllResults()
        let i = getServiceAccessInfo results
        let service = new MsgResponseHandler(i)

        match MsgAdmTask.createTask service.tryGetMessagingService results with
        | Some task -> task.run()
        | None -> printfn "Unable to create connection."

        CompletedSuccessfully
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

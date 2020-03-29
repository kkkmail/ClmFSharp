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
        let task = MsgAdmTask.createTask service results
        task.run()

        CompletedSuccessfully
    with
    | exn ->
        printfn "%s" exn.Message
        UnknownException

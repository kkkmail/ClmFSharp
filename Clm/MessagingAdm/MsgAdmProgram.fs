open Argu
open Messaging.ServiceResponse
open MessagingAdm.AdmCommandLine
open MessagingAdm.MsgAdmTasks
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<MsgAdmArguments>(programName = MsgAdmAppName)
        let results = (parser.Parse argv).GetAllResults()
        let i = getServiceAccessInfo results
        let service = new MsgResponseHandler(i)

        match results |> MsgAdmTask.tryCreate service.messagingService with
        | Some task -> task.run ()
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

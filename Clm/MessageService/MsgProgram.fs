namespace MessagingServer

open System.ServiceProcess
open Argu
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open MessagingService.ServiceTasks
open MessagingService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<MsgSvcArguArgs>(programName = MessagingProgramName)
            let results = (parser.Parse argv).GetAllResults() |> MsgSvcArgs.fromArgu convertArgs

            match MessagingServiceTask.tryCreate getParams results with
            | Some task -> task.run serviceInfo |> ignore
            | None -> ServiceBase.Run [| new MessagingWindowsService() :> ServiceBase |]

            CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

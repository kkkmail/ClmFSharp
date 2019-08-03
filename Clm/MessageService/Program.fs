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
            let parser = ArgumentParser.Create<MsgSvcArguments>(programName = MessagingProgramName)
            let results = (parser.Parse argv).GetAllResults()

            match results |> MessagingServiceTask.tryCreate with
            | Some task ->
                task.run() |> ignore
                CompletedSuccessfully
            | None ->
                ServiceBase.Run [| new MessagingWindowsService() :> ServiceBase |]
                CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

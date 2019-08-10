namespace ContGenService

open System.ServiceProcess
open Argu
open ContGenServiceInfo.ServiceInfo
open ContGenService.SvcCommandLine
open ContGenService.ContGenServiceTasks
open ContGenService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<ContGenSvcArguments>(programName = ProgramName)
            let results = (parser.Parse argv).GetAllResults()

            match ContGenServiceTask.tryCreate getParams results with
            | Some task ->
                task.run serviceInfo |> ignore
                CompletedSuccessfully
            | None ->
                ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]
                CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

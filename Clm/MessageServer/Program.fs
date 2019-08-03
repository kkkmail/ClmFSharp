﻿namespace MessagingServer

open System.ServiceProcess
open Argu
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open ContGenService.ContGenServiceTasks
open ContGenService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<SvcArguments>(programName = ProgramName)
            let results = (parser.Parse argv).GetAllResults()

            match results |> ContGenServiceTask.tryCreate with
            | Some task ->
                task.run() |> ignore
                CompletedSuccessfully
            | None ->
                ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]
                CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

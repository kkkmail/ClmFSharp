﻿namespace WorkerNodeService

open System.ServiceProcess
open Argu
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.ServiceTasks
open WorkerNodeService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<WrkNodeSvcArguments>(programName = WorkerNodeServiceProgramName)
            let results = (parser.Parse argv).GetAllResults()

            match results |> MessagingServiceTask.tryCreate with
            | Some task ->
                task.run() |> ignore
                CompletedSuccessfully
            | None ->
                ServiceBase.Run [| new WorkerNodeWindowsService() :> ServiceBase |]
                CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

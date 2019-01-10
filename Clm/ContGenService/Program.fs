namespace ContGenService

open System.ServiceProcess
open Argu
open ContGenServiceInfo.ServiceInfo
open ContGenService.SvcCommandLine
open ContGenService.ContGenServiceTasks
open ContGenService.WindowsService

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<SvcArguments>(programName = ProgramName)
            let results = parser.Parse argv

            match results.GetAllResults() |> ContGenServiceTask.tryCreate with
            | Some task ->
                task.run() |> ignore
                0
            | None ->
                ServiceBase.Run
                    [|
                        //new ProgressNotifierWindowsService() :> ServiceBase
                        new ContGenWindowsService() :> ServiceBase
                    |]

                0
        with
            | exn ->
                printfn "%s" exn.Message
                -1

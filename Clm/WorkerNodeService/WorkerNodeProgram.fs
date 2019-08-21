namespace WorkerNodeService

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
            let saveSettings() =
                let p = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
                let r = (p.Parse argv).GetAllResults()
                saveSettings r


            let parser = ArgumentParser.Create<WorkerNodeServiceArguArgs>(programName = WorkerNodeServiceProgramName)
            let results = (parser.Parse argv).GetAllResults() |> WorkerNodeServiceArgs.fromArgu convertArgs

            match WorkerNodeServiceTask.tryCreate getParams results saveSettings with
            | Some task -> task.run serviceInfo |> ignore
            | None -> ServiceBase.Run [| new WorkerNodeWindowsService() :> ServiceBase |]

            CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

namespace PartitionerService

open System.ServiceProcess
open Argu
open PartitionerServiceInfo.ServiceInfo
open PartitionerService.SvcCommandLine
open PartitionerService.PartitionerServiceTasks
open PartitionerService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<PartitionerServiceArguArgs>(programName = PartitionerServiceProgramName)
            let results = (parser.Parse argv).GetAllResults() |> PartitionerServiceArgs.fromArgu convertArgs

            match PartitionerServiceTask.tryCreate getParams results with
            | Some task -> task.run serviceInfo |> ignore
            | None -> ServiceBase.Run [| new PartitionerWindowsService() :> ServiceBase |]

            CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException

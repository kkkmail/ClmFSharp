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
            failwith ""
        //    let parser = ArgumentParser.Create<SvcArguments>(programName = ProgramName)
        //    let results = (parser.Parse argv).GetAllResults()

        //    match results |> ContGenServiceTask.tryCreate with
        //    | Some task ->
        //        task.run() |> ignore
        //        CompletedSuccessfully
        //    | None ->
        //        ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]
        //        CompletedSuccessfully

        with
            | exn ->
                printfn "%s" exn.Message
                UnknownException
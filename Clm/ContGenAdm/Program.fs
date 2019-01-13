open ContGenAdm.ContGenServiceResponse
open System.Threading
open Argu
open ContGenAdm.ContGenAdmTasks

[<EntryPoint>]
let main argv =
    try
        let service = new ContGenResponseHandler()
        let parser = ArgumentParser.Create<ContGenAdmArguments>(programName = "ContGenAdm.exe")
        let results = parser.Parse argv

        match results.GetAllResults() |> ContGenAdmTask.tryCreate service.contGenService with
        | Some task -> task.run()
        | None ->
            printfn "%s" (parser.PrintUsage())
            -1
    with
        | exn ->
            printfn "%s" exn.Message
            -1

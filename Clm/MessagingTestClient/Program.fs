namespace MessagingServer

open Argu
open Messaging.MsgCliCommandLine
open MessagingTestClient.MessagingTestClientTask
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<MessagingClientRunArgs>(programName = "MessagingTestClient.exe")
            let results = (parser.Parse argv).GetAllResults()

            match results |> MessagingTestClientTask.tryCreate with
            | Some task ->
                task.run() |> ignore
                CompletedSuccessfully
            | None ->
                printfn "Nothing to do!"
                CompletedSuccessfully

        with
        | e ->
            printfn "%s" e.Message
            UnknownException

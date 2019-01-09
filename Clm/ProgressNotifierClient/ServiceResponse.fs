namespace ProgressNotifierClient

open System
open ProgressNotifier.Interfaces

module ServiceResponse =
    type ResponseHandler () =
        let url = "tcp://localhost:12345/ContGenService"
        let service : IProgressNotifier = Activator.GetObject (typeof<IProgressNotifier>, url) :?> IProgressNotifier
        member this.progressNotifierService : IProgressNotifier = service

        static member tryCreate() =
            try
                ResponseHandler () |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

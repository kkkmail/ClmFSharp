namespace ProgressNotifierClient

open System
open ProgressNotifierServiceInfo.ServiceInfo

module ServiceResponse =
    type ResponseHandler () =
        let url = "tcp://localhost:" + ProgressNotifierServicePort + "/" + ProgressNotifierServiceName
        let service : IProgressNotifier = Activator.GetObject (typeof<IProgressNotifier>, url) :?> IProgressNotifier
        member this.progressNotifierService : IProgressNotifier = service

        static member tryCreate() =
            try
                ResponseHandler () |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

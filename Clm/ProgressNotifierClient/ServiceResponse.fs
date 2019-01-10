namespace ProgressNotifierClient

open System
open ContGenServiceInfo.ServiceInfo

module ServiceResponse =
    type ResponseHandler () =
        let url = "tcp://localhost:" + (ContGenServicePort.ToString()) + "/" + ContGenServiceName
        let service : IContGenService = Activator.GetObject (typeof<IContGenService>, url) :?> IContGenService

        //member this.progressNotifierService : IProgressNotifier = service :> IProgressNotifier
        member this.progressNotifierService : IContGenService = service

        static member tryCreate() =
            try
                ResponseHandler () |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

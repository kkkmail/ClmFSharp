namespace ProgressNotifierClient

open System
open ContGenServiceInfo.ServiceInfo

module ContGenServiceResponse =
    type ContGenResponseHandler () =
        let url = "tcp://localhost:" + ContGenServicePort + "/" + ContGenServiceName
        let service : IContGenService = Activator.GetObject (typeof<IContGenService>, url) :?> IContGenService
        member this.contGenService : IContGenService = service

        static member tryCreate() =
            try
                ContGenResponseHandler () |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

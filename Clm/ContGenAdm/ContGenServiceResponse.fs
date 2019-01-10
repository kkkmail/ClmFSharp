namespace ContGenAdm

open System
open ContGenServiceInfo.ServiceInfo

module ContGenServiceResponse =
    type ContGenResponseHandler () =
        let url = "tcp://localhost:" + (ContGenServicePort.ToString()) + "/" + ContGenServiceName
        let service : IContGenService = Activator.GetObject (typeof<IContGenService>, url) :?> IContGenService

        member this.contGenService : IContGenService = service

        static member tryCreate() =
            try
                ContGenResponseHandler () |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

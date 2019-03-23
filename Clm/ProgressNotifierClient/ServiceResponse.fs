namespace ProgressNotifierClient

open System
open ContGenServiceInfo.ServiceInfo

module ServiceResponse =

    type ResponseHandler (address, port) =
        let url = getServiceUrlImpl address port ContGenServiceName
        let service = Activator.GetObject (typeof<IContGenService>, url) :?> IContGenService

        member this.progressNotifierService : IContGenService = service

        static member tryCreate address port =
            try
                ResponseHandler (address, port) |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

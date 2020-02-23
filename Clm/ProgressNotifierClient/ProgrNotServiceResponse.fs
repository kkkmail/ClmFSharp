namespace ProgressNotifierClient

open System
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData

module ServiceResponse =

    type WorkerNodeResponseHandler (w : NodeServiceAccessInfo) =
        let service = Activator.GetObject (typeof<IWorkerNodeService>, w.nodeServiceAccessInfo.serviceUrl) :?> IWorkerNodeService
        let updateLocalProgressImpl p = service.updateLocalProgress p

        member __.updateLocalProgress p = updateLocalProgressImpl p
        member __.workerNodeService = service

        static member tryCreate i =
            try
                WorkerNodeResponseHandler i |> Some
            with
            | exn ->
                printfn "Exception occurred: %s." exn.Message
                None

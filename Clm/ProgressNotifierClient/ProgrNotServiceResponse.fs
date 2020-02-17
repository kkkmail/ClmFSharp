namespace ProgressNotifierClient

open System
open ClmSys.GeneralData
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open ClmSys.SolverRunnerData

module ServiceResponse =

    type WorkerNodeResponseHandler (w : WorkerNodeServiceAccessInfo) =
        do printfn "WorkerNodeResponseHandler: workerNodeServiceAccessInfo: %A" w.workerNodeServiceAccessInfo
        let service =
            Activator.GetObject (typeof<IWorkerNodeService>, w.workerNodeServiceAccessInfo.serviceUrl) :?> IWorkerNodeService

        member __.workerNodeService = service


    type SolverRunnerProgressNotifier =
        | ContGenNotifier of IContGenService
        | WorkerNodeNotifier of IWorkerNodeService


    type ResponseHandler (i : SolverRunnerAccessInfo) =
        let service =
            match i with
            | ContGenSvcAccessInfo c ->
                Activator.GetObject (typeof<IContGenService>, c.contGenServiceAccessInfo.serviceUrl) :?> IContGenService |> ContGenNotifier
            | WorkerNodeSvcAccessInfo w ->
                Activator.GetObject (typeof<IWorkerNodeService>, w.wrkNodeServiceAccessInfo.serviceUrl) :?> IWorkerNodeService |> WorkerNodeNotifier

        let updateLocalProgressImpl p =
            match service with
            | ContGenNotifier s ->
                //s.updateLocalProgress p
                failwith "ContGenNotifier.updateLocalProgress is not implemented."
            | WorkerNodeNotifier w -> w.updateLocalProgress p

        member __.updateLocalProgress p = updateLocalProgressImpl p

        static member tryCreate i =
            try
                ResponseHandler i |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

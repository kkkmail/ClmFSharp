namespace WorkerNodeAdm

open WorkerNodeAdm.AdmCommandLine
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData

module WorkerNodeAdmTasks =

    /// TODO kk:20190831 - Make it more general.
    type  WrkAdmTask =
        | ConfigureWorkerNodeTask of IWorkerNodeService * list<WorkerNodeConfigParam>

        member task.run () =
            match task with
            | ConfigureWorkerNodeTask (s, p) -> p |> List.map s.configure |> ignore

        static member private tryCreateConfigureTask s (i : WorkerNodeServiceAccessInfo) (p : list<WorkerNodeAdmArgs>) =
            p |> List.tryPick (fun e -> match e with | ConfigureWrkService -> (s, [ WorkerNumberOfSores i.noOfCores ]) |> ConfigureWorkerNodeTask |> Some | _ -> None)

        static member tryCreateTask s (i : WorkerNodeServiceAccessInfo) (p : list<WorkerNodeAdmArgs>) =
            [
                    WrkAdmTask.tryCreateConfigureTask
            ]
            |> List.tryPick (fun e -> e s i p)

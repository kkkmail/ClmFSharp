namespace WorkerNodeAdm

open WorkerNodeAdm.AdmCommandLine
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open System.Threading

module WorkerNodeAdmTasks =

    let monitor (service : IWorkerNodeService) p =
        let i = 30_000
            //match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            //| Some i -> i * 1_000
            //| None -> 30_000

        while true do
            try
                getServiceState service p
            with
                | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(i)
        0


    type  WrkAdmTask =
        | ConfigureWorkerNodeTask of IWorkerNodeService * list<WorkerNodeConfigParam>
        | MonitorWorkerNodeTask of IWorkerNodeService * WorkerNodeMonitorParam

        member task.run () =
            match task with
            | ConfigureWorkerNodeTask (s, p) -> p |> List.map s.configure |> ignore
            | MonitorWorkerNodeTask (s, p)-> monitor s p |> ignore

        static member private tryCreateConfigureTask s (i : WorkerNodeServiceAccessInfo) (p : list<WorkerNodeAdmArgs>) =
            p |> List.tryPick (fun e -> match e with | ConfigureWrkService -> (s, [ WorkerNumberOfSores i.noOfCores ]) |> ConfigureWorkerNodeTask |> Some | _ -> None)

        static member private tryCreateMonitorTask s (i : WorkerNodeServiceAccessInfo) (p : list<WorkerNodeAdmArgs>) =
            p |> List.tryPick (fun e -> match e with | MonitorWrkService -> (s, DummyWrkMonitorParam 0) |> MonitorWorkerNodeTask |> Some | _ -> None)

        static member tryCreateTask s (i : WorkerNodeServiceAccessInfo) (p : list<WorkerNodeAdmArgs>) =
            [
                    WrkAdmTask.tryCreateMonitorTask
                    WrkAdmTask.tryCreateConfigureTask
            ]
            |> List.tryPick (fun e -> e s i p)

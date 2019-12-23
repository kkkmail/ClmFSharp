namespace WorkerNodeServiceInfo

open ClmSys.VersionInfo
open ContGenServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels.Tcp
open ClmSys.GeneralData
open System.Threading
open System

module ServiceInfo =

    let WorkerNodeServiceName = "WorkerNodeService" + " - " + versionNumberValue.value
    let WorkerNodeServiceProgramName = "WorkerNodeService.exe"


    type WrkNodeShutDownInfo =
        {
            wrkNodeTcpChannel : TcpChannel
        }


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type RunnerState =
        {
            runnerRemoteProcessId : RemoteProcessId
            progress : TaskProgress
            lastUpdated : DateTime
        }


    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<LocalProcessId, RunnerState>
            numberOfCores : int
        }


    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam of int


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | WrkNodeState of WorkerNodeRunnerState

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node"
            | WrkNodeState s ->
                let toString acc ((LocalProcessId k), (v : RunnerState)) =
                    (if acc = EmptyString then acc else acc + ", ") +
                    (sprintf "        LP: %A, RP %A, P:%A, T: %s\n" k v.runnerRemoteProcessId v.progress (v.lastUpdated.ToString("yyyy-MM-dd.HH:mm")))

                let x =
                    match s.runningWorkers |> Map.toList |> List.sortBy (fun (_, r) -> r.progress) |> List.fold toString EmptyString with
                    | EmptyString -> "[]"
                    | s -> "\n    [\n" + s + "    ]"
                sprintf "Running: %s\nCount: %A, cores: %A" x s.runningWorkers.Count s.numberOfCores


    type IWorkerNodeService =
        abstract updateLocalProgress : LocalProgressUpdateInfo -> unit
        abstract configure : WorkerNodeConfigParam -> unit
        abstract monitor : WorkerNodeMonitorParam -> WorkerNodeMonitorResponse

        /// To check if service is working.
        abstract ping : unit -> unit


    let mutable private callCount = -1


    let getServiceState (service : IWorkerNodeService) p =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting worker node state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let state = service.monitor p
                printfn "...state at %s =\n%s\n\n" (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) (state.ToString())
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore



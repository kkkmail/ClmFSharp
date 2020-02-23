namespace WorkerNodeServiceInfo

open ClmSys.VersionInfo
open ContGenServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels.Tcp
open ClmSys.GeneralData
open System.Threading
open System
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenData
open ClmSys.ContGenPrimitives

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
            started : DateTime
            lastUpdated : DateTime
        }

        override r.ToString() =
            let s = (DateTime.Now - r.started).ToString("d\.hh\:mm")

            let estCompl =
                match r.progress.estimateEndTime r.started with
                | Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
                | None -> EmptyString

            sprintf "RP: %A; T: %s;%s %A" r.runnerRemoteProcessId.value s estCompl r.progress

        static member defaultValue r =
            {
                runnerRemoteProcessId = r
                progress = TaskProgress.NotStarted
                started = DateTime.Now
                lastUpdated = DateTime.Now
            }


    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<LocalProcessId, RunnerState>
            numberOfWorkerCores : int
        }

    type WorkerNodeRunnerResult = StateWithResult<WorkerNodeRunnerState>


    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam of int


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | ErrorOccurred of ClmError
        | WrkNodeState of WorkerNodeRunnerState

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node"
            | WrkNodeState s ->
                let toString acc ((LocalProcessId k), (v : RunnerState)) =
                    acc + (sprintf "        LP: %A; %s; L: %s\n" k (v.ToString()) (v.lastUpdated.ToString("yyyy-MM-dd.HH:mm")))
        
                let x =
                    match s.runningWorkers |> Map.toList |> List.sortBy (fun (_, r) -> r.progress) |> List.fold toString EmptyString with
                    | EmptyString -> "[]"
                    | s -> "\n    [\n" + s + "    ]"
                sprintf "Running: %s\nCount: %A, cores: %A" x s.runningWorkers.Count s.numberOfWorkerCores
            | ErrorOccurred e -> "Error occurred: " + e.ToString()


    type IWorkerNodeService =
        abstract updateLocalProgress : LocalProgressUpdateInfo -> UnitResult
        abstract configure : WorkerNodeConfigParam -> UnitResult
        abstract monitor : WorkerNodeMonitorParam -> WorkerNodeMonitorResponse

        /// To check if service is working.
        abstract ping : unit -> UnitResult


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

namespace WorkerNodeServiceInfo

open System.Runtime.Remoting.Channels.Tcp
open ClmSys.GeneralData
open System.Threading
open System
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodeData

module ServiceInfo =

    let workerNodeServiceProgramName = "WorkerNodeService.exe"


    type WrkNodeShutDownInfo =
        {
            wrkNodeTcpChannel : TcpChannel
        }


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type RunnerState =
        {
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

            sprintf "T: %s;%s %A" s estCompl r.progress


    type RunnerStateWithCancellation =
        {
            runnerState : RunnerState
            cancellationTokenSource : CancellationTokenSource
        }


    type WorkerNodeRunnerMonitorState =
        {
            workers : Map<RunQueueId, RunnerState>
            noOfWorkerCores : int
        }


    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<RunQueueId, RunnerStateWithCancellation>
            numberOfWorkerCores : int
        }

        member w.toWorkerNodeRunnerMonitorState() =
            {
                workers = w.runningWorkers |> Map.map (fun _ e -> e.runnerState)
                noOfWorkerCores = w.numberOfWorkerCores
            }


    type WorkerNodeRunnerResult = StateWithResult<WorkerNodeRunnerState>


    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam of int


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | ErrorOccurred of ClmError
        | WrkNodeState of WorkerNodeRunnerMonitorState

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node"
            | WrkNodeState s ->
                let toString acc ((RunQueueId k), (v : RunnerState)) =
                    acc + (sprintf "        Q: %A; %s; L: %s\n" k (v.ToString()) (v.lastUpdated.ToString("yyyy-MM-dd.HH:mm")))
        
                let x =
                    match s.workers |> Map.toList |> List.sortBy (fun (_, r) -> r.progress) |> List.fold toString EmptyString with
                    | EmptyString -> "[]"
                    | s -> "\n    [\n" + s + "    ]"
                sprintf "Running: %s\nCount: %A, cores: %A" x s.workers.Count s.noOfWorkerCores
            | ErrorOccurred e -> "Error occurred: " + e.ToString()


    type IWorkerNodeService =
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


    type WorkerNodeResponseHandler (w : WorkerNodeServiceAccessInfo) =
        let service = Activator.GetObject (typeof<IWorkerNodeService>, w.serviceUrl) :?> IWorkerNodeService
        member __.workerNodeService = service

        static member tryCreate i =
            try
                WorkerNodeResponseHandler i |> Some
            with
            | exn ->
                printfn "Exception occurred: %s." exn.Message
                None

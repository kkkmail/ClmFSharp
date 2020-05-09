namespace WorkerNodeServiceInfo

open ClmSys.GeneralData
open System.Threading
open System
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodeData
open System.ServiceModel
open ClmSys.Wcf
open ClmSys.WorkerNodeErrors

module ServiceInfo =

    let workerNodeServiceProgramName = "WorkerNodeService.exe"


    [<Literal>]
    let WorkerNodeWcfServiceName = "WorkerNodeWcfService"


    type WrkNodeWcfSvcShutDownInfo =
        {
            wrkNodeServiceHost : ServiceHost
        }


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type RunnerState =
        {
            progress : TaskProgress
            started : DateTime
            lastUpdated : DateTime
        }

        static member defaultValue =
            {
                progress = NotStarted
                started = DateTime.Now
                lastUpdated = DateTime.Now
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
            cancellationRequested : bool
        }

        static member defaultValue =
            {
                runnerState = RunnerState.defaultValue
                cancellationRequested = false
            }


    type WorkerNodeRunnerMonitorState =
        {
            workers : Map<RunQueueId, RunnerState>
            noOfWorkerCores : int
        }


    type WorkerNodeState =
        | NotStartedWorkerNode
        | StartedWorkerNode

    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<RunQueueId, RunnerStateWithCancellation>
            numberOfWorkerCores : int
            workerNodeState : WorkerNodeState
        }

        member w.toWorkerNodeRunnerMonitorState() =
            {
                workers = w.runningWorkers |> Map.map (fun _ e -> e.runnerState)
                noOfWorkerCores = w.numberOfWorkerCores
            }


    type WorkerNodeRunnerResult = StateWithResult<WorkerNodeRunnerState>


    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | ErrorOccurred of ClmError
        | WrkNodeState of WorkerNodeRunnerMonitorState

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node."
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
        abstract monitor : WorkerNodeMonitorParam -> ClmResult<WorkerNodeMonitorResponse>

        /// To check if service is working.
        abstract ping : unit -> UnitResult


    let mutable private callCount = -1


    let getServiceState (service : IWorkerNodeService) p =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting worker node state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let state = service.monitor p

                match state with
                | Ok r -> printfn "...state at %s =\n%s\n\n" (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) (r.ToString())
                | Error e -> printfn "...state at %s =\n%A\n\n" (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) e
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = WorkerNodeWcfServiceName)>]
    type IWorkerNodeWcfService =

        [<OperationContract(Name = "configure")>]
        abstract configure : q:byte[] -> byte[]

        [<OperationContract(Name = "monitor")>]
        abstract monitor : q:byte[] -> byte[]

        [<OperationContract(Name = "ping")>]
        abstract ping : q:byte[] -> byte[]


    /// Low level WCF messaging client.
    type WorkerNodeResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IWorkerNodeWcfService> url

        let configureWcfErr e = e |> ConfigureWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let monitorWcfErr e = e |> MonitorWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let pingWcfErr e = e |> PingWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr

        let configureImpl p = tryCommunicate tryGetWcfService (fun service -> service.configure) configureWcfErr p
        let monitorImpl p = tryCommunicate tryGetWcfService (fun service -> service.monitor) monitorWcfErr p
        let pingImpl() = tryCommunicate tryGetWcfService (fun service -> service.ping) pingWcfErr ()

        interface IWorkerNodeService with
            member _.configure p = configureImpl p
            member _.monitor p = monitorImpl p
            member _.ping() = pingImpl()

        new (i : WorkerNodeServiceAccessInfo) = WorkerNodeResponseHandler(i.wcfServiceUrl)

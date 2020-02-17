namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.AsyncRunErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ServiceProxy.AsyncRunProxy

module AsyncRun =

    let private toError g f = f |> g |> AsyncRunErr |> Error
    let private addError g f e = ((f |> g |> AsyncRunErr) + e) |> Error


    //type AsyncRunnerData =
    //    {
    //        generate : unit -> ListResult<RunInfo>
    //        getQueue : unit -> ListResult<RunInfo>
    //        removeFromQueue : RunQueueId -> UnitResult
    //        runModel : ModelDataId -> ModelCommandLineParam -> ClmResult<RunInfo>
    //        usePartitioner : bool
    //    }


    type AsyncRunnerData =
        {
            removeFromQueue : RunQueueId -> UnitResult
            runModel : ModelDataId -> ModelCommandLineParam -> ClmResult<RunInfo>
        }


    //type AsyncRunnerState =
    //    {
    //        //running : Map<ProcessId, RunningProcessInfo>
    //        //queue : list<RunInfo>
    //        //runLimit : int
    //        //maxQueueLength : int
    //        workState : WorkState
    //        messageCount : int64
    //        minUsefulEe : MinUsefulEe
    //        usePartitioner : bool
    //        lastRunError : ClmError option
    //    }

    type AsyncRunnerState =
        {
            messageCount : int64
            minUsefulEe : MinUsefulEe
            lastRunError : ClmError option
        }

    //    //member state.runningCount = state.running.Count
    //    //member state.runningQueue = state.running |> Map.toList |> List.map (fun (_, v) -> v.progressUpdateInfo.processStartedInfo.runningProcessData.runQueueId) |> Set.ofList
    //
    //    static member defaultValue u =
    //        {
    //            //running = Map.empty
    //            //queue = []
    //            //runLimit = if u then 0 else Environment.ProcessorCount
    //            //maxQueueLength = 4
    //            workState = NotInitialized
    //            messageCount = 0L
    //            minUsefulEe = MinUsefulEe DefaultMinEe
    //            usePartitioner = u
    //            lastRunError = None
    //        }
    //
    //    override s.ToString() =
    //        let q = s.queue |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", "
    //        let r =
    //            s.running
    //            |> Map.toList
    //            |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.progressUpdateInfo.processStartedInfo.runningProcessData.modelDataId e.progressUpdateInfo.processStartedInfo.processId e.started e.progressUpdateInfo.progress) |> String.concat ", "
    //        sprintf "{ running: [%s]; queue: [%s]; runLimit = %A; runningCount: %A; workState: %A; minUsefulEe: %A; lastRunError = %A }" r q s.runLimit s.runningCount s.workState s.minUsefulEe s.lastRunError
    //
    //    member s.isShuttingDown =
    //        match s.workState with
    //        | NotInitialized | Idle | CanGenerate | Generating -> false
    //        | ShuttingDown -> true


    //type RunnerMessage =
    //    | QueueStarting
    //    | ConfigureService of AsyncRunner * ContGenConfigParam
    //    | ProgressUpdated of ProgressUpdateInfo
    //    | GetState of AsyncReplyChannel<AsyncRunnerState>
    //    | GenerationStarted of AsyncRunner
    //    | GenerationCompleted of list<RunInfo>
    //    | RunModel of ModelDataId * ModelCommandLineParam
    //
    //    override m.ToString() =
    //        let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", ") + "]"
    //
    //        match m with
    //        | QueueStarting _ -> "QueueStarting"
    //        | ConfigureService _ -> "ConfigureService"
    //        | ProgressUpdated p -> "ProgressUpdated: " + (p.ToString())
    //        | GetState _ -> "GetState"
    //        | GenerationStarted _ -> "GenerationStarted"
    //        | GenerationCompleted r -> "GenerationCompleted: " + (toStr r)
    //        | RunModel _ -> "RunModel"


    //type AsyncRunnerMessage =
    //    | X


    //type OnStartRunProxy =
    //    {
    //        removeFromQueue : RunQueueId -> UnitResult
    //    }


    //let onStartRun (proxy : OnStartRunProxy) (s : AsyncRunnerState) : StateWithResult<AsyncRunnerState> =
    //    let addError = addError OnStartRunErr
    //    let updateQueue t ((g : AsyncRunnerState), f) = { g with queue = t }, f
    //
    //    let start (g : AsyncRunnerState) e f =
    //        let x = e.run e.processToStartInfo
    //
    //        match x with
    //        | Ok (StartedSuccessfully (r, fo)) ->
    //            { g with running = g.running.Add(r.processId, r.toRunningProcessInfo())}, toUnitResult fo
    //        |Error e -> g, addError FailedToStartErr e
    //        | Ok (AlreadyCompleted fo) ->
    //            let r = proxy.removeFromQueue e.processToStartInfo.runQueueId
    //            g, toUnitResult fo |> combineUnitResults r
    //
    //    let w() =
    //        if s.runningCount < s.runLimit
    //        then
    //            let run, queue = s.queue |> List.splitAt (min s.queue.Length (max 0 (s.runLimit - s.runningCount)))
    //
    //            let w, result =
    //                run
    //                |> List.fold (fun (acc, f) e -> start acc e f) (s, Ok())
    //                |> updateQueue queue
    //            w, result
    //        else s, Ok()
    //
    //    match s.workState with
    //    | NotInitialized -> s, Ok()
    //    | Idle -> w()
    //    | CanGenerate -> w()
    //    | Generating -> w()
    //    | ShuttingDown -> s, Ok()


    //let onQueueObtained (s : AsyncRunnerState) p =
    //    { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) }
    //    |> onStartRun


    //type OnQueueStartingProxy =
    //    {
    //        getQueue : unit -> ListResult<RunInfo>
    //        onQueueObtained : int
    //    }

    //let onQueueStarting (proxy : OnQueueStartingProxy) s =
    //    let w t =
    //        let x = proxy.getQueue()
    //        x |> onQueueObtained { s with workState = t }
    //
    //    match s.workState with
    //    | NotInitialized -> w CanGenerate
    //    | Idle | CanGenerate | Generating -> w s.workState
    //    | ShuttingDown -> s


    //let cancelProcessImpl i =
    //    try
    //        match i with
    //        | LocalProcess (LocalProcessId a) -> (Process.GetProcessById a).Kill()
    //        | RemoteProcess a -> generatorInfo.logger.logErr (sprintf "%s: Cannot yet cancel remote process: %A." cancelProcessImplName a)
    //        true
    //    with
    //    | _ -> false


    //type OnConfigureServiceProxy =
    //    {
    //        generationStarted : unit -> UnitResult
    //    }


    //let onConfigureService (proxy : OnConfigureServiceProxy) (s : AsyncRunnerState) (p : ContGenConfigParam) =
    //    match p with
    //    | SetToIdle -> { s with workState = Idle }
    //    | SetToCanGenerate ->
    //        proxy.generationStarted()
    //        { s with workState = CanGenerate }
    //    | RequestShutDown b ->
    //        match b with
    //        | false ->
    //            s.running
    //            |> Map.toList
    //            |> List.map (fun (i, _) -> a.configureService (CancelTask i))
    //            |> ignore
    //
    //            { s with workState = ShuttingDown; queue = [] }
    //        | true -> { s with workState = ShuttingDown }
    //    | SetRunLimit v ->
    //        match s.usePartitioner with
    //        | false -> { s with runLimit = max 1 (min v Environment.ProcessorCount) }
    //        | true -> { s with runLimit = max 0 v }
    //        |> onStartRun
    //    | CancelTask i ->
    //        match cancelProcessImpl i with
    //        | true -> { s with running = s.running.tryRemove i }
    //        | false -> s
    //    | SetMinUsefulEe ee -> { s with minUsefulEe = MinUsefulEe ee }


    //let updateQueue (s : AsyncRunnerState) r =
    //    let x = s.runningQueue
    //    { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }


    //let onRunModel (s : AsyncRunnerState) (i : ModelDataId) p =
    //    match generatorInfo.runModel i p with
    //    | Some r ->
    //        let x = s.runningQueue
    //        let filter e = x.Contains e.processToStartInfo.runQueueId |> not
    //        { s with queue = s.queue @ [ r ] |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter filter }
    //    | None -> s


    //let  onProgressUpdated (s : AsyncRunnerState) (p : ProgressUpdateInfo) =
    //    printfn "AsyncRunner.onProgressUpdated: %A" p
    //    let removeFromQueue() = generatorInfo.removeFromQueue p.processStartedInfo.runningProcessData.runQueueId

    //    let onCompleted g =
    //        printfn "AsyncRunner.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.processStartedInfo.processId
    //        onStartRun { g with running = g.running.tryRemove p.processStartedInfo.processId }

    //    let onFailed g f =
    //        let i = p.processStartedInfo.runningProcessData.modelDataId
    //        let c = p.processStartedInfo.runningProcessData.commandLineParams
    //        onRunModel { g with running = g.running.tryRemove p.processStartedInfo.processId; lastRunError = Some f } i c

    //    match s.running.TryFind p.processStartedInfo.processId with
    //    | Some e ->
    //        match p.progress with
    //        | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, { e with progressUpdateInfo = { e.progressUpdateInfo with progress = p.progress } })}
    //        | Completed ->
    //            removeFromQueue()
    //            onCompleted s
    //        | Failed f -> onFailed s f
    //    | None ->
    //        printfn "AsyncRunner.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.processStartedInfo.processId
    //        match p.progress with
    //        | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, p.toRunningProcessInfo()) }
    //        | Completed ->
    //            removeFromQueue()
    //            s
    //        | Failed f -> onFailed s f


    //let onGetState s (r : AsyncReplyChannel<AsyncRunnerState>) =
    //    r.Reply s
    //    s


    //let onGenerationStarted (s : AsyncRunnerState) (a : AsyncRunner) =
    //    match s.workState with
    //    | NotInitialized | Idle | Generating | ShuttingDown -> s
    //    | CanGenerate ->
    //        let generate() = generatorInfo.generate() |> a.generationCompleted

    //        if s.queue.Length <= s.maxQueueLength
    //        then
    //            printfn "%s: s.queue.Length = %A. Starting generating..." onGenerationStartedName s.queue.Length
    //            generate |> toAsync |> Async.Start
    //            { s with workState = Generating }
    //        else s


    //let onGenerationCompleted (s : AsyncRunnerState) r =
    //    let w t = { updateQueue s r with workState = t } |> timed onGenerationCompletedName onStartRun

    //    match s.workState with
    //    | Idle | CanGenerate -> w s.workState
    //    | Generating -> w CanGenerate
    //    | NotInitialized | ShuttingDown -> s


    //type AsyncRunner (generatorInfo : GeneratorInfo) =

    //    let messageLoop =
    //        MailboxProcessor.Start(fun u ->
    //            let rec loop (sInput : AsyncRunnerState) =
    //                async
    //                    {
    //                        let! m = u.Receive()
    //                        let s = { sInput with messageCount = sInput.messageCount + 1L }
    //                        printfn "%s: s = %s, m = %A." className (s.ToString()) (m.ToString())

    //                        match m with
    //                        | QueueStarting -> return! onQueueStarting s |> loop
    //                        | ConfigureService (a, p) -> return! onConfigureService s a p |> loop
    //                        | ProgressUpdated p -> return! onProgressUpdated s p |> loop
    //                        | GetState r -> return! onGetState s r |> loop
    //                        | GenerationStarted a -> return! onGenerationStarted s a |> loop
    //                        | GenerationCompleted r -> return! onGenerationCompleted s r |> loop
    //                        | RunModel (m, p) -> return! onRunModel s m p |> loop
    //                    }

    //            AsyncRunnerState.defaultValue generatorInfo.usePartitioner |> loop
    //            )

    //    member __.queueStarting () : unit = QueueStarting |> messageLoop.Post
    //    member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
    //    member __.progressUpdated p = ProgressUpdated p |> messageLoop.Post
    //    member __.getState () = messageLoop.PostAndReply GetState
    //    member this.generationStarted () : unit = GenerationStarted this |> messageLoop.Post
    //    member private __.generationCompleted (r : list<RunInfo>) : unit = GenerationCompleted r |> messageLoop.Post
    //    member __.runModel(m, p) = RunModel (m, p) |> messageLoop.Post
    //    member this.start() = SetToCanGenerate |> this.configureService
    //    member this.stop() = SetToIdle |> this.configureService

    type AsyncRunner (i : AsyncRunnerData) =


        member x.y = 0

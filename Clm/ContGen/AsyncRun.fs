namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open System.Threading

module AsyncRun =

    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : RunQueueId -> unit
            //maxQueueLength : int
            runModel : ModelDataId -> ModelCommandLineParam -> RunInfo option
            usePartitioner : bool
        }


    //type AsyncRunnerHelper =
    //    {
    //        cancelProcess : ProcessId -> bool
    //        generate : unit -> unit
    //        tryAcquireGenerating : unit -> bool
    //        releaseGenerating : unit -> unit
    //        startGenerate : unit -> unit
    //        startRun : unit -> unit // Requests to run model(s).
    //        tryAcquireStartingModel : unit -> bool
    //        releaseStartingModel : unit -> unit
    //        startModel : RunInfo -> unit // Schedules a model run.
    //        getQueue : unit -> unit
    //        removeFromQueue : RunQueueId -> unit
    //        runModel : ModelDataId -> ModelCommandLineParam -> unit
    //    }


    type AsyncRunnerState =
        {
            running : Map<ProcessId, RunningProcessInfo>
            queue : list<RunInfo>
            runLimit : int
            //maxQueueLength : int
            workState : WorkState
            messageCount : int64
            minUsefulEe : MinUsefulEe
            usePartitioner : bool
        }

        member state.runningCount = state.running.Count

        member state.runningQueue =
            state.running |> Map.toList |> List.map (fun (_, v) -> v.runningQueueId) |> List.choose id |> Set.ofList

        static member defaultValue u =
            {
                running = Map.empty
                queue = []
                runLimit = if u then 0 else Environment.ProcessorCount
                //maxQueueLength = 4
                workState = CanGenerate
                messageCount = 0L
                minUsefulEe = MinUsefulEe DefaultMinEe
                usePartitioner = u
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.runningModelId e.runningProcessId e.started e.progress) |> String.concat ", "
            sprintf "{ running: [%s]; queue: [%s]; runLimit = %A; runningCount: %A; workState: %A; minUsefulEe: %A }" r q s.runLimit s.runningCount s.workState s.minUsefulEe

        member s.isShuttingDown =
            match s.workState with
            | Idle | CanGenerate -> false
            | ShuttingDown -> true

        //member s.onProcessStarted h (x : ProcessStartedInfo) =
        //    let w() =
        //        h.releaseStartingModel()
        //        h.startRun()
        //        { s with running = s.running.Add(x.processId, x.runningProcessInfo) }

        //    match s.workState with
        //    | Idle -> w()
        //    | CanGenerate ->
        //        h.startGenerate()
        //        w()
        //    | ShuttingDown ->
        //        h.releaseStartingModel()
        //        s

        //member s.onStarted p =
        //    printfn "Started: %A" p
        //    { s with running = s.running.Add(p.processId, p.runningProcessInfo)}

        //member s.runModel h i p =
        //    h.runModel i p
        //    s


    type RunnerMessage =
        | QueueStarting of AsyncRunner
        | QueueObtained of AsyncRunner * list<RunInfo>
        | ConfigureService of AsyncRunner * ContGenConfigParam
        | ProgressUpdated of AsyncRunner * ProgressUpdateInfo
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | GenerationStarted of AsyncRunner
        | GenerationCompleted of AsyncRunner * list<RunInfo>
        | RunModel of AsyncRunner * ModelDataId * ModelCommandLineParam
        | StartRun of AsyncRunner

        //| Started of ProcessStartedInfo
        //| CompleteRun of AsyncRunner * ProcessStartedInfo

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", ") + "]"

            match m with
            | QueueStarting _ -> "QueueStarting"
            | QueueObtained _ -> "QueueObtained"
            | ConfigureService _ -> "ConfigureService"
            | ProgressUpdated (_, p) -> "ProgressUpdated: " + (p.ToString())
            | GetState _ -> "GetState"
            | GenerationStarted _ -> "GenerationStarted"
            | GenerationCompleted (_, r) -> "GenerationCompleted: " + (toStr r)
            | RunModel _ -> "RunModel"
            | StartRun _ -> "StartRun"

            //| Started p -> "Started: " + (p.ToString())
            //| CompleteRun (_, r) -> "CompleteRun: " + (r.ToString())


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        //let mutable generating = 0
        //let mutable msgCount = 0L
        //let mutable runningModel = 0


        let onQueueStarting s (a : AsyncRunner) =
            let w() =
                generatorInfo.getQueue() |> a.queueObtained
                s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s


        let onQueueObtained (s : AsyncRunnerState) (a : AsyncRunner) p =
            a.startRun()
            let x = s.runningQueue
            { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }


        let onConfigureService (s : AsyncRunnerState) (a : AsyncRunner) (p : ContGenConfigParam) =
            //match p with
            //| SetToIdle -> { s with workState = Idle }
            //| SetToCanGenerate ->
            //    h.startGenerate()
            //    { s with workState = CanGenerate }
            //| RequestShutDown b ->
            //    match b with
            //    | false ->
            //        let s1 =
            //            s.running
            //            |> Map.toList
            //            |> List.fold (fun (acc : AsyncRunnerState) (i, _) -> acc.configureService h (CancelTask i)) s
            //        { s1 with workState = ShuttingDown; queue = [] }
            //    | true -> { s with workState = ShuttingDown }
            //| SetRunLimit v ->
            //    let newState =
            //        match s.usePartitioner with
            //        | false -> { s with runLimit = max 1 (min v Environment.ProcessorCount) }
            //        | true -> { s with runLimit = max 0 v }
            //
            //    printfn "AsyncRunnerState.configureService: Calling h.startRun()..."
            //    h.startRun()
            //    newState
            //| CancelTask i ->
            //    match h.cancelProcess i with
            //    | true -> { s with running = s.running.tryRemove i }
            //    | false -> s
            //| SetMinUsefulEe ee ->
            //    { s with minUsefulEe = MinUsefulEe ee }
            failwith ""


        let  onProgressUpdated (s : AsyncRunnerState) (a : AsyncRunner) (p : ProgressUpdateInfo) =
            printfn "AsyncRunner.onProgressUpdated: %A" p
            match s.running.TryFind p.updatedProcessId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
                | Completed ->
                    match e.runningQueueId with
                    | Some v -> generatorInfo.removeFromQueue v
                    | None -> ignore()
        
                    a.startRun()
                    printfn "AsyncRunner.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.updatedProcessId
                    { s with running =  s.running.Remove p.updatedProcessId }
            | None ->
                printfn "AsyncRunner.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.updatedProcessId
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, p.runningProcessInfo) }
                | Completed -> s


        let onGetState s (r : AsyncReplyChannel<AsyncRunnerState>) =
            r.Reply s
            s


        let onGenerationStarted (s : AsyncRunnerState) (a : AsyncRunner) =
            match s.workState with
            | Idle -> s
            | CanGenerate ->
                generatorInfo.generate() |> a.generationCompleted
                s
            | ShuttingDown -> s


        let onGenerationCompleted (s : AsyncRunnerState) (a : AsyncRunner) r =
            let w() =
                a.startRun()
                let x = s.runningQueue
                { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s


        let onRunModel s (a : AsyncRunner) (i : ModelDataId) p =
            let x = generatorInfo.runModel i p
            s


        //let runModelImpl (a : AsyncRunner) i p =
        //    printfn "runModelImpl: p = %A" p
        //    match generatorInfo.runModel i p with
        //    | Some r -> [ r ] |> a.completeGenerate
        //    | None -> ignore()


        let startModelImpl s (a : AsyncRunner) (e : RunInfo) : AsyncRunnerState =
        
            let x =
                {
                    modelDataId = e.processToStartInfo.modelDataId
                    runQueueId = e.processToStartInfo.runQueueId
                } |> e.run
        
            printfn "AsyncRunner.startModelImpl: Starting modelId: %A - result: %A." e.processToStartInfo.modelDataId x
        
            match x with
            | StartedSuccessfully r ->
                //a.started { processId = r.runningProcessInfo.runningProcessId; processToStartInfo = r.processToStartInfo }
                //a.completeRun r
                failwith ""
            | FailedToStart -> s
            | AlreadyCompleted ->
                generatorInfo.removeFromQueue e.processToStartInfo.runQueueId
                failwith ""


        let onStartRun (s : AsyncRunnerState) (a : AsyncRunner) =
            printfn "AsyncRunner.onStartRun: s = %A" s

            let w() =
                if s.runningCount < s.runLimit
                then
                    match s.queue with
                    | [] -> s
                    | p :: t ->

                        a.runModel(p.processToStartInfo.modelDataId, failwith "")
                        { s with queue = t }

                        //match h.tryAcquireStartingModel() with
                        //| true ->
                        //    printfn "AsyncRunnerState.onRunStarting - calling h.startModel %A" p
                        //    h.startModel p
                        //    { s with queue = t }
                        //| false -> s
                else s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s


        //// Returns true if successfully acquired generating flag.
        //let tryAcquireGeneratingImpl() = Interlocked.CompareExchange(&generating, 1, 0) = 0
        //let releaseGeneratingImpl() = Interlocked.Exchange(&generating, 0) |> ignore

        //let tryAcquireStartingModelImpl() = Interlocked.CompareExchange(&runningModel, 1, 0) = 0
        //let releaseStartingModelImpl() = Interlocked.Exchange(&runningModel, 0) |> ignore

        //let generateImpl (a : AsyncRunner) () = (fun() -> generatorInfo.generate () |> a.completeGenerate) |> toAsync |> Async.Start
        //let startGenerateImpl(a : AsyncRunner) () = a.startGenerate |> toAsync |> Async.Start
        //let startRunImpl (a : AsyncRunner) () = a.startRun |> toAsync |> Async.Start
        //let getQueueImpl (a : AsyncRunner) () = (fun () -> generatorInfo.getQueue() |> a.queueObtained) |> toAsync |> Async.Start
        //let removeFromQueueImpl i = (fun () -> generatorInfo.removeFromQueue i) |> toAsync |> Async.Start



//type ProcessStartedResult =
//    | StartedSuccessfully of ProcessStartedInfo
//    | AlreadyCompleted
//    | FailedToStart

//type ProgressUpdateInfo =
//    {
//        updatedProcessId : ProcessId
//        updateModelId : ModelDataId
//        progress : TaskProgress
//        resultDataId : ResultDataId
//    }


        //let cancelProcessImpl i =
        //    try
        //        match i with
        //        | LocalProcess (LocalProcessId a) -> (Process.GetProcessById a).Kill()
        //        | RemoteProcess a -> printfn "Cannot yet cancel remove process: %A." a
        //        true
        //    with
        //        | e -> false


        //let h (a : AsyncRunner) =
        //    {
        //        cancelProcess = cancelProcessImpl
        //        generate = generateImpl a
        //        startGenerate = startGenerateImpl a
        //        startRun = startRunImpl a
        //        tryAcquireStartingModel = tryAcquireStartingModelImpl
        //        releaseStartingModel = releaseStartingModelImpl
        //        startModel = startModelImpl removeFromQueueImpl a
        //        getQueue = getQueueImpl a
        //        removeFromQueue = removeFromQueueImpl
        //        tryAcquireGenerating = tryAcquireGeneratingImpl
        //        releaseGenerating = releaseGeneratingImpl
        //        runModel = runModelImpl a
        //    }


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (sInput : AsyncRunnerState) =
                    async
                        {
                            let! m = u.Receive()
                            let s = { sInput with messageCount = sInput.messageCount + 1L }
                            printfn "AsyncRunner.s = %s, m = %A." (s.ToString()) (m.ToString())

                            match m with
                            | QueueStarting a -> return! onQueueStarting s a  |> loop
                            | QueueObtained (a, r) -> return! onQueueObtained s a r |> loop
                            | ConfigureService (a, p) -> return! onConfigureService s a p |> loop
                            | ProgressUpdated (a, p) -> return! onProgressUpdated s a p |> loop
                            | GetState r -> return! onGetState s r |> loop
                            | GenerationStarted a -> return! onGenerationStarted s a |> loop
                            | GenerationCompleted (a, r) -> return! onGenerationCompleted s a r |> loop
                            | RunModel (a, m, p) -> return! onRunModel s a m p |> loop
                            | StartRun a -> return! onStartRun s a |> loop

                            //| Started p -> return! loop (s.onStarted p)
                            //| CompleteRun (a, x) -> return! loop (s.onProcessStarted (h a) x)
                        }

                AsyncRunnerState.defaultValue generatorInfo.usePartitioner |> loop
                )

        member this.queueStarting () : unit = QueueStarting this |> messageLoop.Post
        member private this.queueObtained (r : list<RunInfo>) = QueueObtained (this, r) |> messageLoop.Post
        member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
        member this.progressUpdated p = ProgressUpdated (this, p) |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState
        member this.generationStarted () : unit = GenerationStarted this |> messageLoop.Post
        member private this.generationCompleted (r : list<RunInfo>) : unit = GenerationCompleted (this, r) |> messageLoop.Post
        member this.runModel(m, p) = RunModel (this, m, p) |> messageLoop.Post

        member private this.startRun () = StartRun this |> messageLoop.Post
        //member private this.completeRun n = CompleteRun (this, n) |> messageLoop.Post

        //member this.started p = Started p |> messageLoop.Post
        member this.start() = SetToCanGenerate |> this.configureService
        member this.stop() = SetToIdle |> this.configureService

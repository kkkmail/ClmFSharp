﻿namespace ContGen

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
            maxQueueLength : int
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
            maxQueueLength : int
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
                maxQueueLength = 4
                workState = CanGenerate
                messageCount = 0L
                minUsefulEe = MinUsefulEe DefaultMinEe
                usePartitioner = u
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelDataId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.runningModelId e.runningProcessId e.started e.progress) |> String.concat ", "
            sprintf "{ running: [%s]; queue: [%s]; runLimit = %A; runningCount: %A; workState: %A; minUsefulEe: %A }" r q s.runLimit s.runningCount s.workState s.minUsefulEe

        member s.isShuttingDown =
            match s.workState with
            | Idle | CanGenerate -> false
            | ShuttingDown -> true



        //member s.onGenerationStarting h =
        //    match s.workState with
        //    | Idle -> s
        //    | CanGenerate ->
        //        if s.queue.Length <= s.maxQueueLength
        //        then
        //            printfn "s.queue.Length = %A. Starting generating..." s.queue.Length
        //            if h.tryAcquireGenerating() then h.generate()
        //        s
        //    | ShuttingDown -> s

        //member s.onProgressUpdated h (p : ProgressUpdateInfo) =
        //    printfn "AsyncRunnerState.onProgressUpdated: %A" p
        //    match s.running.TryFind p.updatedProcessId with
        //    | Some e ->
        //        match p.progress with
        //        | NotStarted | InProgress _ ->
        //            { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
        //        | Completed ->
        //            match e.runningQueueId with
        //            | Some v -> h.removeFromQueue v
        //            | None -> ignore()

        //            h.startRun ()
        //            printfn "AsyncRunnerState.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.updatedProcessId
        //            { s with running =  s.running.Remove p.updatedProcessId }
        //    | None ->
        //        printfn "AsyncRunnerState.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.updatedProcessId
        //        match p.progress with
        //        | NotStarted | InProgress _ ->
        //            { s with running = s.running.Add(p.updatedProcessId, p.runningProcessInfo) }
        //        | Completed -> s

        //member s.onGenerationCompleted h r =
        //    let w() =
        //        h.releaseGenerating()
        //        h.startRun ()
        //        let x = s.runningQueue
        //        { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.runQueueId) |> List.filter (fun e -> x.Contains e.runQueueId |> not) }

        //    match s.workState with
        //    | Idle -> w()
        //    | CanGenerate -> w()
        //    | ShuttingDown -> s

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

        //member s.onRunStarting h =
        //    printfn "AsyncRunnerState.onRunStarting: s = %A" s
        //    let w() =
        //        if s.runningCount < s.runLimit
        //        then
        //            match s.queue with
        //            | [] -> s
        //            | p :: t ->
        //                match h.tryAcquireStartingModel() with
        //                | true ->
        //                    printfn "AsyncRunnerState.onRunStarting - calling h.startModel %A" p
        //                    h.startModel p
        //                    { s with queue = t }
        //                | false -> s
        //        else s

        //    match s.workState with
        //    | Idle -> w()
        //    | CanGenerate -> w()
        //    | ShuttingDown -> s

        //member s.onStarted p =
        //    printfn "Started: %A" p
        //    { s with running = s.running.Add(p.processId, p.runningProcessInfo)}

        //member s.getState c reply =
        //    toAsync (fun () -> reply { s with messageCount = c }) |> Async.Start
        //    { s with messageCount = c }

        //member s.configureService h (p : ContGenConfigParam) =
        //    match p with
        //    | SetToIdle -> { s with workState = Idle }
        //    | SetToCanGenerate ->
        //        h.startGenerate()
        //        { s with workState = CanGenerate }
        //    | RequestShutDown b ->
        //        match b with
        //        | false ->
        //            let s1 =
        //                s.running
        //                |> Map.toList
        //                |> List.fold (fun (acc : AsyncRunnerState) (i, _) -> acc.configureService h (CancelTask i)) s
        //            { s1 with workState = ShuttingDown; queue = [] }
        //        | true -> { s with workState = ShuttingDown }
        //    | SetRunLimit v ->
        //        let newState =
        //            match s.usePartitioner with
        //            | false -> { s with runLimit = max 1 (min v Environment.ProcessorCount) }
        //            | true -> { s with runLimit = max 0 v }

        //        printfn "AsyncRunnerState.configureService: Calling h.startRun()..."
        //        h.startRun()
        //        newState
        //    | CancelTask i ->
        //        match h.cancelProcess i with
        //        | true -> { s with running = s.running.tryRemove i }
        //        | false -> s
        //    | SetMinUsefulEe ee ->
        //        { s with minUsefulEe = MinUsefulEe ee }

        //member s.runModel h i p =
        //    h.runModel i p
        //    s


    type RunnerMessage =
        | QueueStarting of AsyncRunner
        | QueueObtained of AsyncRunner * list<RunInfo>
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner
        | Started of ProcessStartedInfo
        | UpdateProgress of AsyncRunner * ProgressUpdateInfo
        | CompleteRun of AsyncRunner * ProcessStartedInfo
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | ConfigureService of AsyncRunner * ContGenConfigParam
        | RunModel of AsyncRunner * ModelDataId * ModelCommandLineParam

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.modelDataId.ToString()) |> String.concat ", ") + "]"

            match m with
            | QueueStarting _ -> "QueueStarting"
            | QueueObtained _ -> "CompleteQueue"
            | StartGenerate _ -> "StartGenerate"
            | CompleteGenerate (_, r) -> "CompleteGenerate: " + (toStr r)
            | StartRun _ -> "StartRun"
            | Started p -> "Started: " + (p.ToString())
            | UpdateProgress (_, p) -> "ProgressUpdate: " + (p.ToString())
            | CompleteRun (_, r) -> "CompleteRun: " + (r.ToString())
            | GetState _ -> "GetState"
            | ConfigureService _ -> "ConfigureService"
            | RunModel _ -> "RunModel"


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        //let mutable generating = 0
        let mutable msgCount = 0L
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
            { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.runQueueId) |> List.filter (fun e -> x.Contains e.runQueueId |> not) }



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


        //let startModelImpl removeFromQueue (a : AsyncRunner) (e : RunInfo) =

        //    let x =
        //        {
        //            modelDataId = e.modelDataId
        //            runQueueId = e.runQueueId
        //        } |> e.run

        //    printfn "AsyncRunner.startModelImpl: Starting modelId: %A - result: %A." e.modelDataId x

        //    match x with
        //    | StartedSuccessfully r ->
        //        a.started { processId = r.runningProcessInfo.runningProcessId; processToStartInfo = r.processToStartInfo }
        //        a.completeRun r
        //    | FailedToStart -> ignore()
        //    | AlreadyCompleted -> removeFromQueue e.runQueueId


        //let cancelProcessImpl i =
        //    try
        //        match i with
        //        | LocalProcess (LocalProcessId a) -> (Process.GetProcessById a).Kill()
        //        | RemoteProcess a -> printfn "Cannot yet cancel remove process: %A." a
        //        true
        //    with
        //        | e -> false


        //let runModelImpl (a : AsyncRunner) i p =
        //    printfn "runModelImpl: p = %A" p
        //    match generatorInfo.runModel i p with
        //    | Some r -> [ r ] |> a.completeGenerate
        //    | None -> ignore()


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
                let rec loop (s : AsyncRunnerState) =
                    async
                        {
                            printfn "AsyncRunner.s = %s" (s.ToString())
                            let! m = u.Receive()
                            Interlocked.Increment(&msgCount) |> ignore
                            printfn "AsyncRunner.m = %s" (m.ToString())

                            match m with
                            | QueueStarting a -> return! loop (onQueueStarting s a)
                            | QueueObtained (a, r) -> return! loop (onQueueObtained s r)
                            | StartGenerate a -> return! loop (s.onGenerationStarting (h a))
                            | CompleteGenerate (a, r) -> return! loop (s.onGenerationCompleted (h a) r)
                            | StartRun a -> return! loop (s.onRunStarting (h a))
                            | Started p -> return! loop (s.onStarted p)
                            | UpdateProgress (a, p) -> return! loop (s.onProgressUpdated (h a) p)
                            | CompleteRun (a, x) -> return! loop (s.onProcessStarted (h a) x)
                            | GetState r -> return! loop (s.getState msgCount r.Reply)
                            | ConfigureService (a, p) -> return! loop (s.configureService (h a) p)
                            | RunModel (a, m, p) -> return! loop (s.runModel (h a) m p)
                        }

                AsyncRunnerState.defaultValue generatorInfo.usePartitioner |> loop
                )

        member this.queueStarting () : unit = QueueStarting this |> messageLoop.Post
        member private this.queueObtained (r : list<RunInfo>) = QueueObtained (this, r) |> messageLoop.Post
        member private this.completeGenerate (r : list<RunInfo>) : unit = CompleteGenerate (this, r) |> messageLoop.Post
        member private this.startRun () = StartRun this |> messageLoop.Post
        member private this.completeRun n = CompleteRun (this, n) |> messageLoop.Post

        member this.started p = Started p |> messageLoop.Post
        member this.startGenerate () : unit = StartGenerate this |> messageLoop.Post
        member this.updateProgress p = UpdateProgress (this, p) |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState
        member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
        member this.start() = SetToCanGenerate |> this.configureService
        member this.stop() = SetToIdle |> this.configureService
        member this.runModel(m, p) = RunModel (this, m, p) |> messageLoop.Post

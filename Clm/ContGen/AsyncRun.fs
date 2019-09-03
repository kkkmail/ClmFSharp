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
            runModel : ModelDataId -> ModelCommandLineParam -> RunInfo option
            usePartitioner : bool
        }


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
        member state.runningQueue = state.running |> Map.toList |> List.map (fun (_, v) -> v.runningQueueId) |> List.choose id |> Set.ofList

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


    type RunnerMessage =
        | QueueStarting
        | ConfigureService of AsyncRunner * ContGenConfigParam
        | ProgressUpdated of ProgressUpdateInfo
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | GenerationStarted of AsyncRunner
        | GenerationCompleted of list<RunInfo>
        | RunModel of ModelDataId * ModelCommandLineParam

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", ") + "]"

            match m with
            | QueueStarting _ -> "QueueStarting"
            | ConfigureService _ -> "ConfigureService"
            | ProgressUpdated p -> "ProgressUpdated: " + (p.ToString())
            | GetState _ -> "GetState"
            | GenerationStarted _ -> "GenerationStarted"
            | GenerationCompleted r -> "GenerationCompleted: " + (toStr r)
            | RunModel _ -> "RunModel"


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        let mutable generating = 0

        // Returns true if successfully acquired generating flag.
        let tryAcquireGenerating() = Interlocked.CompareExchange(&generating, 1, 0) = 0
        let releaseGenerating() = Interlocked.Exchange(&generating, 0) |> ignore


        let onStartRun (s : AsyncRunnerState) =
            printfn "AsyncRunner.onStartRun: s = %A" s
            let updateQueue t (g : AsyncRunnerState) = { g with queue = t }

            let start (g : AsyncRunnerState) e =
                let x = e.run e.processToStartInfo
                printfn "AsyncRunner.onStartRun: Starting modelId: %A - result: %A." e.processToStartInfo.modelDataId x

                match x with
                | StartedSuccessfully r ->
                    { g with running = g.running.Add(r.runningProcessInfo.runningProcessId, r.runningProcessInfo)}
                | FailedToStart -> g
                | AlreadyCompleted ->
                    generatorInfo.removeFromQueue e.processToStartInfo.runQueueId
                    g

            let w() =
                if s.runningCount < s.runLimit
                then
                    let run, queue = s.queue |> List.splitAt (min s.queue.Length (max 0 (s.runLimit - s.runningCount)))

                    run
                    |> List.fold (fun acc e -> start acc e) s
                    |> updateQueue queue
                else s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s


        let onQueueObtained (g : AsyncRunnerState) p =
            let s = onStartRun g
            let x = s.runningQueue
            { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }


        let onQueueStarting s =
            let w() = generatorInfo.getQueue() |> onQueueObtained s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s


        let cancelProcessImpl i =
            try
                match i with
                | LocalProcess (LocalProcessId a) -> (Process.GetProcessById a).Kill()
                | RemoteProcess a -> printfn "Cannot yet cancel remove process: %A." a
                true
            with
                | e -> false


        let onConfigureService (s : AsyncRunnerState) (a : AsyncRunner) (p : ContGenConfigParam) =
            match p with
            | SetToIdle -> { s with workState = Idle }
            | SetToCanGenerate ->
                a.generationStarted()
                { s with workState = CanGenerate }
            | RequestShutDown b ->
                match b with
                | false ->
                    s.running
                    |> Map.toList
                    |> List.map (fun (i, _) -> a.configureService (CancelTask i))
                    |> ignore
                    { s with workState = ShuttingDown; queue = [] }
                | true -> { s with workState = ShuttingDown }
            | SetRunLimit v ->
                let newState =
                    match s.usePartitioner with
                    | false -> { s with runLimit = max 1 (min v Environment.ProcessorCount) }
                    | true -> { s with runLimit = max 0 v }
            
                printfn "AsyncRunner.configureService: Calling startRun()..."
                onStartRun newState
            | CancelTask i ->
                match cancelProcessImpl i with
                | true -> { s with running = s.running.tryRemove i }
                | false -> s
            | SetMinUsefulEe ee -> { s with minUsefulEe = MinUsefulEe ee }


        let  onProgressUpdated (s : AsyncRunnerState) (p : ProgressUpdateInfo) =
            printfn "AsyncRunner.onProgressUpdated: %A" p
            match s.running.TryFind p.updatedProcessId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
                | Completed ->
                    match e.runningQueueId with
                    | Some v -> generatorInfo.removeFromQueue v
                    | None -> ignore()
        
                    printfn "AsyncRunner.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.updatedProcessId
                    onStartRun { s with running =  s.running.tryRemove p.updatedProcessId }
            | None ->
                printfn "AsyncRunner.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.updatedProcessId
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.updatedProcessId, p.runningProcessInfo) }
                | Completed -> s


        let onGetState s (r : AsyncReplyChannel<AsyncRunnerState>) =
            r.Reply s
            s


        let onGenerationStarted (s : AsyncRunnerState) (a : AsyncRunner) =
            match s.workState with
            | Idle -> s
            | CanGenerate ->
                let generate() = generatorInfo.generate() |> a.generationCompleted

                if s.queue.Length <= s.maxQueueLength
                then
                    printfn "AsyncRunner.onGenerationStarted - s.queue.Length = %A. Starting generating..." s.queue.Length
                    if tryAcquireGenerating() then generate |> toAsync |> Async.Start
                s
            | ShuttingDown -> s


        let onGenerationCompleted (s : AsyncRunnerState) r =
            let w() =
                releaseGenerating()
                let x = s.runningQueue
                { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }
                |> onStartRun

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown ->
                releaseGenerating()
                s


        let onRunModel (s : AsyncRunnerState) (i : ModelDataId) p =
            match generatorInfo.runModel i p with
            | Some r ->
                let x = s.runningQueue
                let filter e = x.Contains e.processToStartInfo.runQueueId |> not
                { s with queue = s.queue @ [ r ] |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter filter }
            | None -> s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (sInput : AsyncRunnerState) =
                    async
                        {
                            let! m = u.Receive()
                            let s = { sInput with messageCount = sInput.messageCount + 1L }
                            printfn "AsyncRunner.s = %s, m = %A." (s.ToString()) (m.ToString())

                            match m with
                            | QueueStarting -> return! onQueueStarting s |> loop
                            | ConfigureService (a, p) -> return! onConfigureService s a p |> loop
                            | ProgressUpdated p -> return! onProgressUpdated s p |> loop
                            | GetState r -> return! onGetState s r |> loop
                            | GenerationStarted a -> return! onGenerationStarted s a |> loop
                            | GenerationCompleted r -> return! onGenerationCompleted s r |> loop
                            | RunModel (m, p) -> return! onRunModel s m p |> loop
                        }

                AsyncRunnerState.defaultValue generatorInfo.usePartitioner |> loop
                )

        member __.queueStarting () : unit = QueueStarting |> messageLoop.Post
        member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
        member __.progressUpdated p = ProgressUpdated p |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState
        member this.generationStarted () : unit = GenerationStarted this |> messageLoop.Post
        member private __.generationCompleted (r : list<RunInfo>) : unit = GenerationCompleted r |> messageLoop.Post
        member __.runModel(m, p) = RunModel (m, p) |> messageLoop.Post
        member this.start() = SetToCanGenerate |> this.configureService
        member this.stop() = SetToIdle |> this.configureService

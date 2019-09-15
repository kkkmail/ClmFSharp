namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo

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
        member state.runningQueue = state.running |> Map.toList |> List.map (fun (_, v) -> v.progressUpdateInfo.processStartedInfo.runningProcessData.runQueueId) |> Set.ofList

        static member defaultValue u =
            {
                running = Map.empty
                queue = []
                runLimit = if u then 0 else Environment.ProcessorCount
                maxQueueLength = 4
                workState = NotInitialized
                messageCount = 0L
                minUsefulEe = MinUsefulEe DefaultMinEe
                usePartitioner = u
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.progressUpdateInfo.processStartedInfo.runningProcessData.modelDataId e.progressUpdateInfo.processStartedInfo.processId e.started e.progressUpdateInfo.progress) |> String.concat ", "
            sprintf "{ running: [%s]; queue: [%s]; runLimit = %A; runningCount: %A; workState: %A; minUsefulEe: %A }" r q s.runLimit s.runningCount s.workState s.minUsefulEe

        member s.isShuttingDown =
            match s.workState with
            | NotInitialized | Idle | CanGenerate | Generating -> false
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

        let onStartRun (s : AsyncRunnerState) =
            printfn "AsyncRunner.onStartRun: s = %A" s
            let updateQueue t (g : AsyncRunnerState) = { g with queue = t }

            let start (g : AsyncRunnerState) e =
                let x = timed "AsyncRunner.onStartRun.start: e.run" e.run e.processToStartInfo
                printfn "AsyncRunner.onStartRun: Starting modelId: %A - result: %A." e.processToStartInfo.modelDataId x

                match x with
                | StartedSuccessfully r ->
                    { g with running = g.running.Add(r.processId, r.toRunningProcessInfo())}
                | FailedToStart -> g
                | AlreadyCompleted ->
                    generatorInfo.removeFromQueue e.processToStartInfo.runQueueId
                    g

            let w() =
                if s.runningCount < s.runLimit
                then
                    let run, queue = s.queue |> List.splitAt (min s.queue.Length (max 0 (s.runLimit - s.runningCount)))
                    printfn "AsyncRunner.onStartRun: run = %A, queue = %A" run queue

                    run
                    |> List.fold (fun acc e -> timed "AsyncRunner.onStartRun.start" start acc e) s
                    |> updateQueue queue
                else s

            match s.workState with
            | NotInitialized -> s
            | Idle -> w()
            | CanGenerate -> w()
            | Generating -> w()
            | ShuttingDown -> s


        let onQueueObtained (s : AsyncRunnerState) p =
            { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) }
            |> onStartRun


        let onQueueStarting s =
            let w t = generatorInfo.getQueue() |> onQueueObtained { s with workState = t }

            match s.workState with
            | NotInitialized -> w CanGenerate
            | Idle | CanGenerate | Generating -> w s.workState
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
                match s.usePartitioner with
                | false -> { s with runLimit = max 1 (min v Environment.ProcessorCount) }
                | true -> { s with runLimit = max 0 v }
                |> onStartRun
            | CancelTask i ->
                match cancelProcessImpl i with
                | true -> { s with running = s.running.tryRemove i }
                | false -> s
            | SetMinUsefulEe ee -> { s with minUsefulEe = MinUsefulEe ee }


        let  onProgressUpdated (s : AsyncRunnerState) (p : ProgressUpdateInfo) =
            printfn "AsyncRunner.onProgressUpdated: %A" p
            let removeFromQueue() = generatorInfo.removeFromQueue p.processStartedInfo.runningProcessData.runQueueId

            match s.running.TryFind p.processStartedInfo.processId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, { e with progressUpdateInfo = { e.progressUpdateInfo with progress = p.progress } })}
                | Completed ->
                    removeFromQueue()
                    printfn "AsyncRunner.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.processStartedInfo.processId
                    onStartRun { s with running =  s.running.tryRemove p.processStartedInfo.processId }
            | None ->
                printfn "AsyncRunner.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.processStartedInfo.processId
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, p.toRunningProcessInfo()) }
                | Completed ->
                    removeFromQueue()
                    s


        let onGetState s (r : AsyncReplyChannel<AsyncRunnerState>) =
            r.Reply s
            s


        let onGenerationStarted (s : AsyncRunnerState) (a : AsyncRunner) =
            match s.workState with
            | NotInitialized | Idle | Generating | ShuttingDown -> s
            | CanGenerate ->
                let generate() = generatorInfo.generate() |> a.generationCompleted

                if s.queue.Length <= s.maxQueueLength
                then
                    printfn "AsyncRunner.onGenerationStarted - s.queue.Length = %A. Starting generating..." s.queue.Length
                    generate |> toAsync |> Async.Start
                    { s with workState = Generating }
                else s


        let onGenerationCompleted (s : AsyncRunnerState) r =
            let w t =
                let x = s.runningQueue
                { s with workState = t; queue = s.queue @ r |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }
                |> timed "AsyncRunner.onGenerationCompleted.onStartRun" onStartRun

            match s.workState with
            | Idle | CanGenerate -> w s.workState
            | Generating -> w CanGenerate
            | NotInitialized | ShuttingDown -> s


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
                            | QueueStarting -> return! timed "AsyncRunner.onQueueStarting" onQueueStarting s |> loop
                            | ConfigureService (a, p) -> return! timed "AsyncRunner.onConfigureService" onConfigureService s a p |> loop
                            | ProgressUpdated p -> return! timed "AsyncRunner.onProgressUpdated" onProgressUpdated s p |> loop
                            | GetState r -> return! timed "AsyncRunner.onGetState" onGetState s r |> loop
                            | GenerationStarted a -> return! timed "AsyncRunner.onGenerationStarted" onGenerationStarted s a |> loop
                            | GenerationCompleted r -> return! timed "AsyncRunner.onGenerationCompleted" onGenerationCompleted s r |> loop
                            | RunModel (m, p) -> return! timed "AsyncRunner.onRunModel" onRunModel s m p |> loop
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

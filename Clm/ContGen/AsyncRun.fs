namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.Logging

module AsyncRun =

    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : RunQueueId -> unit
            runModel : ModelDataId -> ModelCommandLineParam -> RunInfo option
            usePartitioner : bool
            logger : Logger
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
            lastRunError : string option
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
                lastRunError = None
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.processToStartInfo.modelDataId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.progressUpdateInfo.processStartedInfo.runningProcessData.modelDataId e.progressUpdateInfo.processStartedInfo.processId e.started e.progressUpdateInfo.progress) |> String.concat ", "
            sprintf "{ running: [%s]; queue: [%s]; runLimit = %A; runningCount: %A; workState: %A; minUsefulEe: %A; lastRunError = %A }" r q s.runLimit s.runningCount s.workState s.minUsefulEe s.lastRunError

        member s.isShuttingDown =
            match s.workState with
            | NotInitialized | Idle | CanGenerate | Generating -> false
            | ShuttingDown -> true


    let private className = "AsyncRunner"
    let private getMethodName n = className + "." + n
    let private onStartRunName = getMethodName "onStartRun"
    let private cancelProcessImplName = getMethodName "cancelProcessImpl"
    let private onQueueStartingName = getMethodName "onQueueStarting"
    let private onGenerationStartedName = getMethodName "onGenerationStarted"
    let private onGenerationCompletedName = getMethodName "onGenerationCompleted"
    let private onConfigureServiceName = getMethodName "onConfigureService"
    let private onProgressUpdatedName = getMethodName "onProgressUpdated"
    let private onGetStateName = getMethodName "onGetState"
    let private onRunModelName = getMethodName "onRunModel"


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
            printfn "%s: s = %A" onStartRunName s
            let updateQueue t (g : AsyncRunnerState) = { g with queue = t }

            let start (g : AsyncRunnerState) e =
                let x = timed (onStartRunName + ".start: e.run") e.run e.processToStartInfo
                printfn "%s: Starting modelId: %A - result: %A." onStartRunName e.processToStartInfo.modelDataId x

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
                    printfn "%s: run = %A, queue = %A" onStartRunName run queue

                    run
                    |> List.fold (fun acc e -> timed (onStartRunName + ".start") start acc e) s
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
            printfn "%s..." onQueueStartingName

            let w t =
                let x = generatorInfo.getQueue()
                printfn "%s: queue length: %A" onQueueStartingName x.Length
                x |> onQueueObtained { s with workState = t }

            match s.workState with
            | NotInitialized -> w CanGenerate
            | Idle | CanGenerate | Generating -> w s.workState
            | ShuttingDown -> s


        let cancelProcessImpl i =
            try
                match i with
                | LocalProcess (LocalProcessId a) -> (Process.GetProcessById a).Kill()
                | RemoteProcess a -> generatorInfo.logger.logErr (sprintf "%s: Cannot yet cancel remote process: %A." cancelProcessImplName a)
                true
            with
            | _ -> false


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


        let updateQueue (s : AsyncRunnerState) r =
            let x = s.runningQueue
            { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter (fun e -> x.Contains e.processToStartInfo.runQueueId |> not) }


        let onRunModel (s : AsyncRunnerState) (i : ModelDataId) p =
            match generatorInfo.runModel i p with
            | Some r ->
                let x = s.runningQueue
                let filter e = x.Contains e.processToStartInfo.runQueueId |> not
                { s with queue = s.queue @ [ r ] |> List.distinctBy (fun e -> e.processToStartInfo.runQueueId) |> List.filter filter }
            | None -> s


        let  onProgressUpdated (s : AsyncRunnerState) (p : ProgressUpdateInfo) =
            printfn "AsyncRunner.onProgressUpdated: %A" p
            let removeFromQueue() = generatorInfo.removeFromQueue p.processStartedInfo.runningProcessData.runQueueId

            let onCompleted g =
                printfn "AsyncRunner.onProgressUpdated: trying to remove: p.updatedProcessId = %A" p.processStartedInfo.processId
                onStartRun { g with running = g.running.tryRemove p.processStartedInfo.processId }

            let onFailed g f =
                let i = p.processStartedInfo.runningProcessData.modelDataId
                let c = p.processStartedInfo.runningProcessData.commandLineParams
                onRunModel { g with running = g.running.tryRemove p.processStartedInfo.processId; lastRunError = Some f } i c

            match s.running.TryFind p.processStartedInfo.processId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, { e with progressUpdateInfo = { e.progressUpdateInfo with progress = p.progress } })}
                | Completed ->
                    removeFromQueue()
                    onCompleted s
                | Failed f -> onFailed s f
            | None ->
                printfn "AsyncRunner.onProgressUpdated: unable to find: p.updatedProcessId = %A" p.processStartedInfo.processId
                match p.progress with
                | NotStarted | InProgress _ -> { s with running = s.running.Add(p.processStartedInfo.processId, p.toRunningProcessInfo()) }
                | Completed ->
                    removeFromQueue()
                    s
                | Failed f -> onFailed s f


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
                    printfn "%s: s.queue.Length = %A. Starting generating..." onGenerationStartedName s.queue.Length
                    generate |> toAsync |> Async.Start
                    { s with workState = Generating }
                else s


        let onGenerationCompleted (s : AsyncRunnerState) r =
            let w t = { updateQueue s r with workState = t } |> timed onGenerationCompletedName onStartRun

            match s.workState with
            | Idle | CanGenerate -> w s.workState
            | Generating -> w CanGenerate
            | NotInitialized | ShuttingDown -> s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (sInput : AsyncRunnerState) =
                    async
                        {
                            let! m = u.Receive()
                            let s = { sInput with messageCount = sInput.messageCount + 1L }
                            printfn "%s: s = %s, m = %A." className (s.ToString()) (m.ToString())

                            match m with
                            | QueueStarting -> return! timed onQueueStartingName onQueueStarting s |> loop
                            | ConfigureService (a, p) -> return! timed onConfigureServiceName onConfigureService s a p |> loop
                            | ProgressUpdated p -> return! timed onProgressUpdatedName onProgressUpdated s p |> loop
                            | GetState r -> return! timed onGetStateName onGetState s r |> loop
                            | GenerationStarted a -> return! timed onGenerationStartedName onGenerationStarted s a |> loop
                            | GenerationCompleted r -> return! timed onGenerationCompletedName onGenerationCompleted s r |> loop
                            | RunModel (m, p) -> return! timed onRunModelName onRunModel s m p |> loop
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

namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.ExitErrorCodes
open ContGenServiceInfo.ServiceInfo
open System.Threading

module AsyncRun =

    let partition maxVal q n =
        let (a, b) =
            q
            |> List.mapi (fun i e -> (i + n + 1, e))
            |> List.partition (fun (i, _) -> i <= maxVal)

        (a |> List.map snd, b |> List.map snd)


    type Map<'k, 'v when 'k : comparison>
        with

        member m.tryRemove k =
            match m.ContainsKey k with
            | true -> m.Remove k
            | false -> m


    //http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []


    type ProcessStartInfo =
        {
            processId : int
            modelId : ModelDataId
            runQueueId : RunQueueId
        }

        member this.runningProcessInfo =
            {
                started = DateTime.Now
                runningProcessId = this.processId
                runningModelId = this.modelId
                runningQueueId = Some this.runQueueId
                progress = TaskProgress.NotStarted
            }


    type ProcessResult =
        {
            startInfo : ProcessStartInfo
            exitCode : int
            runTime : int64
            outputs : seq<string>
            errors : seq<string>
        }


    type ProcessStartedCallBack =
        {
            notifyOnStarted : ProcessStartInfo -> unit
            calledBackModelId : ModelDataId
            runQueueId : RunQueueId
        }


    type RunInfo =
        {
            run : ProcessStartedCallBack -> ProcessStartInfo
            modelId : ModelDataId
            runQueueId : RunQueueId
        }


    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : RunQueueId -> unit
            maxQueueLength : int
            runModel : ModelDataId -> ModelCommandLineParam -> RunInfo
        }


    type AsyncRunnerHelper =
        {
            cancelProcess : int -> bool
            generate : unit -> unit
            tryAcquireGenerating : unit -> bool
            releaseGenerating : unit -> unit
            startGenerate : unit -> unit
            startRun : unit -> unit // Requests to run model(s).
            tryAcquireStartingModel : unit -> bool
            releaseStartingModel : unit -> unit
            startModel : RunInfo -> unit // Schedules a model run.
            getQueue : unit -> unit
            removeFromQueue : RunQueueId -> unit
            runModel : ModelDataId -> ModelCommandLineParam -> unit
        }


    type AsyncRunnerState =
        {
            running : Map<int, RunningProcessInfo>
            queue : list<RunInfo>
            runLimit : int
            maxQueueLength : int
            workState : WorkState
            messageCount : int64
        }

        member state.runningCount = state.running.Count

        member state.runningQueue =
            state.running |> Map.toList |> List.map (fun (_, v) -> v.runningQueueId) |> List.choose id |> Set.ofList

        static member defaultValue =
            {
                running = Map.empty
                queue = []
                runLimit = Environment.ProcessorCount
                maxQueueLength = 4
                workState = CanGenerate
                messageCount = 0L
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.runningModelId e.runningProcessId e.started e.progress) |> String.concat ", "
            sprintf "{ running: [%s], queue: [%s], runLimit = %A, runningCount: %A, workState: %A }" r q s.runLimit s.runningCount s.workState

        member s.onGenerationStarting h =
            match s.workState with
            | Idle -> s
            | CanGenerate ->
                if s.queue.Length <= s.maxQueueLength
                then
                    printfn "s.queue.Length = %A. Starting generating..." s.queue.Length
                    if h.tryAcquireGenerating() then h.generate()
                s
            | ShuttingDown -> s

        member s.onProgressUpdated h (p : ProgressUpdateInfo) =
            match s.running.TryFind p.updatedProcessId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
                | Completed ->
                    match e.runningQueueId with
                    | Some v -> h.removeFromQueue v
                    | None -> ignore()

                    h.startRun ()
                    { s with running =  s.running.Remove p.updatedProcessId }
            | None ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, p.runningProcessInfo) }
                | Completed -> s

        member s.onGenerationCompleted h r =
            let w() =
                h.releaseGenerating()
                h.startRun ()
                let x = s.runningQueue
                { s with queue = s.queue @ r |> List.distinctBy (fun e -> e.runQueueId) |> List.filter (fun e -> x.Contains e.runQueueId |> not) }

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.onQueueStarting h =
            let w() =
                h.getQueue()
                s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.OnQueueObtained h p =
            h.startRun()
            let x = s.runningQueue
            { s with queue = s.queue @ p |> List.distinctBy (fun e -> e.runQueueId) |> List.filter (fun e -> x.Contains e.runQueueId |> not) }

        member s.onProcessStarted h (x : ProcessStartInfo) =
            let w() =
                h.releaseStartingModel()
                h.startRun()
                { s with running = s.running.Add(x.processId, x.runningProcessInfo) }

            match s.workState with
            | Idle -> w()
            | CanGenerate ->
                h.startGenerate()
                w()
            | ShuttingDown -> 
                h.releaseStartingModel()
                s

        member s.onRunStarting h =
            let w() =
                if s.runningCount < s.runLimit
                then
                    match s.queue with
                    | [] -> s
                    | p :: t ->
                        match h.tryAcquireStartingModel() with
                        | true ->
                            h.startModel p
                            { s with queue = t }
                        | false -> s
                else s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.onStarted p =
            printfn "Started: %A" p
            { s with running = s.running.Add(p.processId, p.runningProcessInfo)}

        member s.getState c reply =
            toAsync (fun () -> reply { s with messageCount = c }) |> Async.Start
            { s with messageCount = c }

        member s.configureService h (p : ContGenConfigParam) =
            match p with
            | SetToIdle -> { s with workState = Idle }
            | SetToCanGenerate ->
                h.startGenerate()
                { s with workState = CanGenerate }
            | RequestShutDown b ->
                match b with
                | false ->
                    let s1 =
                        s.running
                        |> Map.toList
                        |> List.fold (fun (acc : AsyncRunnerState) (i, _) -> acc.configureService h (CancelTask i)) s
                    { s1 with workState = ShuttingDown; queue = [] }
                | true -> { s with workState = ShuttingDown }
            | SetRunLimit v -> { s with runLimit = max 1 (min v Environment.ProcessorCount)}
            | CancelTask i ->
                match h.cancelProcess i with
                | true -> { s with running = s.running.tryRemove i}
                | false -> s

        member s.isShuttingDown =
            match s.workState with
            | Idle | CanGenerate -> false
            | ShuttingDown -> true

        member s.runModel h i p =
            h.runModel i p
            s


    type RunnerMessage =
        | StartQueue of AsyncRunner
        | CompleteQueue of AsyncRunner * list<RunInfo>
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner
        | Started of ProcessStartInfo
        | UpdateProgress of AsyncRunner * ProgressUpdateInfo
        | CompleteRun of AsyncRunner * ProcessStartInfo
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | ConfigureService of AsyncRunner * ContGenConfigParam
        | RunModel of AsyncRunner * ModelDataId * ModelCommandLineParam

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", ") + "]"

            match m with
            | StartQueue _ -> "StartQueue"
            | CompleteQueue _ -> "CompleteQueue"
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
        let mutable generating = 0
        let mutable msgCount = 0L
        let mutable runningModel = 0

        // Returns true if successfully acquired generating flag.
        let tryAcquireGeneratingImpl() = Interlocked.CompareExchange(&generating, 1, 0) = 0
        let releaseGeneratingImpl() = Interlocked.Exchange(&generating, 0) |> ignore

        let tryAcquireStartingModelImpl() = Interlocked.CompareExchange(&runningModel, 1, 0) = 0
        let releaseStartingModelImpl() = Interlocked.Exchange(&runningModel, 0) |> ignore

        let generateImpl (a : AsyncRunner) () = (fun() -> generatorInfo.generate () |> a.completeGenerate) |> toAsync |> Async.Start
        let startGenerateImpl(a : AsyncRunner) () = a.startGenerate |> toAsync |> Async.Start
        let startRunImpl (a : AsyncRunner) () = a.startRun |> toAsync |> Async.Start
        let getQueueImpl (a : AsyncRunner) () = (fun () -> generatorInfo.getQueue() |> a.queueObtained) |> toAsync |> Async.Start
        let removeFromQueueImpl i = (fun () -> generatorInfo.removeFromQueue i) |> toAsync |> Async.Start

        let startModelImpl (a : AsyncRunner) e =
            printfn "Starting modelId: %A..." e.modelId
            toAsync (fun () -> { notifyOnStarted = a.started; calledBackModelId = e.modelId; runQueueId = e.runQueueId } |> e.run |> a.completeRun) 
            |> Async.Start

        let cancelProcessImpl i =
            try
                (Process.GetProcessById i).Kill()
                true
            with
                | e -> false

        let runModelImpl (a : AsyncRunner) i p = 
            [ generatorInfo.runModel i p ] |> a.completeGenerate

        let h (a : AsyncRunner) =
            {
                cancelProcess = cancelProcessImpl
                generate = generateImpl a
                startGenerate = startGenerateImpl a
                startRun = startRunImpl a
                tryAcquireStartingModel = tryAcquireStartingModelImpl
                releaseStartingModel = releaseStartingModelImpl
                startModel = startModelImpl a
                getQueue = getQueueImpl a
                removeFromQueue = removeFromQueueImpl
                tryAcquireGenerating = tryAcquireGeneratingImpl
                releaseGenerating = releaseGeneratingImpl
                runModel = runModelImpl a
            }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : AsyncRunnerState) =
                    async
                        {
                            //printfn "s = %s" (s.ToString())
                            let! m = u.Receive()
                            Interlocked.Increment(&msgCount) |> ignore
                            //printfn "m = %s" (m.ToString())

                            match m with
                            | StartQueue a -> return! loop (s.onQueueStarting (h a))
                            | CompleteQueue (a, r) -> return! loop (s.OnQueueObtained (h a) r)
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

                loop AsyncRunnerState.defaultValue
                )

        member private this.completeGenerate (r : list<RunInfo>) : unit = CompleteGenerate (this, r) |> messageLoop.Post
        member private this.startRun () = StartRun this |> messageLoop.Post
        member private this.queueObtained (r : list<RunInfo>) = CompleteQueue (this, r) |> messageLoop.Post
        member private this.started p = Started p |> messageLoop.Post
        member private this.completeRun n = CompleteRun (this, n) |> messageLoop.Post

        member this.startQueue () : unit = StartQueue this |> messageLoop.Post
        member this.startGenerate () : unit = StartGenerate this |> messageLoop.Post
        member this.updateProgress p = UpdateProgress p |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState
        member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
        member this.start() = SetToCanGenerate |> this.configureService
        member this.stop() = SetToIdle |> this.configureService
        member this.runModel(m, p) = RunModel (this, m, p)


    /// http://www.fssnip.net/sw/title/RunProcess + some tweaks.
    /// We can't really fail here, especially if it runs under Windows service.
    let runProcOld (c : ProcessStartedCallBack) filename args startDir =
        let timer = Stopwatch.StartNew()

        let procStartInfo =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )

        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))

        let started =
            try
                p.Start()
            with
                | ex ->
                    // TODO kk:20190203 Here we need to notify AsyncRunner that starting the process has failed.
                    // Otherwise runningCount is not dereased.
                    ex.Data.Add("filename", filename)
                    //reraise()
                    false

        if not started
        then
            printfn "Failed to start process %s" filename

            {
                startInfo =
                    {
                        processId = -1
                        modelId = c.calledBackModelId
                        runQueueId = c.runQueueId
                    }
                exitCode = CannotFindSpecifiedFileException
                runTime = timer.ElapsedMilliseconds
                outputs = []
                errors = []
            }
        else 
            p.PriorityClass <- ProcessPriorityClass.BelowNormal

            let processId = p.Id

            printfn "Started %s with pid %i" p.ProcessName processId
            c.notifyOnStarted { processId = processId; modelId = c.calledBackModelId; runQueueId = c.runQueueId }

            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            p.WaitForExit()
            timer.Stop()
            printfn "Finished %s after %A." filename (timer.ElapsedMilliseconds |> double |> TimeSpan.FromMilliseconds)
            let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)

            {
                startInfo =
                    {
                        processId = processId
                        modelId = c.calledBackModelId
                        runQueueId = c.runQueueId
                    }
                exitCode = p.ExitCode
                runTime = timer.ElapsedMilliseconds
                outputs = cleanOut outputs
                errors = cleanOut errors
            }


    let runProc (c : ProcessStartedCallBack) filename args startDir =
        let timer = Stopwatch.StartNew()

        let procStartInfo =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )

        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))

        let started =
            try
                p.Start()
            with
                | ex ->
                    // TODO kk:20190203 Here we need to notify AsyncRunner that starting the process has failed.
                    // Otherwise runningCount is not dereased.
                    ex.Data.Add("filename", filename)
                    //reraise()
                    false

        if not started
        then
            printfn "Failed to start process %s" filename

            {
                processId = -1
                modelId = c.calledBackModelId
                runQueueId = c.runQueueId
            }
        else
            p.PriorityClass <- ProcessPriorityClass.BelowNormal

            let processId = p.Id

            printfn "Started %s with pid %i" p.ProcessName processId
            c.notifyOnStarted { processId = processId; modelId = c.calledBackModelId; runQueueId = c.runQueueId }

            {
                processId = processId
                modelId = c.calledBackModelId
                runQueueId = c.runQueueId
            }


    let getExeName exeName (ModelDataId modelId) =
        // TODO kk:20190208 - This is a fucked up NET way to get what's needed. Refactor when time permits.
        // See:
        //     https://stackoverflow.com/questions/278761/is-there-a-net-framework-method-for-converting-file-uris-to-paths-with-drive-le
        //     https://stackoverflow.com/questions/837488/how-can-i-get-the-applications-path-in-a-net-console-application
        let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        //p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName
        x + @"\" + exeName


    let runModel exeName (p : ModelCommandLineParam) (c : ProcessStartedCallBack) =
        let fullExeName = getExeName exeName (c.calledBackModelId)
        let commandLineParams = p.toCommandLine c.calledBackModelId
        runProc c fullExeName commandLineParams None

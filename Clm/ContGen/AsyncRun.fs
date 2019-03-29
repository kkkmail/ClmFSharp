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
            startedProcessId : int
            startedModelId : ModelDataId
            startedRunQueueId : RunQueueId
        }


    type ProcessResult =
        {
            exitedProcessId : int
            exitedModelId : ModelDataId
            exitedRunQueueId : RunQueueId
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
            run : ProcessStartedCallBack -> ProcessResult
            modelId : ModelDataId
            runQueueId : RunQueueId
            //clmTaskId : ClmTaskId
        }


    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : RunQueueId -> unit
            maxQueueLength : int
        }


    type AsyncRunnerHelper =
        {
            cancelProcess : int -> bool
            generate : unit -> Async<unit>
            startGenerate : unit -> Async<unit>
            startRun : list<RunInfo> -> Async<unit>
            startModels : list<RunInfo> -> Async<unit>
            getQueue : unit -> Async<unit>
            removeFromQueue : RunQueueId -> Async<unit>
            tryAcquireGenerating : unit -> bool
            releaseGenerating : unit -> unit
        }


    type AsyncRunnerState =
        {
            running : Map<int, RunningProcessInfo>
            queue : list<RunInfo>
            runLimit : int
            maxQueueLength : int
            workState : WorkState
        }

        member state.runningCount = state.running.Count

        static member defaultValue =
            {
                running = Map.empty
                queue = []
                runLimit = Environment.ProcessorCount
                maxQueueLength = 4
                workState = CanGenerate
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A, %A)" e.runningModelId e.runningProcessId e.started e.progress) |> String.concat ", "
            sprintf "{ running: [%s], queue: [%s], runLimit = %A, runningCount: %A, workState: %A }" r q s.runLimit s.runningCount s.workState

        member s.startGenerate h =
            match s.workState with
            | Idle -> s
            | CanGenerate ->
                if s.queue.Length <= s.maxQueueLength
                then
                    if h.tryAcquireGenerating() then h.generate() |> Async.Start
                s
            | ShuttingDown -> s

        member s.updateProgress (p : ProgressUpdateInfo) =
            match s.running.TryFind p.updatedProcessId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
                | Completed ->
                    { s with running = s.running.Remove p.updatedProcessId }
            | None ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    let e =
                        {
                            started = DateTime.Now
                            runningProcessId = p.updatedProcessId
                            runningModelId = p.updateModelId
                            progress = p.progress
                        }
                    { s with running = s.running.Add(p.updatedProcessId, e) }
                | Completed -> s

        member s.completeGenerate h r =
            let w() =
                h.startRun r |> Async.Start
                h.releaseGenerating()
                s

            match s.workState with
            | Idle -> w()
            | CanGenerate ->
                // Wait for the kick from the caller to attempt generating again.
                //if s.runningCount > 0 && s.runningCount < s.runLimit then h.startGenerate() |> Async.Start
                w()
            | ShuttingDown -> s

        member s.startQueue h =
            let w() =
                h.getQueue() |> Async.Start
                s

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.completeRun h (x : ProcessResult) =
            let w() =
                h.removeFromQueue x.exitedRunQueueId |> Async.Start

                match s.running.TryFind x.exitedProcessId with
                | Some _ ->
                    let p, q = partition s.runLimit s.queue (s.runningCount - 1)
                    h.startModels p |> Async.Start
                    { s with queue = q; running = s.running.Remove x.exitedProcessId }
                | None ->
                    let p, q = partition s.runLimit s.queue s.runningCount
                    h.startModels p |> Async.Start
                    { s with queue = q }

            match s.workState with
            | Idle -> w()
            | CanGenerate ->
                h.startGenerate() |> Async.Start
                w()
            | ShuttingDown -> { s with running = s.running.tryRemove x.exitedProcessId }

        member s.startRun h r =
            let w() =
                let p, q = partition s.runLimit (s.queue @ r) s.runningCount
                h.startModels p |> Async.Start
                { s with queue = q }

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.started p =
            printfn "Started: %A" p
            let r =
                {
                    started = DateTime.Now
                    runningProcessId = p.startedProcessId
                    runningModelId = p.startedModelId
                    progress = TaskProgress.create 0.0m
                }
            { s with running = s.running.Add(r.runningProcessId, r)}

        member s.getState reply =
            toAsync (fun () -> reply s) |> Async.Start
            s

        member s.configureService h (p : ContGenConfigParam) =
            match p with
            | SetToIdle -> { s with workState = Idle }
            | SetToCanGenerate ->
                h.startGenerate() |> Async.Start
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


    type RunnerMessage =
        | StartQueue of AsyncRunner
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner * list<RunInfo>
        | Started of ProcessStartInfo
        | UpdateProgress of ProgressUpdateInfo
        | CompleteRun of AsyncRunner * ProcessResult
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | ConfigureService of AsyncRunner * ContGenConfigParam

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", ") + "]"

            match m with
            | StartQueue _ -> "StartQueue"
            | StartGenerate _ -> "StartGenerate"
            | CompleteGenerate (_, r) -> "CompleteGenerate: " + (toStr r)
            | StartRun (_, r) -> "StartRun: " + (toStr r)
            | Started p -> "Started: " + (p.ToString())
            | UpdateProgress p -> "ProgressUpdate: " + (p.ToString())
            | CompleteRun (_, r) -> "CompleteRun: " + (r.ToString())
            | GetState _ -> "GetState"
            | ConfigureService _ -> "ConfigureService"


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        let mutable generating = 0

        // Returns true if successfully acquired generating flag.
        let tryAcquireGenerating() =
            let x = Interlocked.CompareExchange(&generating, 1, 0) 
            x = 0

        let releaseGenerating() =
            Interlocked.Exchange(&generating, 0) |> ignore


        let startModels (a : AsyncRunner) p =
            p
            |> List.map (fun e ->
                            printfn "Starting modelId: %A..." e.modelId
                            toAsync (fun () -> { notifyOnStarted = a.started; calledBackModelId = e.modelId; runQueueId = e.runQueueId } |> e.run |> a.completeRun) |> Async.Start)
            |> ignore

        let cancelProcess i =
            try
                (Process.GetProcessById i).Kill()
                true
            with
                | e -> false


        let h (a : AsyncRunner) =
            {
                cancelProcess = cancelProcess
                generate = fun () -> toAsync (fun () -> generatorInfo.generate () |> a.completeGenerate)
                startGenerate = fun () -> a.startGenerate |> toAsync
                startRun = fun r -> (fun () -> a.startRun r) |> toAsync
                startModels = fun r -> (fun () -> startModels a r) |> toAsync
                getQueue = fun () -> toAsync (fun () -> generatorInfo.getQueue() |> a.startRun)
                removeFromQueue = fun i -> (fun () -> generatorInfo.removeFromQueue i) |> toAsync
                tryAcquireGenerating = tryAcquireGenerating
                releaseGenerating = releaseGenerating
            }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : AsyncRunnerState) =
                    async
                        {
                            //printfn "s = %s" (s.ToString())
                            let! m = u.Receive()
                            //printfn "m = %s" (m.ToString())

                            match m with
                            | StartQueue a -> return! loop (s.startQueue (h a))
                            | StartGenerate a -> return! loop (s.startGenerate (h a))
                            | CompleteGenerate (a, r) -> return! loop (s.completeGenerate (h a) r)
                            | StartRun (a, r) -> return! loop (s.startRun (h a) r)
                            | Started p -> return! loop (s.started p)
                            | UpdateProgress p -> return! loop (s.updateProgress p)
                            | CompleteRun (a, x) -> return! loop (s.completeRun (h a) x)
                            | GetState r -> return! loop (s.getState r.Reply)
                            | ConfigureService (a, p) -> return! loop (s.configureService (h a) p)
                        }

                loop AsyncRunnerState.defaultValue
                )

        member private this.completeGenerate (r : list<RunInfo>) : unit = CompleteGenerate (this, r) |> messageLoop.Post
        member private this.startRun r : unit = StartRun (this, r) |> messageLoop.Post
        member private this.started p = Started p |> messageLoop.Post
        member private this.completeRun n = CompleteRun (this, n) |> messageLoop.Post

        member this.startQueue () : unit = StartQueue this |> messageLoop.Post
        member this.startGenerate () : unit = StartGenerate this |> messageLoop.Post
        member this.updateProgress p = UpdateProgress p |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState
        member this.configureService (p : ContGenConfigParam) = ConfigureService (this, p) |> messageLoop.Post
        member this.start() = SetToCanGenerate |> this.configureService
        member this.stop() = SetToIdle |> this.configureService


    /// http://www.fssnip.net/sw/title/RunProcess + some tweaks.
    /// We can't really fail here, especially if it runs under Windows service.
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
                exitCode = CannotFindSpecifiedFileException
                exitedModelId = c.calledBackModelId
                exitedRunQueueId = c.runQueueId
                runTime = timer.ElapsedMilliseconds
                outputs = []
                errors = []
                exitedProcessId = -1
            }
        else 
            p.PriorityClass <- ProcessPriorityClass.BelowNormal

            let processId = p.Id

            printfn "Started %s with pid %i" p.ProcessName processId
            c.notifyOnStarted { startedProcessId = processId; startedModelId = c.calledBackModelId; startedRunQueueId = c.runQueueId }

            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            p.WaitForExit()
            timer.Stop()
            printfn "Finished %s after %A." filename (timer.ElapsedMilliseconds |> double |> TimeSpan.FromMilliseconds)
            let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)

            {
                exitCode = p.ExitCode
                exitedModelId = c.calledBackModelId
                exitedRunQueueId = c.runQueueId
                runTime = timer.ElapsedMilliseconds
                outputs = cleanOut outputs
                errors = cleanOut errors
                exitedProcessId = processId
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

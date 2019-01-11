namespace ContGen

open System
open System.Diagnostics
open System.Threading.Tasks
open ContGenServiceInfo.ServiceInfo

module AsyncRun =

    let doAsyncTask  (f : unit->'a) = 
         async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }


    let partition maxVal q n =
        let (a, b) =
            q
            |> List.mapi (fun i e -> (i + n + 1, e))
            |> List.partition (fun (i, _) -> i <= maxVal)

        (a |> List.map snd, b |> List.map snd)


    //http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []


    type ProcessStartInfo =
        {
            startedProcessId : int
            startedModelId : int64
        }


    type ProcessResult =
        {
            exitedProcessId : int
            exitCode : int
            runTime : int64
            outputs : seq<string>
            errors : seq<string>
        }


    type ProcessStartedCallBack =
        {
            notifyOnStarted : ProcessStartInfo -> unit
            calledBackModelId : int64
        }


    type RunInfo =
        {
            run : ProcessStartedCallBack -> int64 -> ProcessResult
            modelId : int64
        }


    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            maxQueueLength : int
        }


    type AsyncRunnerState =
        {
            generating : bool
            runLimit : int
            maxQueueLength : int
            runningCount : int
            running : Map<int, RunningProcessInfo>
            queue : list<RunInfo>
            shuttingDown : bool
        }

        static member defaultValue =
            {
                generating = false
                runLimit = Environment.ProcessorCount
                maxQueueLength = 4
                runningCount = 0
                running = Map.empty
                queue = []
                shuttingDown = false
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A)" e.runningModelId e.runningProcessId e.started) |> String.concat ", "
            sprintf "{ generating: %A, runningCount: %A, queue: %A, [%s], running: [%s], shuttingDown: %A }" s.generating s.runningCount s.queue.Length q r s.shuttingDown

        member s.startGenerate generate =
            if s.generating || s.queue.Length >= s.maxQueueLength then s
            else
                generate |> Async.Start
                { s with generating = true }

        member s.updateProgress (p : ProgressUpdateInfo) =
            match s.running.TryFind p.updatedProcessId with
            | Some e ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    { s with running = s.running.Add(p.updatedProcessId, { e with progress = p.progress })}
                | Completed ->
                    { s with running = s.running.Remove p.updatedProcessId; runningCount = s.runningCount - 1 }
            | None ->
                match p.progress with
                | NotStarted | InProgress _ ->
                    let e =
                        {
                            started = DateTime.Now
                            runningProcessId = p.updatedProcessId
                            runningModelId = -1L
                            progress = p.progress
                        }
                    { s with running = s.running.Add(p.updatedProcessId, e); runningCount = s.runningCount + 1 }
                | Completed -> s

        member s.completeGenerate startRun startGenerate r =
            startRun r
            if s.runningCount < Environment.ProcessorCount && (s.shuttingDown |> not) then startGenerate()
            { s with generating = false }

        member s.completeRun startGenerate start (x : ProcessResult) =
            if s.shuttingDown then { s with runningCount = s.runningCount - 1 }
            else
                startGenerate()

                match s.running.TryFind x.exitedProcessId with
                | Some _ ->
                    let p, q = partition s.runLimit s.queue (s.runningCount - 1)
                    start p
                    { s with runningCount = s.runningCount + p.Length - 1; queue = q; running = s.running.Remove x.exitedProcessId }
                | None ->
                    let p, q = partition s.runLimit s.queue s.runningCount
                    start p
                    { s with runningCount = s.runningCount + p.Length; queue = q }

        member s.startRun start r =
            if s.shuttingDown then s
            else
                let p, q = partition s.runLimit (s.queue @ r) s.runningCount
                start p
                { s with runningCount = s.runningCount + p.Length; queue = q }

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

        member s.requestShutDown() =
            { s with shuttingDown = true }

        member s.getState reply =
            reply s
            s

        member s.stopGenerate() =
            s

        member s.configureService (p : ContGenConfigParam) =
            s


    type RunnerMessage =
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner * list<RunInfo>
        | Started of ProcessStartInfo
        | UpdateProgress of ProgressUpdateInfo
        | CompleteRun of AsyncRunner * ProcessResult
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | RequestShutDown
        | StopGenerate
        | ConfigureService of ContGenConfigParam

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", ") + "]"

            match m with
            | StartGenerate _ -> "StartGenerate"
            | CompleteGenerate (_, r) -> "CompleteGenerate: " + (toStr r)
            | StartRun (_, r) -> "StartRun: " + (toStr r)
            | Started p -> "Started: " + (p.ToString())
            | UpdateProgress p -> "ProgressUpdate: " + (p.ToString())
            | CompleteRun (_, r) -> "CompleteRun: " + (r.ToString())
            | GetState _ -> "GetState"
            | RequestShutDown -> "RequestShutDown"
            | StopGenerate -> "StopGenerate"
            | ConfigureService _ -> "ConfigureService"


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        let generate (a : AsyncRunner) =
            async { return! doAsyncTask(fun () -> generatorInfo.generate () |> a.completeGenerate) }


        let run (a : AsyncRunner) runner n =
            async { return! doAsyncTask(fun () -> runner n |> a.completeRun) }


        let start a p =
            p
            |> List.map (fun e ->
                            printfn "Starting modelId: %A..." e.modelId
                            run a (e.run { notifyOnStarted = a.started; calledBackModelId = e.modelId } ) e.modelId |> Async.Start)
            |> ignore


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : AsyncRunnerState) =
                    async
                        {
                            printfn "s = %s" (s.ToString())
                            let! m = u.Receive()
                            printfn "m = %s" (m.ToString())

                            match m with
                            | StartGenerate a -> return! loop (s.startGenerate (generate a))
                            | CompleteGenerate (a, r) -> return! loop (s.completeGenerate a.startRun a.startGenerate r)
                            | StartRun (a, r) -> return! loop (s.startRun (start a) r)
                            | Started p -> return! loop (s.started p)
                            | UpdateProgress p -> return! loop (s.updateProgress p)
                            | CompleteRun (a, x) -> return! loop (s.completeRun a.startGenerate (start a) x)
                            | GetState r -> return! loop (s.getState r.Reply)
                            | RequestShutDown -> return! loop (s.requestShutDown())
                            | StopGenerate -> return! loop (s.stopGenerate())
                            | ConfigureService p -> return! loop (s.configureService p)
                        }

                loop AsyncRunnerState.defaultValue
                )

        member this.startGenerate () = StartGenerate this |> messageLoop.Post
        member this.completeGenerate r = CompleteGenerate (this, r) |> messageLoop.Post
        member this.startRun r = StartRun (this, r) |> messageLoop.Post
        member this.started p = Started p |> messageLoop.Post
        member this.updateProgress p = UpdateProgress p |> messageLoop.Post
        member this.completeRun n = CompleteRun (this, n) |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState
        member this.stopGenerate() = StopGenerate |> messageLoop.Post
        member this.requestShutDown() = RequestShutDown |> messageLoop.Post
        member this.configureService (p : ContGenConfigParam) = ConfigureService p |> messageLoop.Post


    /// http://www.fssnip.net/sw/title/RunProcess + some tweaks.
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
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()

        if not started then failwithf "Failed to start process %s" filename
        else p.PriorityClass <- ProcessPriorityClass.BelowNormal
        let processId = p.Id

        printfn "Started %s with pid %i" p.ProcessName processId
        c.notifyOnStarted { startedProcessId = processId; startedModelId = c.calledBackModelId }

        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
        let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)

        {
            exitCode = p.ExitCode
            runTime = timer.ElapsedMilliseconds
            outputs = cleanOut outputs
            errors = cleanOut errors
            exitedProcessId = processId
        }

namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo

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
            runQueueId : int64
        }


    type GeneratorInfo =
        {
            generate : unit -> list<RunInfo>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : int64 -> unit
            maxQueueLength : int
        }


    type AsyncRunnerHelper =
        {
            cancelProcess : int -> bool
            generate : unit -> Async<unit>
            startGenerate : unit -> Async<unit>
            startRun : list<RunInfo> -> Async<unit>
            startModels : list<RunInfo> -> Async<unit>
            getQueue : unit -> list<RunInfo>
            removeFromQueue : int64 -> unit
        }


    type AsyncRunnerState =
        {
            generating : bool
            runLimit : int
            maxQueueLength : int
            runningCount : int
            running : Map<int, RunningProcessInfo>
            queue : list<RunInfo>
            workState : WorkState
        }

        static member defaultValue =
            {
                generating = false
                runLimit = Environment.ProcessorCount
                maxQueueLength = 4
                runningCount = 0
                running = Map.empty
                queue = []
                workState = CanGenerate
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            let r =
                s.running
                |> Map.toList
                |> List.map (fun (_, e) -> sprintf "(modelId: %A, processId: %A, started: %A)" e.runningModelId e.runningProcessId e.started) |> String.concat ", "
            sprintf "{ queue: %A, [%s], generating: %A, runningCount: %A, running: [%s], workState: %A }" s.queue.Length q s.generating s.runningCount r s.workState

        member s.startGenerate h =
            match s.workState with
            | Idle -> s
            | CanGenerate ->
                if s.generating || s.queue.Length >= s.maxQueueLength then s
                else
                    h.generate() |> Async.Start
                    { s with generating = true }
            | ShuttingDown -> s

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
                            runningModelId = p.updateModelId
                            progress = p.progress
                        }
                    { s with running = s.running.Add(p.updatedProcessId, e); runningCount = s.runningCount + 1 }
                | Completed -> s

        member s.completeGenerate h r =
            let w() =
                h.startRun r |> Async.Start
                { s with generating = false }

            match s.workState with
            | Idle -> w()
            | CanGenerate ->
                if s.runningCount < s.runLimit then h.startGenerate() |> Async.Start
                w()
            | ShuttingDown -> s

        member s.startQueue h =
            let w() =
                let r = h.getQueue()
                s.completeGenerate h r

            match s.workState with
            | Idle -> w()
            | CanGenerate -> w()
            | ShuttingDown -> s

        member s.completeRun h (x : ProcessResult) =
            let w() =
                match s.running.TryFind x.exitedProcessId with
                | Some _ ->
                    let p, q = partition s.runLimit s.queue (s.runningCount - 1)
                    h.startModels p |> Async.Start
                    { s with runningCount = s.runningCount + p.Length - 1; queue = q; running = s.running.Remove x.exitedProcessId }
                | None ->
                    let p, q = partition s.runLimit s.queue s.runningCount
                    h.startModels p |> Async.Start
                    { s with runningCount = s.runningCount + p.Length; queue = q }

            match s.workState with
            | Idle -> w()
            | CanGenerate ->
                h.startGenerate() |> Async.Start
                w()
            | ShuttingDown -> { s with runningCount = s.runningCount - 1; running = s.running.tryRemove x.exitedProcessId }

        member s.startRun h r =
            let w() =
                let p, q = partition s.runLimit (s.queue @ r) s.runningCount
                h.startModels p |> Async.Start
                { s with runningCount = s.runningCount + p.Length; queue = q }

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
            async { return! doAsyncTask(fun () -> reply s) } |> Async.Start
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
                | true -> { s with runningCount = max (s.runningCount - 1) 0; running = s.running.tryRemove i}
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
        let run (a : AsyncRunner) runner n =
            async { return! doAsyncTask(fun () -> runner n |> a.completeRun) }


        let startModels a p =
            p
            |> List.map (fun e ->
                            printfn "Starting modelId: %A..." e.modelId
                            run a (e.run { notifyOnStarted = a.started; calledBackModelId = e.modelId } ) e.modelId |> Async.Start)
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
                generate = fun () -> async { return! doAsyncTask(fun () -> generatorInfo.generate () |> a.completeGenerate) }
                startGenerate = fun () -> async { return! doAsyncTask a.startGenerate }
                startRun = fun r -> async { return! doAsyncTask (fun () -> (a.startRun r)) }
                startModels = fun r -> async { return! doAsyncTask (fun () -> (startModels a r)) }
                getQueue = generatorInfo.getQueue
                removeFromQueue = generatorInfo.removeFromQueue
            }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : AsyncRunnerState) =
                    async
                        {
                            printfn "s = %s" (s.ToString())
                            let! m = u.Receive()
                            printfn "m = %s" (m.ToString())

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

        member private this.completeGenerate r = CompleteGenerate (this, r) |> messageLoop.Post
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

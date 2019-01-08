namespace ContGen

open System
open System.Diagnostics
open System.Threading.Tasks

module AsyncRun =

    let doAsyncTask  (f : unit->'a) = 
         async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }


    //http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []


    type ProcessStartInfo =
        {
            processId : int
            modelId : int64
        }


    type ProcessResult =
        {
            processId : int
            exitCode : int
            runTime : int64
            outputs : seq<string>
            errors : seq<string>
        }


    type ProcessStartedCallBack =
        {
            notifyOnStarted : ProcessStartInfo -> unit
            modelId : int64
        }


    type RunInfo =
        {
            run : ProcessStartedCallBack -> int64 -> ProcessResult
            modelId : int64
        }


    type RunningProcessInfo =
        {
            started : DateTime
            processId : int
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
            runningCount : int
            running : list<RunningProcessInfo>
            queue : list<RunInfo>
            shuttingDown : bool
        }

        static member defaultValue =
            {
                generating = false
                runningCount = 0
                running = []
                queue = []
                shuttingDown = false
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            sprintf "{ generating: %A, running: %A, queue: %A, [%s], shuttingDown: %A }" s.generating s.runningCount s.queue.Length q s.shuttingDown


    type RunnerMessage =
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner * list<RunInfo>
        | Started of ProcessStartInfo
        | CompleteRun of AsyncRunner * ProcessResult
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | RequestShutDown

        override m.ToString() =
            let toStr (r : list<RunInfo>) = "[" + (r |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", ") + "]"

            match m with
            | StartGenerate _ -> "StartGenerate"
            | CompleteGenerate (_, r) -> "CompleteGenerate: " + (toStr r)
            | StartRun (_, r) -> "StartRun: " + (toStr r)
            | Started p -> "Started: " + (p.ToString())
            | CompleteRun (_, r) -> "CompleteRun: " + (r.ToString())
            | GetState _ -> "GetState"
            | RequestShutDown -> "RequestShutDown"


    and AsyncRunner (generatorInfo : GeneratorInfo) =
        let generate (a : AsyncRunner) =
            async
                {
                    return! doAsyncTask(fun () -> generatorInfo.generate () |> a.completeGenerate)
                }

        let run (a : AsyncRunner) runner n =
            async
                {
                    return! doAsyncTask(fun () -> runner n |> a.completeRun)
                }

        let partition q n =
            let (a, b) =
                q
                |> List.mapi (fun i e -> (i + n + 1, e))
                |> List.partition (fun (i, _) -> i <= Environment.ProcessorCount)

            (a |> List.map snd, b |> List.map snd)

        let start a (p :list<RunInfo>) =
            p
            |> List.map (fun e ->
                            printfn "Starting modelId: %A..." e.modelId
                            run a (e.run { notifyOnStarted = a.started; modelId = e.modelId } ) e.modelId |> Async.Start)
            |> ignore

        let logIfFailed x =
            ignore()

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            printfn "s = %s" (s.ToString())
                            let! m = u.Receive()
                            printfn "m = %s" (m.ToString())

                            match m with
                            | StartGenerate a ->
                                if s.generating || s.queue.Length >= generatorInfo.maxQueueLength then return! loop s
                                else
                                    generate a |> Async.Start
                                    return! loop { s with generating = true }
                            | CompleteGenerate (a, r) ->
                                a.startRun r
                                if s.runningCount < Environment.ProcessorCount && (s.shuttingDown |> not) then a.startGenerate()
                                return! loop { s with generating = false }
                            | StartRun (a, r) ->
                                if s.shuttingDown then return! loop s
                                else
                                    let p, q = partition (s.queue @ r) s.runningCount
                                    start a p
                                    return! loop { s with runningCount = s.runningCount + p.Length; queue = q }
                            | Started p ->
                                let r =
                                    {
                                        started = DateTime.Now
                                        processId = p.processId
                                        modelId = p.modelId
                                    }
                                return! loop { s with running = r :: s.running}
                            | CompleteRun (a, x) ->
                                if s.shuttingDown then return! loop { s with runningCount = s.runningCount - 1 }
                                else
                                    logIfFailed x
                                    a.startGenerate()
                                    let p, q = partition s.queue (s.runningCount - 1)
                                    start a p
                                    let r = removeFirst (fun e -> e.processId = x.processId) s.running
                                    return! loop { s with runningCount = s.runningCount + p.Length - 1; queue = q; running = r }
                            | GetState r ->
                                r.Reply s
                                return! loop s
                            | RequestShutDown ->
                                return! loop { s with shuttingDown = true }
                        }

                loop AsyncRunnerState.defaultValue
                )

        member this.startGenerate () = StartGenerate this |> messageLoop.Post
        member this.completeGenerate r = CompleteGenerate (this, r) |> messageLoop.Post
        member this.startRun r = StartRun (this, r) |> messageLoop.Post
        member this.started p = Started p |> messageLoop.Post
        member this.completeRun n = CompleteRun (this, n) |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState


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
        c.notifyOnStarted { processId = processId; modelId = c.modelId }

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
            processId = processId
        }

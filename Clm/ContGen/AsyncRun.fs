﻿namespace ContGen

open System
open System.Diagnostics
open System.Threading.Tasks

module AsyncRun =

    /// http://www.fssnip.net/sw/title/RunProcess
    let runProc filename args startDir =
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

        printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
        let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)
        cleanOut outputs, cleanOut errors


    let doAsyncTask  (f : unit->'a) = 
         async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }


    type RunInfo =
        {
            run : int64 -> int64
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
            running : int
            queue : list<RunInfo>
            shuttingDown : bool
        }

        static member defaultValue =
            {
                generating = false
                running = 0
                queue = []
                shuttingDown = false
            }

        override s.ToString() =
            let q = s.queue |> List.map (fun e -> e.modelId.ToString()) |> String.concat ", "
            sprintf "{ generating: %A, running: %A, queue: %A, [ %A ], shuttingDown: %A }" s.generating s.running s.queue.Length q s.shuttingDown


    type RunnerMessage =
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * list<RunInfo>
        | StartRun of AsyncRunner * list<RunInfo>
        | CompleteRun of AsyncRunner * int64
        | GetState of AsyncReplyChannel<AsyncRunnerState>
        | RequestShutDown


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

        let start a p =
            p
            |> List.map (fun e ->
                            printfn "Starting modelId: %A..." e.modelId
                            run a e.run e.modelId |> Async.Start)
            |> ignore

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            printfn "s = %s, Environment.ProcessorCount = %A" (s.ToString()) Environment.ProcessorCount
                            let! m = u.Receive()
                            printfn "m = %A" m

                            match m with
                            | StartGenerate a ->
                                if s.generating || s.running >= generatorInfo.maxQueueLength then return! loop s
                                else
                                    generate a |> Async.Start
                                    return! loop { s with generating = true }
                            | CompleteGenerate (a, r) ->
                                a.startRun r
                                if s.running < Environment.ProcessorCount && (s.shuttingDown |> not) then a.startGenerate()
                                return! loop { s with generating = false }
                            | StartRun (a, r) ->
                                if s.shuttingDown then return! loop s
                                else
                                    let p, q = partition (s.queue @ r) s.running
                                    start a p
                                    return! loop { s with running = s.running + p.Length; queue = q }
                            | CompleteRun (a, _) ->
                                if s.shuttingDown then return! loop { s with running = s.running - 1 }
                                else
                                    a.startGenerate()
                                    let p, q = partition s.queue (s.running - 1)
                                    start a p
                                    return! loop { s with running = s.running + p.Length - 1; queue = q }
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
        member this.completeRun n = CompleteRun (this, n) |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState

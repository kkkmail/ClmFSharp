namespace ContGen

open System
open System.Diagnostics
open System.Threading.Tasks

module AsyncRun =
    open Clm

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

        if not started then
            failwithf "Failed to start process %s" filename

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


    type RunnerState =
        {
            generating : bool
            running : int
            shuttingDown : bool
        }

        static member defaultValue =
            {
                generating = false
                running = 0
                shuttingDown = false
            }


    type Runner =
        {
            generate : unit -> int64
            run : int64 -> int64
        }


    type RunnerMessage =
        | StartGenerate of AsyncRunner
        | CompleteGenerate of AsyncRunner * int64
        | StartRun of AsyncRunner * int64
        | CompleteRun of AsyncRunner * int64
        | GetState of AsyncReplyChannel<RunnerState>
        | RequestShutDown


    and AsyncRunner (runner : Runner) =
        let generate (a : AsyncRunner) =
            async
                {
                    return! doAsyncTask(fun () -> runner.generate () |> a.completeGenerate)
                }

        let run (a : AsyncRunner) n =
            async
                {
                    return! doAsyncTask(fun () -> runner.run n |> a.completeRun)
                }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            let! m = u.Receive()

                            match m with
                            | StartGenerate a ->
                                if s.generating
                                then return! loop s
                                else
                                    generate a |> Async.Start
                                    return! loop { s with generating = true }
                            | CompleteGenerate (a, n) ->
                                a.startRun n
                                if s.running < Environment.ProcessorCount then a.startGenerate()
                                return! loop { s with generating = false }
                            | StartRun (a, n) ->
                                run a n |> Async.Start
                                return! loop { s with running = s.running + 1 }
                            | CompleteRun (a, n) ->
                                if s.generating |> not then a.startGenerate()
                                return! loop { s with running = s.running - 1 }
                            | GetState r ->
                                r.Reply s
                                return! loop s
                            | RequestShutDown ->
                                return! loop { s with shuttingDown = true }
                        }

                loop RunnerState.defaultValue
                )

        member this.startGenerate () = StartGenerate this |> messageLoop.Post
        member this.completeGenerate n = CompleteGenerate (this, n) |> messageLoop.Post
        member this.startRun n = StartRun (this, n) |> messageLoop.Post
        member this.completeRun n = CompleteRun (this, n) |> messageLoop.Post
        member this.getState () = messageLoop.PostAndReply GetState


    [<Literal>]
    let ModelCommandLineParamName = "ModelCommandLineParam"

    type ModelCommandLineParam =
        {
            tEnd : double
            y0 : double
            useAbundant : bool option
        }

        static member defaultValue =
            {
                tEnd = 100_000.0
                y0 = 10.0
                useAbundant = None
            }

        override this.ToString() =
            [
                this.tEnd.ToString() |> Some
                this.y0.ToString() |> Some
                this.useAbundant |> Option.bind (fun e -> (if e then "1" else "") |> Some)
            ]
            |> List.choose id
            |> String.concat " "

        static member name = ModelCommandLineParamName
        static member variableName = ModelCommandLineParam.name |> Distributions.toVariableName

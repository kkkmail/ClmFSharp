namespace ContGen

open System
open System.Diagnostics
open System.Threading
open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.SettingsExt
open System.Data.SqlClient
open System.Text

open Clm.VersionInfo
open Clm.DataLocation
open Clm.Generator.ClmModel
open Clm.Generator
open System.Threading.Tasks

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder =

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


    type ModelCommandLineParam =
        {
            tEnd : double option
            y0 : double option
            useAbundant : bool option
        }

        static member defaultValue =
            {
                tEnd = Some 100_000.0
                y0 = Some 10.0
                useAbundant = None
            }

        override this.ToString() =
            [
                this.tEnd |> Option.bind (fun e -> e.ToString() |> Some)
                this.y0 |> Option.bind (fun e -> e.ToString() |> Some)
                this.useAbundant |> Option.bind (fun e -> e.ToString() |> Some)
            ]
            |> List.choose id
            |> String.concat " "


    type ModelRunnerParam = 
        {
            connectionString : string
            rootBuildFolder : string
            buildTarget : string
            exeName : string
        }

        static member defaultValue =
            {
                connectionString = ClmConnectionString
                rootBuildFolder = @"C:\Temp\Clm\"
                buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"
                exeName = @"SolverRunner.exe"
            }


    type ModelRunner (p : ModelRunnerParam) =
        let getBuildDir modelId = p.rootBuildFolder + (toModelName modelId) + @"\"
        let getExeName modelId = p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName


        let getModelId () =
            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn
            getNewModelDataId conn


        let loadParams seeder modelId =
            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn

            let m = loadSettings conn

            match ModelGenerationParams.tryGet m seeder with
            | Some m ->
                { m with modelLocationData = { m.modelLocationData with modelName = ConsecutiveName modelId; useDefaultModeData = true } }
                |> Some
            | None -> None


        let generateModel modelGenerationParams =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams
            let code = model.generateCode()
            printfn "... completed."
            code


        let saveModel (code : list<string>) (pm : ModelGenerationParams) modelId =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

            let m =
                {
                    modelId = modelId
                    numberOfAminoAcids = pm.numberOfAminoAcids
                    maxPeptideLength = pm.maxPeptideLength
                    seedValue = pm.seedValue
                    fileStructureVersion = pm.fileStructureVersionNumber
                    modelData = sb.ToString()
                }

            use conn = new SqlConnection (p.connectionString)
            openConnIfClosed conn
            tryUpdateModelData conn m


        let compileModel modelId =
            let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
            Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

            // Properties
            let buildDir = getBuildDir modelId

            // Targets
            Target.create "Clean" (fun _ ->
              Shell.cleanDir buildDir
            )

            Target.create "BuildApp" (fun _ ->
              [ p.buildTarget ]
                |> MSBuild.runRelease id buildDir "Build"
                |> Trace.logItems "AppBuild-Output: "
            )

            Target.create "Default" (fun _ ->
              Trace.trace "Built completed."
            )

            "Clean"
              ==> "BuildApp"
              ==> "Default"
              |> ignore

            Target.runOrDefault "Default"


        let runModel modelId (p : ModelCommandLineParam) =
            let exeName = getExeName modelId
            let commandLineParams = p.ToString()
            runProc exeName commandLineParams None |> ignore
            modelId


        member fake.sucks() = 0


    //type Runner (command : string) =
    //    let myProcess_Exited(e : System.EventArgs) = 
    //        printfn "Completed."

    //    let myProcess = new Process()
    //    let startInfo = new ProcessStartInfo()

    //    do myProcess.StartInfo.FileName <- @"C:\Temp\WTF\SolverRunner.exe"
    //    do myProcess.StartInfo.Arguments <- "10000 10"
    //    do myProcess.EnableRaisingEvents <- true
    //    do myProcess.Exited.Add(myProcess_Exited)


    //    member this.run() =
    //        myProcess.Start()

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

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Builder =
    open Clm.Generator

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


    //type IUpdater<'P, 'S> = 
    //    abstract member init : unit -> 'S
    //    abstract member update : 'P -> 'S -> 'S
    //    abstract member remove : 'P -> 'S -> 'S


    //type Updater<'T> = MailboxProcessor<'T>


    //type UpdatatableStorage<'P, 'S> = 
    //  | UpdateContent of 'P
    //  | GetContent of AsyncReplyChannel<'S>
    //  | RemoveContent of 'P


    //type AsyncUpdater<'P, 'S> (updater : IUpdater<'P, 'S>) =
    //    let chat = Updater.Start(fun u -> 
    //      let rec loop s = async {
    //        let! m = u.Receive()

    //        match m with 
    //        | UpdateContent p -> 
    //            return! loop (updater.update p s)
    //        | GetContent r -> 
    //            r.Reply s
    //            return! loop s 
    //        | RemoveContent p -> 
    //            return! loop (updater.remove p s) }

    //      updater.init () |> loop)

    //    member this.updateContent p = UpdateContent p |> chat.Post
    //    member this.getContent () = chat.PostAndReply GetContent


    //type RunnerMessage =
    //    | StartRun
    //    | CompleteRun

    //type AsyncUpdater (updater) =
    //    let chat = MailboxProcessor.Start(fun u -> 
    //      let rec loop s = async {
    //        let! m = u.Receive()

    //        match m with 
    //        | StartRun -> 
    //            return! loop (updater s)
    //        //| CompleteRun r -> 
    //        //    r.Reply s
    //        //    return! loop s 
    //        | CompleteRun -> 
    //            return! loop (updater s) }

    //      updater.init () |> loop)

    //    member this.updateContent p = UpdateContent p |> chat.Post
    //    member this.getContent () = chat.PostAndReply GetContent

    type MessageBasedCounter () =

        static let updateState (count,sum) msg = 

            // increment the counters and...
            let newSum = sum + msg
            let newCount = count + 1
            printfn "Count is: %i. Sum is: %i" newCount newSum 

            // ...emulate a short delay
            //Utility.RandomSleep()

            // return the new state
            (newCount,newSum)

        // create the agent
        static let agent = MailboxProcessor.Start(fun inbox -> 

            // the message processing function
            let rec messageLoop oldState = async{

                // read a message
                let! msg = inbox.Receive()

                // do the core logic
                let newState = updateState oldState msg

                // loop to top
                return! messageLoop newState
                }

            // start the loop 
            messageLoop (0,0)
            )

        // public interface to hide the implementation
        static member Add i = agent.Post i



    type X (connStr) =
        let getModelId () = 
            use conn = new SqlConnection (connStr)
            openConnIfClosed conn
            getNewModelDataId conn


        //let modelId = getModelId ()


        let loadParams seeder modelId =
            use conn = new SqlConnection (connStr)
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


        let saveModel (code : list<string>) (p : ModelGenerationParams) modelId =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + FSharpCodeExt.Nl)) |> ignore

            let m =
                {
                    modelId = modelId
                    numberOfAminoAcids = p.numberOfAminoAcids
                    maxPeptideLength = p.maxPeptideLength
                    seedValue = p.seedValue
                    fileStructureVersion = p.fileStructureVersionNumber
                    modelData = sb.ToString()
                }

            use conn = new SqlConnection (connStr)
            openConnIfClosed conn
            tryUpdateModelData conn m


        let compile modelId =
            let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
            Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

            // Properties
            let buildDir = @"C:\Temp\Clm\" + (toModelName modelId) + @"\"
            let buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"

            // Targets
            Target.create "Clean" (fun _ ->
              Shell.cleanDir buildDir
            )

            Target.create "BuildApp" (fun _ ->
              [ buildTarget ]
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


        // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.exited?view=netframework-4.7.2
        let runModel modelId =
            0


        member fake.sucks() = 0


    type Runner (command : string) =
        let myProcess_Exited(e : System.EventArgs) = 
            printfn "Completed."

        let myProcess = new Process()
        let startInfo = new ProcessStartInfo()

        do myProcess.StartInfo.FileName <- @"C:\Temp\WTF\SolverRunner.exe"
        do myProcess.StartInfo.Arguments <- "10000 10"
        do myProcess.EnableRaisingEvents <- true
        do myProcess.Exited.Add(myProcess_Exited)


        member this.run() =
            myProcess.Start()

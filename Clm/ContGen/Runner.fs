namespace ContGen

open System
open ClmSys.GeneralData
open ClmSys.Retry
open Clm.DataLocation
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.SettingsExt
open Clm.Generator.SettingGenExt
open System.Text
open Clm.Generator.ClmModel
open AsyncRun

open Fake.DotNet
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
//open Fake.IO.Globbing.Operators //enables !! and globbing

module Runner =

    type ModelRunnerParam =
        {
            connectionString : ConnectionString
            rootBuildFolder : string
            buildTarget : string
            exeName : string
        }

        static member defaultValue =
            {
                connectionString = clmConnectionString
                rootBuildFolder = DefaultRootFolder + @"bin\"
                buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"
                exeName = @"SolverRunner.exe"
            }


    type ModelRunner (p : ModelRunnerParam) =
        let rnd = new Random()
        let getBuildDir (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\"
        let getExeName (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName
        let getRandomSeeder (seed : int option) = getRandomSeeder rnd seed
        let getDeterministicSeeder (seed : int option) = getDeterministicSeeder rnd seed

        let logError e = printfn "Error: %A" e
        let tryDbFun f = tryDbFun logError (p.connectionString) f
        let getModelId () = tryDbFun getNewModelDataId


        let loadParams seeder (ModelDataId modelId) =
            match tryDbFun loadSettings with
                | Some m ->
                    match ModelGenerationParams.tryGet m seeder [] with
                    | Some q ->
                        (
                            { q with
                                modelLocationData =
                                    { q.modelLocationData with
                                        modelName = ConsecutiveName modelId
                                        useDefaultModeData = true
                                    }
                                seedValue = rnd.Next() |> Some
                            },
                            ModelCommandLineParam.getValues m []
                        )
                        |> Some
                    | None -> None
                | None -> None


        let generateModel modelGenerationParams =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams
            let code = model.generateCode()
            printfn "... completed."
            code


        let saveModel (code : list<string>) (pm : ModelGenerationParams) modelDataId =
            let sb = new StringBuilder()
            code |> List.map(fun s -> sb.Append (s + Nl)) |> ignore

            let m =
                {
                    modelDataId = modelDataId
                    numberOfAminoAcids = pm.numberOfAminoAcids
                    maxPeptideLength = pm.maxPeptideLength
                    seedValue = pm.seedValue
                    fileStructureVersion = pm.fileStructureVersionNumber
                    modelData = sb.ToString()
                    defaultSetIndex = pm.defaultSetIndex
                }

            tryDbFun (tryUpdateModelData m)


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
                |> MSBuild.runRelease (fun p -> { p with Properties = [ "platform", "x64" ] } ) buildDir "Build"
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


        let runModel (p : ModelCommandLineParam) (c : ProcessStartedCallBack) =
            let exeName = getExeName (c.calledBackModelId)
            let commandLineParams = p.ToString()
            runProc c exeName commandLineParams None


        let getQueueId (p : ModelCommandLineParam) modelId =
            match tryDbFun (saveRunQueueEntry p modelId) with
            | Some q -> q
            | None -> RunQueueId -1L


        let generate() =
            try
                match getModelId () with
                    | Some modelId ->
                        let cmd i e = { e with saveModelSettings = (i = 0) } // Save model settings on the first run.

                        match loadParams getRandomSeeder modelId with
                        | Some (p, r) ->
                            let code = generateModel p
                            match saveModel code p modelId with
                            | Some true ->
                                compileModel modelId
                                r |> List.mapi (fun i e -> 
                                                    {
                                                        run = cmd i e |> runModel
                                                        modelId = modelId
                                                        runQueueId = getQueueId e modelId
                                                    })
                            | Some false ->
                                logError (sprintf "Cannot save modelId: %A." modelId)
                                []
                            | None ->
                                logError (sprintf "Exception occurred while saving modelId: %A." modelId)
                                []
                        | None ->
                            logError (sprintf "Cannot load parameters for modelId: %A." modelId)
                            []
                    | None ->
                        logError (sprintf "Cannot get modelId.")
                        []
            with
                | e ->
                    logError (sprintf "Exception: %A" e)
                    []


        let getQueue () =
            match tryDbFun loadRunQueue with
            | Some q -> q |> List.map (fun e ->
                                        {
                                            run = { e.modelCommandLineParam with saveModelSettings = true } |> runModel
                                            modelId = e.info.modelDataId
                                            runQueueId = e.runQueueId
                                        })
            | None -> []


        let removeFromQueue runQueueId =
            match tryDbFun (deleteRunQueueEntry runQueueId) with
            | Some v -> ignore()
            | None ->
                logError (sprintf "Cannot delete runQueueId = %A" runQueueId)
                ignore()


        let createGeneratorImpl() =
            {
                generate = generate
                getQueue = getQueue
                removeFromQueue = removeFromQueue
                maxQueueLength = 4
            }


        member __.createGenerator = createGeneratorImpl


    let createRunner p =
        let r = ModelRunner p
        let a = r.createGenerator() |> AsyncRunner
        a.startQueue()
        a


    let saveDefaults connectionString (d, i) n m =
        let rnd = new Random()
        truncateSettings connectionString
        let p = AllParams.getDefaultValue rnd d n m i

        let settings =
            []
            |> p.modelGenerationParams.setValue []
            |> ModelCommandLineParam.setValues p.modelCommandLineParams []

        saveSettings settings connectionString

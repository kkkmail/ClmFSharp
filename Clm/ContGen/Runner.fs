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
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CalculationData
open Clm.CommandLine
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
            saveModelCode : bool
        }

        static member defaultValue =
            {
                connectionString = clmConnectionString
                rootBuildFolder = DefaultRootFolder + @"bin\"
                buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"
                exeName = SolverRunnerName
                saveModelCode = false
            }


    type ModelRunner (p : ModelRunnerParam) =
        let rnd = new Random()
        let getBuildDir (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\"
        let getExeName (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName

        let logError e = printfn "Error: %A" e
        let tryDbFun f = tryDbFun logError (p.connectionString) f
        let getModelId () = tryDbFun getNewModelDataId


        let loadParams (ModelDataId modelId) =
            match tryDbFun loadSettings with
                | Some m ->
                    match ModelGenerationParams.tryGet m [] with
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


        let generateModel (modelGenerationParams : ModelGenerationParams) =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel modelGenerationParams

            match p.saveModelCode with
            | true ->
                printfn "Saving model code..."
                model.generateCode() |> ignore
                printfn "... completed."
            | false -> printfn "NOT saving model code."

            model.getModelData


        let saveModel getModelData =
            getModelData() |> tryUpdateModelData |> tryDbFun


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
            let commandLineParams = p.toCommandLine c.calledBackModelId
            runProc c exeName commandLineParams None


        let getQueueId (p : ModelCommandLineParam) modelId =
            match tryDbFun (saveRunQueueEntry modelId p) with
            | Some q -> q
            | None -> RunQueueId -1L


        let generateImpl() =
            try
                match getModelId () with
                    | Some modelId ->
                        match loadParams modelId with
                        | Some (p, r) ->
                            
                            match generateModel p |> saveModel with
                            | Some true ->
                                compileModel modelId
                                r |> List.map (fun e -> 
                                                    {
                                                        run = runModel e
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
                                            run = e.modelCommandLineParam |> runModel
                                            modelId = e.info.modelDataId
                                            runQueueId = e.runQueueId
                                        })
            | None -> []


        let removeFromQueue runQueueId =
            match tryDbFun (deleteRunQueueEntry runQueueId) with
            | Some _ -> ignore()
            | None ->
                logError (sprintf "Cannot delete runQueueId = %A" runQueueId)
                ignore()


        let createGeneratorImpl() =
            {
                generate = generateImpl
                getQueue = getQueue
                removeFromQueue = removeFromQueue
                maxQueueLength = 4
            }


        member __.createGenerator = createGeneratorImpl
        member __.generate = generateImpl


    let createRunner p =
        let r = ModelRunner p
        let a = r.createGenerator() |> AsyncRunner
        a.startQueue()
        a


    let createOneTimeGenerator p =
        let r = ModelRunner p
        r.generate


    let saveDefaults connectionString (d, i) n m =
        let rnd = new Random()
        truncateSettings connectionString
        let p = AllParams.getDefaultValue rnd d n m i

        let settings =
            []
            |> p.modelGenerationParams.setValue []
            |> ModelCommandLineParam.setValues p.modelCommandLineParams []

        saveSettings settings connectionString

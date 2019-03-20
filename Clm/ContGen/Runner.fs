namespace ContGen

open System
open ClmSys.GeneralData
open ClmSys.Retry
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CommandLine
open AsyncRun

// ! Do not delete !
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
        let getBuildDir (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\"

        let getExeName (ModelDataId modelId) =
            // TODO kk:20190208 - This is a fucked up NET way to get what's needed. Refactor when time permits.
            // See:
            //     https://stackoverflow.com/questions/278761/is-there-a-net-framework-method-for-converting-file-uris-to-paths-with-drive-le
            //     https://stackoverflow.com/questions/837488/how-can-i-get-the-applications-path-in-a-net-console-application
            let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath
            //p.rootBuildFolder + (toModelName modelId) + @"\" + p.exeName
            x + @"\" + p.exeName

        let logError e = printfn "Error: %A" e
        let tryDbFun f = tryDbFun logError (p.connectionString) f
        let getModelId () = tryDbFun getNewModelDataId


        let tryLoadParams () = tryDbFun tryloadAllParams |> Option.bind id


        let generateModel (modelGenerationParams : ModelGenerationParams) modelDataId =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel (modelGenerationParams, modelDataId)

            match p.saveModelCode with
            | true ->
                printfn "Saving model code..."
                model.generateCode() |> ignore
                printfn "... completed."
            | false -> printfn "NOT saving model code."

            model.getModelData


        let saveModel getModelData =
            getModelData() |> tryUpdateModelData |> tryDbFun


        // kk:20190208 - Do not delete. It was not straightforward to tweak all the parameters.
        //let compileModel modelId =
        //    let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
        //    Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
        //
        //    // Properties
        //    let buildDir = getBuildDir modelId
        //
        //    // Targets
        //    Target.create "Clean" (fun _ ->
        //      Shell.cleanDir buildDir
        //    )
        //
        //    Target.create "BuildApp" (fun _ ->
        //      [ p.buildTarget ]
        //        |> MSBuild.runRelease (fun p -> { p with Properties = [ "platform", "x64" ] } ) buildDir "Build"
        //        |> Trace.logItems "AppBuild-Output: "
        //    )
        //
        //    Target.create "Default" (fun _ ->
        //      Trace.trace "Built completed."
        //    )
        //
        //    "Clean"
        //      ==> "BuildApp"
        //      ==> "Default"
        //      |> ignore
        //
        //    Target.runOrDefault "Default"


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
                        match tryLoadParams() with
                        | Some a ->
                            match generateModel a.modelGenerationParams modelId |> saveModel with
                            | Some true ->
                                //compileModel modelId
                                a.modelCommandLineParams |> List.map (fun e ->
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


    let saveDefaults connectionString d n m =
        truncateAllParams connectionString
        let p = AllParams.getDefaultValue d n m
        saveAllParams p connectionString

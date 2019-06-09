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
open Clm.CalculationData
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
            serviceAccessInfo : ServiceAccessInfo
        }

        static member defaultValue i =
            {
                connectionString = clmConnectionString
                rootBuildFolder = DefaultRootFolder + @"bin\"
                buildTarget = __SOURCE_DIRECTORY__ + @"\..\SolverRunner\SolverRunner.fsproj"
                exeName = SolverRunnerName
                saveModelCode = false
                serviceAccessInfo = i
            }


    type ModelRunner (p : ModelRunnerParam) =

        let logError e = printfn "Error: %A" e
        let tryDbFun f = tryDbFun logError (p.connectionString) f
        let getModelDataId() = Guid.NewGuid() |> ModelDataId

        let runModel e c =
            {
                exeName = p.exeName
                commandLineParam = e
                callBack = c
                minUsefulEe = p.serviceAccessInfo.minUsefulEe
            }
            |> runModel

        let getBuildDir (ModelDataId modelId) = p.rootBuildFolder + (toModelName modelId) + @"\"


        let tryLoadParams (c : ClmTask) : AllParams option =
            fun d -> tryDbFun (tryLoadClmDefaultValue d) |> Option.bind id
            |> AllParams.tryGetDefaultValue c


        let generateModel (modelGenerationParams : ModelGenerationParams) modelDataId clmTaskId =
            printfn "Creating model..."
            printfn "Starting at: %A" DateTime.Now

            let model = ClmModel (modelGenerationParams, modelDataId, clmTaskId)

            match p.saveModelCode with
            | true ->
                printfn "Saving model code..."
                model.generateCode() |> ignore
                printfn "... completed."
            | false -> printfn "NOT saving model code."

            model.getModelData


        let saveModelData modelData = modelData |> tryUpdateModelData |> tryDbFun
        let saveModel getModelData = getModelData() |> saveModelData


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


        let getQueueId (p : ModelCommandLineParam) modelId =
             match tryDbFun (saveRunQueueEntry modelId p) with
             | Some q -> q
             | None -> failwith "getQueueId - cannot get run queue id..." // TODO kk:20190531 - This is not so good! refactor.


        let updateTask (c : ClmTask) =
            tryDbFun (tryUpdateClmTask c)
            |> ignore


        let tryAddClmTask c =
            tryDbFun (addClmTask c)


        let tryLoadClmTask i t =
            match tryDbFun (tryLoadClmTask i t) with
            | Some (Some c) -> Some c
            | _ -> None


        let tryLoadModelData i m =
            match tryDbFun (tryLoadModelData i m) with
            | Some (Some c) -> Some c
            | _ -> None


        let generateImpl (c : ClmTask) =
            try
                let modelDataId = getModelDataId()

                match tryLoadParams c with
                | Some a ->
                    match generateModel a.modelGenerationParams modelDataId c.clmTaskInfo.clmTaskId |> saveModel with
                    | Some true ->
                        updateTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }

                        a.modelCommandLineParams |> List.map (fun e ->
                                                    {
                                                        run = runModel e
                                                        modelDataId = modelDataId
                                                        runQueueId = getQueueId e modelDataId
                                                    })
                    | Some false ->
                        logError (sprintf "Cannot save modelId: %A." modelDataId)
                        []
                    | None ->
                        logError (sprintf "Exception occurred while saving modelId: %A." modelDataId)
                        []
                | None ->
                    logError (sprintf "Cannot load parameters for modelId: %A." modelDataId)
                    []
            with
                | e ->
                    logError (sprintf "Exception: %A" e)
                    []


        let generateAll i () =
            match tryDbFun (loadIncompleteClmTasks i) with
            | Some c -> c |> List.map generateImpl |> List.concat
            | None -> []


        let getQueue i () =
            match tryDbFun (loadRunQueue i) with
            | Some q -> q |> List.map (fun e ->
                                        {
                                            run = e.modelCommandLineParam |> runModel
                                            modelDataId = e.info.modelDataId
                                            runQueueId = e.runQueueId
                                        })
            | None -> []


        let removeFromQueue runQueueId =
            match tryDbFun (deleteRunQueueEntry runQueueId) with
            | Some _ -> ignore()
            | None ->
                logError (sprintf "Cannot delete runQueueId = %A" runQueueId)
                ignore()


        let runModel j m p =
            match tryLoadModelData j m with
            | Some parent ->
                match tryLoadClmTask j parent.clmTaskInfo.clmTaskId |> Option.bind tryLoadParams, parent.data with
                | Some a, OwnData d ->
                    let t =
                        {
                            clmTaskInfo =
                                {
                                    clmTaskId = Guid.NewGuid() |> ClmTaskId
                                    clmDefaultValueId = a.modelGenerationParams.clmDefaultValueId
                                    numberOfAminoAcids = a.modelGenerationParams.numberOfAminoAcids
                                    maxPeptideLength = a.modelGenerationParams.maxPeptideLength
                                }
                            commandLineParams = [ p ]
                            numberOfRepetitions = 1
                            remainingRepetitions = 0
                            createdOn = DateTime.Now
                        }

                    match tryAddClmTask t with
                    | Some t1 ->
                        let modelDataId = getModelDataId()

                        let m1 =
                            {
                                modelDataId = modelDataId
                                clmTaskInfo = t1.clmTaskInfo
                                data = (parent.modelDataId, d) |> ParentProvided
                            }

                        match saveModelData m1 with
                        | Some true ->
                            let r =
                                {
                                    run = runModel p
                                    modelDataId = modelDataId
                                    runQueueId = getQueueId p modelDataId
                                }
                            Some r
                        | _ -> None
                    | None -> None
                | _ -> None
            | None -> None


        let createGeneratorImpl() =
            {
                generate = generateAll p.serviceAccessInfo
                getQueue = getQueue p.serviceAccessInfo
                removeFromQueue = removeFromQueue
                maxQueueLength = 4
                runModel = runModel p.serviceAccessInfo
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

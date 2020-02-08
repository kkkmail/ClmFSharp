namespace ContGen

open System
open ClmSys
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CommandLine
open Clm.CalculationData
open AsyncRun
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner
open ClmSys.Logging
open ClmSys.MessagingData
open ClmSys.SolverRunnerData
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmErrors
open ClmSys.RunnerErrors

module Runner =

    let private toError g f = f |> g |> RunnerErr |> Error
    let private addError g f e = ((f |> g |> RunnerErr) + e) |> Error


    type ModelRunnerData =
        {
            exeName : string
            saveModelCode : bool
            minUsefulEe : MinUsefulEe
            serviceAccessInfo : SolverRunnerAccessInfo
            runnerProxy : RunnerProxy
        }

        static member defaultValue i p =
            {
                exeName = SolverRunnerName
                saveModelCode = false
                serviceAccessInfo = i
                minUsefulEe = MinUsefulEe.defaultValue
                runnerProxy = p
            }


    //let getModelDataId() = Guid.NewGuid() |> ModelDataId
    let localWorkerNodeId = Guid.Empty |> MessagingClientId |> WorkerNodeId


    let runModel p e c =
        {
            exeName = p.exeName
            commandLineParam = e
            callBackInfo = c
        }
        |> p.runnerProxy.runModel


    //let generateModel p (modelGenerationParams : ModelGenerationParams) modelDataId clmTaskId =
    //    printfn "Creating model..."
    //    printfn "Starting at: %A" DateTime.Now
    //
    //    let model = ClmModel (modelGenerationParams, modelDataId, clmTaskId)
    //
    //    match p.saveModelCode with
    //    | true ->
    //        printfn "Saving model code..."
    //        model.generateCode() |> ignore
    //        printfn "... completed."
    //    | false -> printfn "NOT saving model code."
    //
    //    model.getModelData


    type ModelRunner (p : ModelRunnerData) =

        let loadParams = AllParams.getDefaultValue p.runnerProxy.loadClmDefaultValue
        let saveModelData = p.runnerProxy.updateModelData
        let saveModel getModelData = getModelData() |> saveModelData
        let updateTask = p.runnerProxy.updateClmTask
        let addClmTask = p.runnerProxy.addClmTask
        let loadClmTask = p.runnerProxy.loadClmTask
        let loadModelData = p.runnerProxy.loadModelData
        let tryGetQueueId = p.runnerProxy.saveRunQueue
        let runModel = runModel p
        let generateModel = generateModel p

        //let generateImpl (c : ClmTask) =
        //    let addError = addError GenerateImplErr
        //    let toError = toError GenerateImplErr
        //
        //    try
        //        let modelDataId = getModelDataId()
        //
        //        match loadParams c with
        //        | Ok a ->
        //            match generateModel a.modelGenerationParams modelDataId c.clmTaskInfo.clmTaskId |> saveModel with
        //            | Ok() ->
        //                let r1 = updateTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }
        //
        //                let tryCreate e =
        //                    match tryGetQueueId modelDataId c.clmTaskInfo.clmDefaultValueId e with
        //                    | Ok q ->
        //                        {
        //                            run = runModel e
        //
        //                            processToStartInfo =
        //                                {
        //                                    modelDataId = modelDataId
        //                                    defaultValueId = c.clmTaskInfo.clmDefaultValueId
        //                                    runQueueId = q
        //                                    workerNodeId = localWorkerNodeId
        //                                    commandLineParams = e
        //                                }
        //                        }
        //                        |> Ok
        //                    | Error e -> Error e
        //
        //                let (r, f) =
        //                    a.modelCommandLineParams
        //                    |> List.map tryCreate
        //                    |> Rop.unzip
        //                let e = f |> foldErrors |> toUnitResult |> combineUnitResults r1
        //                r, e
        //            | Error e -> [], addError (SaveModelErr modelDataId) e
        //        | Error e -> [], addError (LoadParametersErr modelDataId) e
        //    with
        //    | e -> [], toError (GenerateModelExn e)


        let generateAll i () =
            let a, f =
                p.runnerProxy.loadIncompleteClmTasks i
                |> Rop.mapListResult generateImpl
                |> Rop.unzipListResult

            let s, e = a |> List.unzip
            let err = foldUnitResults (e @ [foldToUnitResult f])
            s |> List.concat, err


        //let getQueue i () =
        //    printfn "ModelRunner.getQueue"
        //    match p.runnerProxy.loadRunQueue i with
        //    | Some q -> q |> List.map (fun e ->
        //                                {
        //                                    run = e.modelCommandLineParam |> runModel
        //
        //                                    processToStartInfo =
        //                                        {
        //                                            modelDataId = e.info.modelDataId
        //                                            defaultValueId = e.info.defaultValueId
        //                                            runQueueId = e.runQueueId
        //                                            workerNodeId = localWorkerNodeId
        //                                            commandLineParams = e.modelCommandLineParam
        //                                        }
        //                                })
        //    | None -> []


        let removeFromQueue runQueueId = p.runnerProxy.deleteRunQueue runQueueId


        let runRunnerModel j (m : ModelDataId) p =
            match loadModelData j m with
            | Ok parent ->
                match loadClmTask j parent.clmTaskInfo.clmTaskId |> Rop.bind loadParams, parent.data with
                | Ok a, OwnData d ->
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

                    match addClmTask t with
                    | Ok t1 ->
                        let modelDataId = getModelDataId()

                        let m1 =
                            {
                                modelDataId = modelDataId
                                clmTaskInfo = t1.clmTaskInfo
                                data = (parent.modelDataId, d) |> ParentProvided
                            }

                        match saveModelData m1 with
                        | Ok() ->
                            match tryGetQueueId modelDataId t1.clmTaskInfo.clmDefaultValueId p with
                            | Ok q ->
                                {
                                    run = runModel p

                                    processToStartInfo =
                                        {
                                            modelDataId = modelDataId
                                            defaultValueId = t1.clmTaskInfo.clmDefaultValueId
                                            runQueueId = q
                                            workerNodeId = localWorkerNodeId
                                            commandLineParams = p
                                        }
                                }
                                |> Ok
                            |Error e -> Error e
                        | Error e -> Error e
                    | Error e -> Error e
                | _ -> toError RunRunnerModelErr (InvalidDataErr m)
            | Error e -> Error e


        let createGeneratorImpl u =
            {
                //generate = generateAll p.serviceAccessInfo
                //getQueue = getQueue p.serviceAccessInfo
                removeFromQueue = removeFromQueue
                runModel = runRunnerModel p.serviceAccessInfo
                //usePartitioner = u
                //logger = Logger.log4net
            }


        member __.createGenerator = createGeneratorImpl
        member __.generate = generateImpl


    let createRunner p u =
        let r = ModelRunner p
        let a = r.createGenerator u |> AsyncRunner
        //a.queueStarting()
        a


    let createOneTimeGenerator p =
        let r = ModelRunner p
        r.generate

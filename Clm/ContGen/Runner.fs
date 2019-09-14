namespace ContGen

open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CommandLine
open Clm.CalculationData
open AsyncRun
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner

module Runner =

    type ModelRunnerParam =
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


    type ModelRunner (p : ModelRunnerParam) =
        let logError e = printfn "Error: %A" e
        let getModelDataId() = Guid.NewGuid() |> ModelDataId

//{
//    runModelParam : RunModelParam
//    callBackInfo : ProcessStartedInfoWithCallBack
//}


        let runModel e c =
            {
                exeName = p.exeName
                commandLineParam = e
                callBackInfo = c
            }
            |> p.runnerProxy.runModel


        let tryLoadParams (c : ClmTask) : AllParams option =
            fun d -> p.runnerProxy.tryLoadClmDefaultValue d
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


        let saveModelData modelData = p.runnerProxy.tryUpdateModelData modelData
        let saveModel getModelData = getModelData() |> saveModelData
        let updateTask (c : ClmTask) = p.runnerProxy.tryUpdateClmTask c |> ignore
        let tryAddClmTask c = p.runnerProxy.addClmTask c
        let tryLoadClmTask i t = p.runnerProxy.tryLoadClmTask i t
        let tryLoadModelData i m = p.runnerProxy.tryLoadModelData i m


        let getQueueId (c : ModelCommandLineParam) modelId d =
             match p.runnerProxy.saveRunQueueEntry modelId d c with
             | Some q -> q
             | None -> failwith "getQueueId - cannot get run queue id..." // TODO kk:20190531 - This is not so good! refactor.


        let generateImpl (c : ClmTask) =
            try
                let modelDataId = getModelDataId()

                match tryLoadParams c with
                | Some a ->
                    match generateModel a.modelGenerationParams modelDataId c.clmTaskInfo.clmTaskId |> saveModel with
                    | Some true ->
                        updateTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }

                        a.modelCommandLineParams
                        |> List.map (fun e ->
                            {
                                run = runModel e

                                processToStartInfo =
                                    {
                                        modelDataId = modelDataId
                                        defaultValueId = c.clmTaskInfo.clmDefaultValueId
                                        runQueueId = getQueueId e modelDataId c.clmTaskInfo.clmDefaultValueId
                                    }
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
            match p.runnerProxy.loadIncompleteClmTasks i with
            | Some c -> c |> List.map generateImpl |> List.concat
            | None -> []


        let getQueue i () =
            match p.runnerProxy.loadRunQueue i with
            | Some q -> q |> List.map (fun e ->
                                        {
                                            run = e.modelCommandLineParam |> runModel

                                            processToStartInfo =
                                                {
                                                    modelDataId = e.info.modelDataId
                                                    defaultValueId = e.info.defaultValueId
                                                    runQueueId = e.runQueueId
                                                }
                                        })
            | None -> []


        let removeFromQueue runQueueId =
            match p.runnerProxy.deleteRunQueueEntry runQueueId with
            | Some _ -> ignore()
            | None ->
                logError (sprintf "Cannot delete runQueueId = %A" runQueueId)
                ignore()



//type ProcessToStartInfo =
//    {
//        modelDataId : ModelDataId
//        runQueueId : RunQueueId
//    }
//
//type ProcessStartedInfo =
//    {
//        processId : ProcessId
//        processToStartInfo : ProcessToStartInfo
//    }
//
//type ProcessStartedResult =
//    | StartedSuccessfully of ProcessStartedInfo
//    | AlreadyCompleted
//    | FailedToStart


        let runRunnerModel j (m : ModelDataId) p = //: ProcessStartedResult =
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

                                    processToStartInfo =
                                        {
                                            modelDataId = modelDataId
                                            defaultValueId = t1.clmTaskInfo.clmDefaultValueId
                                            runQueueId = getQueueId p modelDataId t1.clmTaskInfo.clmDefaultValueId
                                        }
                                }
                            Some r
                        | _ -> None
                    | None -> None
                | _ -> None
            | None -> None


        let createGeneratorImpl u =
            {
                generate = generateAll p.serviceAccessInfo
                getQueue = getQueue p.serviceAccessInfo
                removeFromQueue = removeFromQueue
                //maxQueueLength = 4
                runModel = runRunnerModel p.serviceAccessInfo
                usePartitioner = u
            }


        member __.createGenerator = createGeneratorImpl
        member __.generate = generateImpl


    let createRunner p u =
        let r = ModelRunner p
        let a = r.createGenerator u |> AsyncRunner
        a.queueStarting()
        a


    let createOneTimeGenerator p =
        let r = ModelRunner p
        r.generate

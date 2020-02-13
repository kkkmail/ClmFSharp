namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.ModelGeneratorErrors
open ServiceProxy.ModelGeneratorProxy
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CalculationData
open ClmSys.WorkerNodeData
open ClmSys.ContGenData
open ClmSys.SolverRunnerData
open ClmSys.Rop
open DbData.DatabaseTypes
open ClmSys.Logging
open ClmSys.TimerEvents

module ModelGenerator =

    let private toError g f = f |> g |> ModelGeneratorErr |> Error
    let private addError g f e = ((f |> g |> ModelGeneratorErr) + e) |> Error


    let generateModel (proxy : GenerateModelProxy) (c : ClmTask) =
        let addError = addError GenerateModelErr
        let toError = toError GenerateModelErr
        let modelDataId = ModelDataId.getNewId()

        if c.remainingRepetitions > 0
        then
            match proxy.loadParams c with
            | Ok a ->
                let model = ClmModel (a.modelGenerationParams, modelDataId, c.clmTaskInfo.clmTaskId)

                match model.getModelData() |> proxy.upsertModelData with
                | Ok() ->
                    let r =
                        a.modelCommandLineParams
                        |> List.map(fun e ->
                                        {
                                            runQueueId = RunQueueId.getNewId()
                                            info =
                                                {
                                                    modelDataId = modelDataId;
                                                    defaultValueId = c.clmTaskInfo.clmDefaultValueId;
                                                    modelCommandLineParam = e
                                                }
                                            runQueueStatus = NotStartedRunQueue
                                            workerNodeIdOpt = None
                                            progress = NotStarted
                                        })
                        |> List.map (fun e -> proxy.upsertRunQueue e)
                        |> foldUnitResults

                    let r1 = proxy.updateClmTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }
                    combineUnitResults r r1
                | Error e -> addError (UnableUpsertModelDataErr c.clmTaskInfo.clmTaskId) e
            | Error e -> addError (UnableLoadParamsErr c.clmTaskInfo.clmTaskId) e
        else toError (TaskCompletedErr c.clmTaskInfo.clmTaskId)


    let generateAll (proxy : GenerateAllProxy) =
        let (r, f) = proxy.loadIncompleteClmTasks() |> unzipListResult
        let e = r |> List.map proxy.generateModel |> foldUnitResults
        f |> foldToUnitResult |> combineUnitResults e


    type GenerateAllProxy
        with

        static member create c =
            {
                loadIncompleteClmTasks = fun () -> loadIncompleteClmTasks c
                generateModel = generateModel (GenerateModelProxy.create c)
            }


    let createModelGenerator (logger : Logger) c =
        logger.logInfoString "createModelGenerator: Creating model generator..."
        let proxy = GenerateAllProxy.create c
        let e = fun () -> generateAll proxy
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger.logError e)
        h

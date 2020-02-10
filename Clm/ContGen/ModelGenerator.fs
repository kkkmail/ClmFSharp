namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.AsyncRunErrors
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

module ModelGenerator =

    let private toError g f = f |> g |> ModelGeneratorErr |> Error
    let private addError g f e = ((f |> g |> ModelGeneratorErr) + e) |> Error


    let generateModel (proxy : GenerateModelProxy) (c : ClmTask) =
        let addError = addError GenerateModelErr
        let modelDataId = ModelDataId.getNewId()

        match proxy.loadParams c with
        | Ok a ->
            let model = ClmModel (a.modelGenerationParams, modelDataId, c.clmTaskInfo.clmTaskId)

            match model.getModelData() |> proxy.upsertModelData with
            | Ok() ->
                let result =
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

                result
            | Error e -> addError (UnableUpsertModelData c.clmTaskInfo.clmTaskId) e
        | Error e -> addError (UnableLoadParamsErr c.clmTaskInfo.clmTaskId) e


    let generateAll (proxy : GenerateAllProxy) i =
        let (r, f) = proxy.loadIncompleteClmTasks i |> unzipListResult
        let e = r |> List.map proxy.generateModel |> foldUnitResults
        f |> foldToUnitResult |> combineUnitResults e

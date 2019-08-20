namespace NoSql

open Newtonsoft.Json
open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open System.IO
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo

module FileSystemTypes =

    let fileStorageFolder = DefaultFileStorageFolder

    let messageTblName = "Message"
    let modelDataTblName = "ModelData"
    let resultDataTblName = "ResultData"
    let chartTblName = "Chart"
    let workerNodeRunModelDataTblName = "WorkerNodeRunModelData"

    let storageExt = "json"


    let getFolderName tableName = fileStorageFolder + "\\" + tableName


    let getFileName<'A> tableName (objectId : 'A) =
        let folder = getFolderName tableName
        Directory.CreateDirectory(folder) |> ignore
        Path.Combine(folder, objectId.ToString() + "." + storageExt)


    let tryLoadData<'T, 'A> tableName (objectId : 'A) =
        let f = getFileName tableName objectId

        if File.Exists f
        then File.ReadAllText(f) |> JsonConvert.DeserializeObject<'T> |> Some
        else None


    let saveData<'T, 'A> tableName (objectId : 'A) (t : 'T) =
        let f = getFileName tableName objectId
        let d = t |> JsonConvert.SerializeObject
        File.WriteAllText(f, d)
        true


    let tryDeleteData<'T, 'A> tableName (objectId : 'A) =
        let f = getFileName tableName objectId

        if File.Exists f then File.Delete f


    let getObjectIds<'A> tableName (creator : string -> 'A) =
        let folder = getFolderName tableName
        Directory.GetFiles(folder, "*." + storageExt)
        |> Array.map (fun e -> Path.GetFileNameWithoutExtension e)
        |> Array.map creator
        |> List.ofArray


    let saveMessageFs (m : Message) = saveData<Message, Guid> messageTblName m.messageId.value m
    let tryLoadMessageFs (MessageId messageId) = tryLoadData<ModelData, Guid> messageTblName messageId
    let tryDeleteMessageFs (MessageId messageId) = tryDeleteData<ModelData, Guid> messageTblName messageId
    let getMessageIdsFs () = getObjectIds<Guid> messageTblName Guid.Parse |> List.map MessageId

    let saveModelDataFs (m : ModelData) = saveData<ModelData, Guid> modelDataTblName m.modelDataId.value m
    let tryLoadModelDataFs (ModelDataId modelDataId) = tryLoadData<ModelData, Guid> modelDataTblName modelDataId
    let tryDeleteModelDataFs (ModelDataId modelDataId) = tryDeleteData<ModelData, Guid> modelDataTblName modelDataId
    let getModelDataIdsFs () = getObjectIds<Guid> modelDataTblName Guid.Parse |> List.map ModelDataId

    let saveWorkerNodeRunModelDataFs (m : WorkerNodeRunModelData) = saveData<WorkerNodeRunModelData, Guid> workerNodeRunModelDataTblName m.remoteProcessId.value m
    let tryLoadWorkerNodeRunModelDataFs (RemoteProcessId processId) = tryLoadData<WorkerNodeRunModelData, Guid> workerNodeRunModelDataTblName processId
    let tryDeleteWorkerNodeRunModelDataFs (RemoteProcessId processId) = tryDeleteData<WorkerNodeRunModelData, Guid> workerNodeRunModelDataTblName processId
    let getWorkerNodeRunModelDataIdsFs () = getObjectIds<Guid> workerNodeRunModelDataTblName Guid.Parse |> List.map RemoteProcessId

    let saveResultDataFs (r : ResultDataWithId) = saveData<ResultDataWithId, Guid> resultDataTblName r.resultDataId.value r
    let tryLoadResultDataFs (ResultDataId resultDataId) = tryLoadData<ResultDataWithId, Guid> resultDataTblName resultDataId
    let tryDeleteResultDataFs (ResultDataId resultDataId) = tryDeleteData<WorkerNodeRunModelData, Guid> resultDataTblName resultDataId
    let getResultDataIdsFs () = getObjectIds<Guid> resultDataTblName Guid.Parse |> List.map ResultDataId

    let saveChartsFs (ResultDataId resultDataId) (p : list<string>) = saveData<ChartInfo, Guid> chartTblName resultDataId { chartFileNames = p |> Array.ofList }

    let tryLoadCharts (ResultDataId resultDataId) =
        tryLoadData<ChartInfo, Guid> chartTblName resultDataId
        |> Option.bind (fun e -> e.chartFileNames |> List.ofArray |> Some)

    let tryDeleteChartsFs (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> chartTblName resultDataId
    let getChartsIdsFs () = getObjectIds<Guid> chartTblName Guid.Parse |> List.map ResultDataId


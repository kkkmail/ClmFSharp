namespace NoSql

open Newtonsoft.Json
open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open System.IO
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open ClmSys.MessagingData

module FileSystemTypes =

    let fileStorageFolder = DefaultFileStorageFolder

    type TableName =
        | TableName of string


    let messageTblName = TableName "Message"
    let messageWithTypeTblName = TableName "MessageWithType"
    let modelDataTblName = TableName "ModelData"
    let resultDataTblName = TableName "ResultData"
    let chartTblName = TableName "Chart"
    let workerNodeRunModelDataTblName = TableName "WorkerNodeRunModelData"

    let storageExt = "json"


    let getFolderName (MessagingClientName serviceName) (TableName tableName) = fileStorageFolder + "\\" + serviceName + "\\" + tableName


    let getFileName<'A> serviceName tableName (objectId : 'A) =
        let folder = getFolderName serviceName tableName
        Directory.CreateDirectory(folder) |> ignore
        Path.Combine(folder, objectId.ToString() + "." + storageExt)


    let tryLoadData<'T, 'A> serviceName tableName (objectId : 'A) =
        let f = getFileName serviceName tableName objectId

        if File.Exists f
        then File.ReadAllText(f) |> JsonConvert.DeserializeObject<'T> |> Some
        else None


    let saveData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        let f = getFileName serviceName tableName objectId
        let d = t |> JsonConvert.SerializeObject
        File.WriteAllText(f, d)
        true


    let tryDeleteData<'T, 'A> serviceName tableName (objectId : 'A) =
        let f = getFileName serviceName tableName objectId

        if File.Exists f then File.Delete f


    let getObjectIds<'A> serviceName tableName (creator : string -> 'A) =
        let folder = getFolderName serviceName tableName
        Directory.GetFiles(folder, "*." + storageExt)
        |> Array.map (fun e -> Path.GetFileNameWithoutExtension e)
        |> Array.map creator
        |> List.ofArray


    let saveMessageFs serviceName (m : Message) = saveData<Message, Guid> serviceName messageTblName m.messageId.value m
    let tryLoadMessageFs serviceName (MessageId messageId) = tryLoadData<Message, Guid> serviceName messageTblName messageId
    let tryDeleteMessageFs serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    let getMessageIdsFs serviceName () = getObjectIds<Guid> serviceName messageTblName Guid.Parse |> List.map MessageId

    let saveMessageWithTypeFs serviceName (m : MessageWithType) = saveData<MessageWithType, Guid> serviceName messageWithTypeTblName m.message.messageId.value m
    let tryLoadMessageWithTypeFs serviceName (MessageId messageId) = tryLoadData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let tryDeleteMessageWithTypeFs serviceName (MessageId messageId) = tryDeleteData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let getMessageWithTypeIdsFs serviceName () = getObjectIds<Guid> serviceName messageWithTypeTblName Guid.Parse |> List.map MessageId

    let saveModelDataFs serviceName (m : ModelData) = saveData<ModelData, Guid> serviceName modelDataTblName m.modelDataId.value m
    let tryLoadModelDataFs serviceName (ModelDataId modelDataId) = tryLoadData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let tryDeleteModelDataFs serviceName (ModelDataId modelDataId) = tryDeleteData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let getModelDataIdsFs serviceName () = getObjectIds<Guid> serviceName modelDataTblName Guid.Parse |> List.map ModelDataId

    let saveWorkerNodeRunModelDataFs serviceName (m : WorkerNodeRunModelData) = saveData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName m.remoteProcessId.value m
    let tryLoadWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = tryLoadData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let tryDeleteWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = tryDeleteData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let getWorkerNodeRunModelDataIdsFs serviceName () = getObjectIds<Guid> serviceName workerNodeRunModelDataTblName Guid.Parse |> List.map RemoteProcessId

    let saveResultDataFs serviceName (r : ResultDataWithId) = saveData<ResultDataWithId, Guid> serviceName resultDataTblName r.resultDataId.value r
    let tryLoadResultDataFs serviceName (ResultDataId resultDataId) = tryLoadData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let tryDeleteResultDataFs serviceName (ResultDataId resultDataId) = tryDeleteData<WorkerNodeRunModelData, Guid> serviceName resultDataTblName resultDataId
    let getResultDataIdsFs serviceName () = getObjectIds<Guid> serviceName resultDataTblName Guid.Parse |> List.map ResultDataId

    let saveChartsFs serviceName (ResultDataId resultDataId) (p : list<string>) = saveData<ChartInfo, Guid> serviceName chartTblName resultDataId { chartFileNames = p |> Array.ofList }

    let tryLoadCharts serviceName (ResultDataId resultDataId) =
        tryLoadData<ChartInfo, Guid> serviceName chartTblName resultDataId
        |> Option.bind (fun e -> e.chartFileNames |> List.ofArray |> Some)

    let tryDeleteChartsFs serviceName (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> serviceName chartTblName resultDataId
    let getChartsIdsFs serviceName () = getObjectIds<Guid> serviceName chartTblName Guid.Parse |> List.map ResultDataId


﻿namespace NoSql

open Newtonsoft.Json
open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open System.IO
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ClmSys.WorkerNodeData

module FileSystemTypes =

    let fileStorageFolder = DefaultFileStorageFolder

    type TableName =
        | TableName of string


    let messageTblName = TableName "Message"
    let messageWithTypeTblName = TableName "MessageWithType"
    let modelDataTblName = TableName "ModelData"
    let resultDataTblName = TableName "ResultData"
    let chartInfoTblName = TableName "ChartInfo"
    let workerNodeRunModelDataTblName = TableName "WorkerNodeRunModelData"
    let workerNodeInfoTblName = TableName "WorkerNodeInfo"

    let storageExt = "json"


    let getFolderName (MessagingClientName serviceName) (TableName tableName) =
        let folder = fileStorageFolder + "\\" + serviceName + "\\" + tableName
        printfn "getFolderName: Attempting to create folder: %A" folder
        Directory.CreateDirectory(folder) |> ignore
        printfn "    ... created."
        folder


    let getFileName<'A> serviceName tableName (objectId : 'A) =
        let folder = getFolderName serviceName tableName
        let file = Path.Combine(folder, objectId.ToString() + "." + storageExt)
        printfn "getFileName: Using file: %A" file
        file


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


    // TODO kk:20190822 - Possibley refactor to use service name only once.
    //type TableActions<'T, 'A> =
    //    {
    //        saveMessage : MessagingClientName -> 'T -> bool
    //        tryLoadMessage : MessagingClientName -> 'A -> 'T option
    //        tryDeleteMessage : MessagingClientName -> 'A -> unit
    //        getMessageIds : MessagingClientName -> unit -> list<'A>
    //    }
    //
    //
    //let createTableActions<'T, 'A> serviceName (getId : 'T -> 'A) =
    //    let saveObject serviceName m = saveData<'T, 'A> serviceName messageTblName (getId m) m
    //    let tryLoadObject serviceName (MessageId messageId) = tryLoadData<Message, Guid> serviceName messageTblName messageId
    //    let tryDeleteObject serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    //    let getObjectIds serviceName () = getObjectIds<Guid> serviceName messageTblName Guid.Parse |> List.map MessageId
    //
    //    0


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
    let tryDeleteResultDataFs serviceName (ResultDataId resultDataId) = tryDeleteData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let getResultDataIdsFs serviceName () = getObjectIds<Guid> serviceName resultDataTblName Guid.Parse |> List.map ResultDataId

    let saveChartInfoFs serviceName (c : ChartInfo) = saveData<ChartInfo, Guid> serviceName chartInfoTblName c.resultDataId.value c
    let tryLoadChartInfoFs serviceName (ResultDataId resultDataId) = tryLoadData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let tryDeleteChartInfoFs serviceName (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let getChartInfoIdsFs serviceName () = getObjectIds<Guid> serviceName chartInfoTblName Guid.Parse |> List.map ResultDataId

    let saveWorkerNodeInfoFs serviceName (r : WorkerNodeInfo) = saveData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName r.workerNodeId.value.value r
    let tryLoadWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryLoadData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let tryDeleteWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryDeleteData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let getWorkerNodeInfoIdsFs serviceName () = getObjectIds<Guid> serviceName workerNodeInfoTblName Guid.Parse |> List.map (fun e -> e |> MessagingClientId |> WorkerNodeId)

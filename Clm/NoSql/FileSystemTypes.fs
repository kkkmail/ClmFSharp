namespace NoSql

open Newtonsoft.Json
open System
open ClmSys.Logging
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open Clm.ModelParams
open Clm.CalculationData
open System.IO
open MessagingServiceInfo.ServiceInfo
open PartitionerServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ClmSys

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
    let runModelParamWithRemoteIdTblName = TableName "RunModelParamWithRemoteId"
    let workerNodeStateTblName = TableName "WorkerNodeState"
    let partitionerQueueElementTblName = TableName "PartitionerQueueElement"

    let storageExt = "xml"


    let tryGetFolderName (MessagingClientName serviceName) (TableName tableName) =
        let folder = fileStorageFolder + "\\" + serviceName + "\\" + tableName
        //logger.logInfo (sprintf "getFolderName: Attempting to create folder: %A" folder)

        try
            Directory.CreateDirectory(folder) |> ignore
            Ok folder
        with
        | e -> e |> GetFolderNameException |> Error


    let tryGetFileName<'A> serviceName tableName (objectId : 'A) =
        try
            match tryGetFolderName serviceName tableName with
            | Ok folder ->
                let file = Path.Combine(folder, objectId.ToString() + "." + storageExt)
                Ok file
            | Error e -> Error e
        with
        | e -> e |> GetFileNameException |> Error


    let tryLoadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T, FileError>) =
        try
            //printfn "tryLoadData: Loading data for objectId: %A ..." objectId
            match tryGetFileName serviceName tableName objectId with
            | Ok f ->
                let x =
                    if File.Exists f
                    then
                        //printfn "tryLoadData: Reading the data from file %A for objectId: %A ..." f objectId
                        let data = File.ReadAllText(f)
                        //printfn "tryLoadData: Finished reading the data from file %A for objectId: %A. Deserializing into type %A ..." f objectId typeof<'T>
                        let retVal = data |> deserialize |> Ok
                        //printfn "tryLoadData: Finished deserializing for objectId: %A." objectId
                        retVal
                    else Error (FileNotFound f)

                //printfn "tryLoadData: Finished loading data for objectId: %A." objectId
                x
            | Error e -> Error e
        with
        | e -> e |> ReadFileException |> Error


    let trySaveData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        try
            match tryGetFileName serviceName tableName objectId with
            | Ok f ->
                let d = t |> serialize
                File.WriteAllText(f, d)
                Ok ()
            | Error e -> Error e
        with
        | e -> e |> WriteFileException |> Error


    let tryDeleteData<'T, 'A> serviceName tableName (objectId : 'A) =
        try
            match tryGetFileName serviceName tableName objectId with
            | Ok f ->
                if File.Exists f then File.Delete f
                Ok ()
            | Error e -> Error e
        with
        | e -> e |> DeleteFileException |> Error


    let tryGetObjectIds<'A> serviceName tableName (creator : string -> 'A) =
        try
            match tryGetFolderName serviceName tableName with
            | Ok folder ->
                Directory.GetFiles(folder, "*." + storageExt)
                |> Array.map (fun e -> Path.GetFileNameWithoutExtension e)
                |> Array.map creator
                |> List.ofArray
                |> Ok
            | Error e -> Error e
        with
        | e -> e |> GetObjectIdsException |> Error


    type TableActions<'T, 'A> =
        {
            save : 'T -> bool
            tryLoad : 'A -> 'T option
            tryDelete : 'A -> unit
            getIds : unit -> list<'A>
        }


    let createTableActions<'T, 'A> tableName (getId : 'T -> 'A) (getObject : Guid -> 'A) serviceName =
        {
            save = fun m -> saveData<'T, 'A> serviceName tableName (getId m) m
            tryLoad = fun m -> tryLoadData<'T, 'A> serviceName tableName m
            tryDelete = fun m -> tryDeleteData<'T, 'A> serviceName tableName m
            getIds = fun () -> (getObjectIds<Guid> serviceName tableName Guid.Parse |> List.map getObject)
        }


    let saveMessageFs serviceName (m : Message) = saveData<Message, Guid> serviceName messageTblName m.messageDataInfo.messageId.value m
    let tryLoadMessageFs serviceName (MessageId messageId) = tryLoadData<Message, Guid> serviceName messageTblName messageId
    let tryDeleteMessageFs serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    let getMessageIdsFs serviceName () = getObjectIds<Guid> serviceName messageTblName Guid.Parse |> List.map MessageId

    let saveMessageWithTypeFs serviceName (m : MessageWithType) = saveData<MessageWithType, Guid> serviceName messageWithTypeTblName m.message.messageDataInfo.messageId.value m
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

    let saveRunModelParamWithRemoteIdFs serviceName (r : RunModelParamWithRemoteId) = saveData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName r.remoteProcessId.value r
    let tryLoadRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = tryLoadData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let tryDeleteRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = tryDeleteData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let getRunModelParamWithRemoteIdsFs serviceName () = getObjectIds<Guid> serviceName runModelParamWithRemoteIdTblName Guid.Parse |> List.map (fun e -> e |> RemoteProcessId)

    let saveWorkerNodeStateFs serviceName (r : WorkerNodeState) = saveData<WorkerNodeState, Guid> serviceName workerNodeStateTblName r.workerNodeInfo.workerNodeId.value.value r
    let tryLoadWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = tryLoadData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let tryDeleteWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = tryDeleteData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let getWorkerNodeStateIdsFs serviceName () = getObjectIds<Guid> serviceName workerNodeStateTblName Guid.Parse |> List.map (fun e -> e |> MessagingClientId |> WorkerNodeId)


    // TODO kk:20190824 - That does not seem to go in the proper direction. Delete after 90 days or make it work.
    //let loadAllWorkerNodeInfoImpl (tryFun : (unit -> 'C) -> 'C option) (getIds : ServiceName -> unit -> list<'A>) (tryLoad : ServiceName -> 'A -> 'B option) name =
    //    match tryFun (getIds name) with
    //    | Some i ->
    //        i
    //        |> List.map (fun e -> tryFun (fun _ -> tryLoad name e) |> Option.bind id)
    //        |> List.choose id
    //    | None -> []



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
open ClmSys.Retry
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.SolverRunnerData
open ClmSys.ContGenData
open ClmSys.WorkerNodePrimitives

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


    let getFolderName (MessagingClientName serviceName) (TableName tableName) =
        let folder = fileStorageFolder + "\\" + serviceName + "\\" + tableName
        //logger.logInfo (sprintf "getFolderName: Attempting to create folder: %A" folder)

        try
            Directory.CreateDirectory(folder) |> ignore
            Ok folder
        with
        | e -> e |> GetFolderNameExn |> FileErr |> Error


    let getFileName<'A> serviceName tableName (objectId : 'A) =
        try
            match getFolderName serviceName tableName with
            | Ok folder ->
                let file = Path.Combine(folder, objectId.ToString() + "." + storageExt)
                Ok file
            | Error e -> Error e
        with
        | e -> e |> GetFileNameExn |> FileErr |> Error


    /// Tries to load data.
    /// Returns (Ok (Some Object)) if object was found and successfully loaded.
    /// Returns (Ok None) if the object is not found.
    /// Returns (Error e) in case of any other issues.
    let tryLoadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T option, ClmError>) =
        let w () =
            try
                match getFileName serviceName tableName objectId with
                | Ok f ->
                    let x =
                        if File.Exists f
                        then
                            let data = File.ReadAllText(f)
                            let retVal = data |> deserialize |> Some |> Ok
                            retVal
                        else Ok None
                    x
                | Error e -> Error e
            with
            | e -> e |> ReadFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    /// Loads the data if successfull and returns an error if an object is not found OR any error occurs.
    let loadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T, ClmError>) =
        match tryLoadData<'T, 'A> serviceName tableName objectId with
        | Ok (Some r) -> Ok r
        | Ok None ->
            match getFileName<'A> serviceName tableName objectId with
            | Ok f -> f |> FileNotFoundErr |> FileErr |> Error
            | Error e -> Error e
        | Error e -> Error e


    let saveData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        let w() =
            try
                match getFileName serviceName tableName objectId with
                | Ok f ->
                    let d = t |> serialize
                    File.WriteAllText(f, d)
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> WriteFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    /// Tries to delete object if it exists.
    let tryDeleteData<'T, 'A> serviceName tableName (objectId : 'A) =
        let w() =
            try
                match getFileName serviceName tableName objectId with
                | Ok f ->
                    if File.Exists f then File.Delete f
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> DeleteFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let getObjectIds<'A> serviceName tableName (creator : string -> 'A) =
        let w() =
            try
                match getFolderName serviceName tableName with
                | Ok folder ->
                    Directory.GetFiles(folder, "*." + storageExt)
                    |> List.ofArray
                    |> List.map (fun e -> Path.GetFileNameWithoutExtension e)
                    |> List.map creator
                    |> Ok
                | Error e -> Error e
            with
            | e -> e |> GetObjectIdsExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let loadObjects<'T, 'A> serviceName tableName (creator : string -> 'A) =
        match getObjectIds serviceName tableName creator with
        | Ok i ->
            i
            |> List.map (loadData<'T, 'A> serviceName tableName)
            |> Ok
        | Error e -> Error e


    //type TableActions<'T, 'A> =
    //    {
    //        save : 'T -> bool
    //        tryLoad : 'A -> 'T option
    //        tryDelete : 'A -> unit
    //        getIds : unit -> list<'A>
    //    }
    //
    //
    //let createTableActions<'T, 'A> tableName (getId : 'T -> 'A) (getObject : Guid -> 'A) serviceName =
    //    {
    //        save = fun m -> saveData<'T, 'A> serviceName tableName (getId m) m
    //        tryLoad = fun m -> tryLoadData<'T, 'A> serviceName tableName m
    //        tryDelete = fun m -> tryDeleteData<'T, 'A> serviceName tableName m
    //        getIds = fun () -> (getObjectIds<Guid> serviceName tableName Guid.Parse |> List.map getObject)
    //    }


    let saveMessageFs serviceName (m : Message) = saveData<Message, Guid> serviceName messageTblName m.messageDataInfo.messageId.value m
    let loadMessageFs serviceName (MessageId messageId) = loadData<Message, Guid> serviceName messageTblName messageId
    let tryDeleteMessageFs serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    let getMessageIdsFs serviceName () = getObjectIds<MessageId> serviceName messageTblName (fun e -> e |> Guid.Parse |> MessageId)
    let loadMessageAllFs serviceName () = loadObjects<Message, Guid> serviceName messageTblName Guid.Parse

    let saveMessageWithTypeFs serviceName (m : MessageWithType) = saveData<MessageWithType, Guid> serviceName messageWithTypeTblName m.message.messageDataInfo.messageId.value m
    let loadMessageWithTypeFs serviceName (MessageId messageId) = loadData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let tryDeleteMessageWithTypeFs serviceName (MessageId messageId) = tryDeleteData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let getMessageWithTypeIdsFs serviceName () = getObjectIds<MessageId> serviceName messageWithTypeTblName (fun e -> e |> Guid.Parse |> MessageId)
    let loadMessageWithTypeAllFs serviceName () = loadObjects<MessageWithType, Guid> serviceName messageWithTypeTblName Guid.Parse

    let saveModelDataFs serviceName (m : ModelData) = saveData<ModelData, Guid> serviceName modelDataTblName m.modelDataId.value m
    let loadModelDataFs serviceName (ModelDataId modelDataId) = loadData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let tryDeleteModelDataFs serviceName (ModelDataId modelDataId) = tryDeleteData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let getModelDataIdsFs serviceName () = getObjectIds<ModelDataId> serviceName modelDataTblName (fun e -> e |> Guid.Parse |> ModelDataId)
    let loadModelDataAllsFs serviceName () = loadObjects<ModelData, Guid> serviceName modelDataTblName Guid.Parse

    let saveWorkerNodeRunModelDataFs serviceName (m : WorkerNodeRunModelData) = saveData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName m.remoteProcessId.value m
    let loadWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = loadData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let tryDeleteWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = tryDeleteData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let getWorkerNodeRunModelDataIdsFs serviceName () = getObjectIds<RemoteProcessId> serviceName workerNodeRunModelDataTblName (fun e -> e |> Guid.Parse |> RemoteProcessId)
    let loadWorkerNodeRunModelDataAllFs serviceName () = loadObjects<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName Guid.Parse

    let saveResultDataFs serviceName (r : ResultDataWithId) = saveData<ResultDataWithId, Guid> serviceName resultDataTblName r.resultDataId.value r
    let loadResultDataFs serviceName (ResultDataId resultDataId) = loadData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let tryDeleteResultDataFs serviceName (ResultDataId resultDataId) = tryDeleteData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let getResultDataIdsFs serviceName () = getObjectIds<ResultDataId> serviceName resultDataTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let loadResultDataAllFs serviceName () = loadObjects<ResultDataWithId, Guid> serviceName resultDataTblName Guid.Parse

    let saveChartInfoFs serviceName (c : ChartInfo) = saveData<ChartInfo, Guid> serviceName chartInfoTblName c.resultDataId.value c
    let loadChartInfoFs serviceName (ResultDataId resultDataId) = loadData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let tryDeleteChartInfoFs serviceName (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let getChartInfoIdsFs serviceName () = getObjectIds<ResultDataId> serviceName chartInfoTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let loadChartInfoAllFs serviceName () = loadObjects<ChartInfo, Guid> serviceName chartInfoTblName Guid.Parse


    let saveLocalChartInfo d (c : ChartInfo) =
        let w() =
            try
                let getFileName name =
                    match d with
                    | Some (f, g) -> Path.Combine(f, g.ToString(), Path.GetFileName name)
                    | None -> name

                let saveChart f c =
                    let folder = Path.GetDirectoryName f
                    Directory.CreateDirectory(folder) |> ignore
                    File.WriteAllText(f, c)

                c.charts
                |> List.map (fun e -> saveChart (getFileName e.chartName) e.chartContent)
                |> ignore
                Ok ()
            with
            | e -> e |> SaveChartsExn |> FileErr |> Error

        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let saveWorkerNodeInfoFs serviceName (r : WorkerNodeInfo) = saveData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName r.workerNodeId.value.value r
    let loadWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = loadData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let tryDeleteWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryDeleteData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let getWorkerNodeInfoIdsFs serviceName () = getObjectIds<WorkerNodeId> serviceName workerNodeInfoTblName (fun e -> e |> Guid.Parse |> MessagingClientId |> WorkerNodeId)
    let loadeWorkerNodeInfoAllFs serviceName () = loadObjects<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName Guid.Parse

    let saveRunModelParamWithRemoteIdFs serviceName (r : RunModelParamWithRemoteId) = saveData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName r.remoteProcessId.value r
    let loadRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = loadData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let tryDeleteRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = tryDeleteData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let getRunModelParamWithRemoteIdIdsFs serviceName () = getObjectIds<RemoteProcessId> serviceName runModelParamWithRemoteIdTblName (fun e -> e |> Guid.Parse |> RemoteProcessId)
    let loadeRunModelParamWithRemoteIdAllFs serviceName () = loadObjects<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName Guid.Parse

    let saveWorkerNodeStateFs serviceName (r : WorkerNodeState) = saveData<WorkerNodeState, Guid> serviceName workerNodeStateTblName r.workerNodeInfo.workerNodeId.value.value r
    let loadWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = loadData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let tryDeleteWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = tryDeleteData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let getWorkerNodeStateIdsFs serviceName () = getObjectIds<WorkerNodeId> serviceName workerNodeStateTblName (fun e -> e |> Guid.Parse |> MessagingClientId |> WorkerNodeId)
    let loadWorkerNodeStateAllFs serviceName () = loadObjects<WorkerNodeState, Guid> serviceName workerNodeStateTblName Guid.Parse

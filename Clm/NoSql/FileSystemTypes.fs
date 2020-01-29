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


    let tryGetFolderName (MessagingClientName serviceName) (TableName tableName) =
        let folder = fileStorageFolder + "\\" + serviceName + "\\" + tableName
        //logger.logInfo (sprintf "getFolderName: Attempting to create folder: %A" folder)

        try
            Directory.CreateDirectory(folder) |> ignore
            Ok folder
        with
        | e -> e |> GetFolderNameException |> FileErr |> Error


    let tryGetFileName<'A> serviceName tableName (objectId : 'A) =
        try
            match tryGetFolderName serviceName tableName with
            | Ok folder ->
                let file = Path.Combine(folder, objectId.ToString() + "." + storageExt)
                Ok file
            | Error e -> Error e
        with
        | e -> e |> GetFileNameException |> FileErr |> Error


    let tryLoadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T, ClmError>) =
        let w () =
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
                        else f |> FileNotFound |> FileErr |> Error

                    //printfn "tryLoadData: Finished loading data for objectId: %A." objectId
                    x
                | Error e -> Error e
            with
            | e -> e |> ReadFileException |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileException |> FileErr) w


    let trySaveData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        let w() =
            try
                match tryGetFileName serviceName tableName objectId with
                | Ok f ->
                    let d = t |> serialize
                    File.WriteAllText(f, d)
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> WriteFileException |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileException |> FileErr) w


    let tryDeleteData<'T, 'A> serviceName tableName (objectId : 'A) =
        let w() =
            try
                match tryGetFileName serviceName tableName objectId with
                | Ok f ->
                    if File.Exists f then File.Delete f
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> DeleteFileException |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileException |> FileErr) w


    let tryGetObjectIds<'A> serviceName tableName (creator : string -> 'A) =
        let w() =
            try
                match tryGetFolderName serviceName tableName with
                | Ok folder ->
                    Directory.GetFiles(folder, "*." + storageExt)
                    |> List.ofArray
                    |> List.map (fun e -> Path.GetFileNameWithoutExtension e)
                    |> List.map creator
                    |> Ok
                | Error e -> Error e
            with
            | e -> e |> GetObjectIdsException |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileException |> FileErr) w


    let tryLoadObjects<'T, 'A> serviceName tableName (creator : string -> 'A) =
        match tryGetObjectIds serviceName tableName creator with
        | Ok i ->
            i
            |> List.map (tryLoadData<'T, 'A> serviceName tableName)
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


    let trySaveMessageFs serviceName (m : Message) = trySaveData<Message, Guid> serviceName messageTblName m.messageDataInfo.messageId.value m
    let tryLoadMessageFs serviceName (MessageId messageId) = tryLoadData<Message, Guid> serviceName messageTblName messageId
    let tryDeleteMessageFs serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    let tryGetMessageIdsFs serviceName () = tryGetObjectIds<MessageId> serviceName messageTblName (fun e -> e |> Guid.Parse |> MessageId)
    let tryLoadMessageAllFs serviceName () = tryLoadObjects<Message, Guid> serviceName messageTblName Guid.Parse

    let trySaveMessageWithTypeFs serviceName (m : MessageWithType) = trySaveData<MessageWithType, Guid> serviceName messageWithTypeTblName m.message.messageDataInfo.messageId.value m
    let tryLoadMessageWithTypeFs serviceName (MessageId messageId) = tryLoadData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let tryDeleteMessageWithTypeFs serviceName (MessageId messageId) = tryDeleteData<MessageWithType, Guid> serviceName messageWithTypeTblName messageId
    let tryGetMessageWithTypeIdsFs serviceName () = tryGetObjectIds<MessageId> serviceName messageWithTypeTblName (fun e -> e |> Guid.Parse |> MessageId)
    let tryLoadMessageWithTypeAllFs serviceName () = tryLoadObjects<MessageWithType, Guid> serviceName messageWithTypeTblName Guid.Parse

    let trySaveModelDataFs serviceName (m : ModelData) = trySaveData<ModelData, Guid> serviceName modelDataTblName m.modelDataId.value m
    let tryLoadModelDataFs serviceName (ModelDataId modelDataId) = tryLoadData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let tryDeleteModelDataFs serviceName (ModelDataId modelDataId) = tryDeleteData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let tryGetModelDataIdsFs serviceName () = tryGetObjectIds<ModelDataId> serviceName modelDataTblName (fun e -> e |> Guid.Parse |> ModelDataId)
    let tryLoadModelDataAllsFs serviceName () = tryLoadObjects<ModelData, Guid> serviceName modelDataTblName Guid.Parse

    let trySaveWorkerNodeRunModelDataFs serviceName (m : WorkerNodeRunModelData) = trySaveData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName m.remoteProcessId.value m
    let tryLoadWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = tryLoadData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let tryDeleteWorkerNodeRunModelDataFs serviceName (RemoteProcessId processId) = tryDeleteData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName processId
    let tryGetWorkerNodeRunModelDataIdsFs serviceName () = tryGetObjectIds<RemoteProcessId> serviceName workerNodeRunModelDataTblName (fun e -> e |> Guid.Parse |> RemoteProcessId)
    let tryLoadWorkerNodeRunModelDataAllFs serviceName () = tryLoadObjects<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName Guid.Parse

    let trySaveResultDataFs serviceName (r : ResultDataWithId) = trySaveData<ResultDataWithId, Guid> serviceName resultDataTblName r.resultDataId.value r
    let tryLoadResultDataFs serviceName (ResultDataId resultDataId) = tryLoadData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let tryDeleteResultDataFs serviceName (ResultDataId resultDataId) = tryDeleteData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let tryGetResultDataIdsFs serviceName () = tryGetObjectIds<ResultDataId> serviceName resultDataTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let tryLoadResultDataAllFs serviceName () = tryLoadObjects<ResultDataWithId, Guid> serviceName resultDataTblName Guid.Parse

    let trySaveChartInfoFs serviceName (c : ChartInfo) = trySaveData<ChartInfo, Guid> serviceName chartInfoTblName c.resultDataId.value c
    let tryLoadChartInfoFs serviceName (ResultDataId resultDataId) = tryLoadData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let tryDeleteChartInfoFs serviceName (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let tryGetChartInfoIdsFs serviceName () = tryGetObjectIds<ResultDataId> serviceName chartInfoTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let tryLoadChartInfoAllFs serviceName () = tryLoadObjects<ChartInfo, Guid> serviceName chartInfoTblName Guid.Parse


    let trySaveLocalChartInfo d (c : ChartInfo) =
        let w() =
            try
                let getFileName name =
                    match d with
                    | Some (f, g) -> Path.Combine(f, g.ToString(), Path.GetFileName name)
                    | None -> name

                let trySaveChart f c =
                    let folder = Path.GetDirectoryName f
                    Directory.CreateDirectory(folder) |> ignore
                    File.WriteAllText(f, c)

                c.charts
                |> List.map (fun e -> trySaveChart (getFileName e.chartName) e.chartContent)
                |> ignore
                Ok ()
            with
            | e -> e |> SaveChartsException |> FileErr |> Error

        tryRopFun (fun e -> e |> GeneralFileException |> FileErr) w


    let trySaveWorkerNodeInfoFs serviceName (r : WorkerNodeInfo) = trySaveData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName r.workerNodeId.value.value r
    let tryLoadWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryLoadData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let tryDeleteWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryDeleteData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let tryGetWorkerNodeInfoIdsFs serviceName () = tryGetObjectIds<WorkerNodeId> serviceName workerNodeInfoTblName (fun e -> e |> Guid.Parse |> MessagingClientId |> WorkerNodeId)
    let tryLoadeWorkerNodeInfoAllFs serviceName () = tryLoadObjects<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName Guid.Parse

    let trySaveRunModelParamWithRemoteIdFs serviceName (r : RunModelParamWithRemoteId) = trySaveData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName r.remoteProcessId.value r
    let tryLoadRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = tryLoadData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let tryDeleteRunModelParamWithRemoteIdFs serviceName (RemoteProcessId processId) = tryDeleteData<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName processId
    let tryGetRunModelParamWithRemoteIdIdsFs serviceName () = tryGetObjectIds<RemoteProcessId> serviceName runModelParamWithRemoteIdTblName (fun e -> e |> Guid.Parse |> RemoteProcessId)
    let tryLoadeRunModelParamWithRemoteIdAllFs serviceName () = tryLoadObjects<RunModelParamWithRemoteId, Guid> serviceName runModelParamWithRemoteIdTblName Guid.Parse

    let trySaveWorkerNodeStateFs serviceName (r : WorkerNodeState) = trySaveData<WorkerNodeState, Guid> serviceName workerNodeStateTblName r.workerNodeInfo.workerNodeId.value.value r
    let tryLoadWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = tryLoadData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let tryDeleteWorkerNodeStateFs serviceName (WorkerNodeId (MessagingClientId nodeId)) = tryDeleteData<WorkerNodeState, Guid> serviceName workerNodeStateTblName nodeId
    let tryGetWorkerNodeStateIdsFs serviceName () = tryGetObjectIds<WorkerNodeId> serviceName workerNodeStateTblName (fun e -> e |> Guid.Parse |> MessagingClientId |> WorkerNodeId)
    let tryLoadWorkerNodeStateAllFs serviceName () = tryLoadObjects<WorkerNodeState, Guid> serviceName workerNodeStateTblName Guid.Parse

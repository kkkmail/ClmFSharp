﻿namespace DbData

open Newtonsoft.Json
open FSharp.Data
open System
open ClmSys.VersionInfo
open Clm.Substances
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open DynamicSql
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.WorkerNodeData
open ClmSys
open ClmSys.PartitionerData

// ! Must be the last to open !
open Configuration

module DatabaseTypes =

    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>


    type ClmDefaultValueTable = ClmDB.dbo.Tables.ClmDefaultValue
    type ClmDefaultValueTableRow = ClmDefaultValueTable.Row


    type ClmDefaultValueData = SqlCommandProvider<"
        select * 
        from dbo.ClmDefaultValue
        where clmDefaultValueId = @clmDefaultValueId", ClmConnectionStringValue, ResultType.DataReader>


    type ClmTaskTable = ClmDB.dbo.Tables.ClmTask
    type ClmTaskTableRow = ClmTaskTable.Row


    type ClmTaskData = SqlCommandProvider<"
        select *
        from dbo.ClmTask
        where clmTaskId = @clmTaskId", ClmConnectionStringValue, ResultType.DataReader>


    type ClmTaskByDefaultData = SqlCommandProvider<"
        select top 1 *
        from dbo.ClmTask
        where clmDefaultValueId = @clmDefaultValueId
        order by createdOn", ClmConnectionStringValue, ResultType.DataReader>


    type ClmTaskAllIncompleteData = SqlCommandProvider<"
        select *
        from dbo.ClmTask
        where remainingRepetitions > 0 and clmTaskStatusId = 0
        order by clmTaskOrder", ClmConnectionStringValue, ResultType.DataReader>


    type CommandLineParamTable = ClmDB.dbo.Tables.CommandLineParam
    type CommandLineParamTableRow = CommandLineParamTable.Row


    type CommandLineParamData = SqlCommandProvider<"
        select *
        from dbo.CommandLineParam
        where clmTaskId = @clmTaskId
        order by createdOn", ClmConnectionStringValue, ResultType.DataReader>


    type ModelDataTable = ClmDB.dbo.Tables.ModelData
    type ModelDataTableRow = ModelDataTable.Row


    type ModelDataTableData = SqlCommandProvider<"
        select
            modelDataId,
            clmTaskId,
            fileStructureVersion,
            seedValue,
            modelDataParams,
            modelBinaryData,
            createdOn
        from dbo.ModelData
        where modelDataId = @modelDataId", ClmConnectionStringValue, ResultType.DataReader>


    type ResultDataTable = ClmDB.dbo.Tables.ResultData
    type ResultDataTableRow = ResultDataTable.Row


    type ResultDataTableData = SqlCommandProvider<"
        select *
        from dbo.ResultData
        where resultDataId = @resultDataId", ClmConnectionStringValue, ResultType.DataReader>


    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row


    /// SQL to upsert RunQueue.
    type RunQueueTableData = SqlCommandProvider<"
        select * 
        from dbo.RunQueue
        where runQueueId = @runQueueId", ClmConnectionStringValue, ResultType.DataReader>


    /// SQL to load all not started RunQueue.
    type RunQueueAllTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 0 and r.progress = 0 and t.clmTaskStatusId = 0 and r.workerNodeId is null
        order by runQueueOrder", ClmConnectionStringValue, ResultType.DataReader>


    /// SQL to load RunQueue by runQueueId.
    type RunQueueSingleTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where runQueueId = @runQueueId", ClmConnectionStringValue, ResultType.DataReader>


    /// SQL to load first not started RunQueue.
    type RunQueueFirstTableData = SqlCommandProvider<"
        select top 1
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 0 and r.progress = 0 and t.clmTaskStatusId = 0 and r.workerNodeId is null
        order by runQueueOrder", ClmConnectionStringValue, ResultType.DataReader>


    /// SQL to load all currently running models == total progress.
    /// runQueueStatusId = 2 is InProgressRunQueue.
    type RunQueueProgressTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 2 and t.clmTaskStatusId = 0 and r.workerNodeId is not null
        order by runQueueOrder", ClmConnectionStringValue, ResultType.DataReader>


    type WorkerNodeTable = ClmDB.dbo.Tables.WorkerNode
    type WorkerNodeTableRow = WorkerNodeTable.Row


    type WorkerNodeTableData = SqlCommandProvider<"
        select *
        from dbo.WorkerNode
        where workerNodeId = @workerNodeId", ClmConnectionStringValue, ResultType.DataReader>


    type ClmDefaultValue
        with
        static member create (r : ClmDefaultValueTableRow) =
            {
                clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                defaultRateParams = r.defaultRateParams |> JsonConvert.DeserializeObject<ReactionRateProviderParams>
                description = r.description
            }


    type ModelCommandLineParam
        with

        static member create (r : CommandLineParamTableRow) =
            {
                y0 = r.y0
                tEnd = r.tEnd
                useAbundant = r.useAbundant
            }

        /// TODO kk:20190531 - Perhaps it is worth assigning commandLineParamId on the client OR by database.
        member r.addRow (ClmTaskId clmTaskId) (t : CommandLineParamTable) =
            let newRow =
                t.NewRow(
                        commandLineParamId = Guid.NewGuid(),
                        clmTaskId = clmTaskId,
                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant
                        )

            t.Rows.Add newRow
            newRow


    type ClmTask
        with

        static member tryCreate c (r : ClmTaskTableRow) =
            match r.numberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.maxPeptideLength |> MaxPeptideLength.tryCreate with
            | Some n, Some m ->
                let clmTaskId = r.clmTaskId |> ClmTaskId

                match c clmTaskId with
                | Ok p ->
                    {
                        clmTaskInfo =
                            {
                                clmTaskId = clmTaskId
                                clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = p
                        numberOfRepetitions = r.numberOfRepetitions
                        remainingRepetitions = r.remainingRepetitions
                        createdOn = r.createdOn
                    }
                    |> Ok
                | Error e -> addError ClmTaskTryCreatErr r.clmTaskId e
            | _ -> toError ClmTaskTryCreatErr r.clmTaskId

        member r.addRow (t : ClmTaskTable) =
            let newRow =
                t.NewRow(
                        clmTaskId = r.clmTaskInfo.clmTaskId.value,
                        clmDefaultValueId = r.clmTaskInfo.clmDefaultValueId.value,
                        numberOfAminoAcids = r.clmTaskInfo.numberOfAminoAcids.length,
                        maxPeptideLength = r.clmTaskInfo.maxPeptideLength.length,
                        numberOfRepetitions = r.numberOfRepetitions,
                        remainingRepetitions = r.remainingRepetitions,
                        createdOn = DateTime.Now
                        )

            t.Rows.Add newRow
            newRow


    type ModelData
        with

        static member tryCreate (c : ClmTaskId -> Result<ClmTask, ClmError>) (r : ModelDataTableRow) =
            match r.clmTaskId |> ClmTaskId |> c with
            | Ok i ->
                let rawData =
                    {
                        seedValue = r.seedValue
                        fileStructureVersion = r.fileStructureVersion
                        modelData =
                            {
                                modelDataParams = r.modelDataParams |> JsonConvert.DeserializeObject<ModelDataParams>
                                modelBinaryData = r.modelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                            }
                    }

                {
                    modelDataId = r.modelDataId |> ModelDataId
                    clmTaskInfo = i.clmTaskInfo
                    data = rawData
                }
                |> Ok
            | Error e ->  addError ModelDataTryCreateErr r.modelDataId e


    type ResultDataWithId
        with

        static member create (r : ResultDataTableRow) =
            {
                    resultDataId = r.resultDataId |> ResultDataId
                    workerNodeId = r.workerNodeId |> MessagingClientId |> WorkerNodeId

                    resultData =
                    {
                        modelDataId = r.modelDataId |> ModelDataId

                        y0 = r.y0
                        tEnd = r.tEnd
                        useAbundant = r.useAbundant

                        maxEe = r.maxEe
                        maxAverageEe = r.maxAverageEe
                        maxWeightedAverageAbsEe = r.maxWeightedAverageAbsEe
                        maxLastEe = r.maxLastEe
                    }
            }

        member r.addRow (t : ResultDataTable) =
            let newRow =
                t.NewRow(
                        resultDataId = r.resultDataId.value,
                        workerNodeId = r.workerNodeId .value.value,
                        y0 = r.resultData.y0,
                        tEnd = r.resultData.tEnd,
                        useAbundant = r.resultData.useAbundant,

                        maxEe = r.resultData.maxEe,
                        maxAverageEe = r.resultData.maxAverageEe,
                        maxWeightedAverageAbsEe = r.resultData.maxWeightedAverageAbsEe,
                        maxLastEe = r.resultData.maxLastEe
                        )

            newRow.modelDataId <- r.resultData.modelDataId.value

            t.Rows.Add newRow
            newRow


    type RunQueue
        with

        static member tryCreate i d (r : RunQueueTableRow) =
            match RunQueueStatus.tryCreate r.runQueueStatusId with
            | Some s ->
                {
                    runQueueId = RunQueueId r.runQueueId
                    info =
                        {
                            modelDataId = ModelDataId r.modelDataId
                            defaultValueId = d

                            modelCommandLineParam =
                                {
                                    y0 = r.y0
                                    tEnd = r.tEnd
                                    useAbundant = r.useAbundant
                                }
                        }
                    runQueueStatus = s
                    errorMessageOpt = r.errorMessage |> Option.map ErrorMessage
                    workerNodeIdOpt = r.workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
                    progress = TaskProgress.create r.progress
                    createdOn = r.createdOn
                }
                |> Some
            | None -> None

        member r.addRow (t : RunQueueTable) =
            let newRow =
                t.NewRow(
                        runQueueId = r.runQueueId.value,
                        modelDataId = r.info.modelDataId.value,
                        y0 = r.modelCommandLineParam.y0,
                        tEnd = r.modelCommandLineParam.tEnd,
                        runQueueStatusId = r.runQueueStatus.value,
                        useAbundant = r.modelCommandLineParam.useAbundant,
                        workerNodeId = (r.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value)),
                        progress = r.progress.value,
                        modifiedOn = DateTime.Now
                        )

            newRow.errorMessage <- r.errorMessageOpt |> Option.bind (fun e -> Some e.value)
            t.Rows.Add newRow
            newRow

        /// The following transitions are allowed here:
        ///     NotStartedRunQueue + None (workerNodeId) -> InProgressRunQueue + Some workerNodeId.
        ///     NotStartedRunQueue -> CancelledRunQueue + both None (workerNodeId).
        ///     InProgressRunQueue -> InProgressRunQueue + the same Some workerNodeId + not decreasing progress.
        ///     InProgressRunQueue -> CompletedRunQueue + the same Some workerNodeId (+ the progress will be updated to 1.0).
        ///     InProgressRunQueue -> FailedRunQueue + the same Some workerNodeId.
        ///     InProgressRunQueue -> CancelRequestedRunQueue + the same Some workerNodeId.
        ///     CancelRequestedRunQueue -> CancelledRunQueue + the same Some workerNodeId.
        ///
        /// All others are not allowed and / or out of scope of this function.
        member q.tryUpdateRow (r : RunQueueTableRow) =
            let toError e = e |> RunQueueTryUpdateRowErr |> DbErr |> Error

            let g p s =
                r.runQueueId <- q.runQueueId.value
                r.workerNodeId <- (q.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value))
                r.progress <- p
                r.errorMessage <- q.errorMessageOpt |> Option.bind (fun e -> Some e.value)

                match s with
                | Some v -> r.startedOn <- Some v
                | None -> ignore()

                r.modifiedOn <- DateTime.Now
                r.runQueueStatusId <- q.runQueueStatus.value
                Ok()

            let f s =
                {
                    runQueueId = q.runQueueId
                    runQueueStatusFrom = s
                    runQueueStatusTo = q.runQueueStatus
                    workerNodeIdOptFrom = r.workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
                    workerNodeIdOptTo = q.workerNodeIdOpt
                    progressFrom = r.progress |> TaskProgress.create
                    progressTo = q.progress
                }

            let f1 e = e |> InvalidStatusTransitionErr |> toError
            let f2 e = e |> InvalidDataErr |> toError

            match RunQueueStatus.tryCreate r.runQueueStatusId with
            | Some s ->
                match s, r.workerNodeId, q.runQueueStatus, q.workerNodeIdOpt with
                | NotStartedRunQueue,       None,    InProgressRunQueue,       Some _ -> g NotStarted.value (Some DateTime.Now)
                | NotStartedRunQueue,       None,    CancelledRunQueue,        None -> g TaskProgress.failedValue None
                | InProgressRunQueue,       Some w1, InProgressRunQueue,       Some w2 when w1 = w2.value.value && q.progress.value >= r.progress -> g q.progress.value None
                | InProgressRunQueue,       Some w1, CompletedRunQueue,        Some w2 when w1 = w2.value.value -> g Completed.value None
                | InProgressRunQueue,       Some w1, FailedRunQueue,           Some w2 when w1 = w2.value.value -> g TaskProgress.failedValue None
                | InProgressRunQueue,       Some w1, CancelRequestedRunQueue,  Some w2 when w1 = w2.value.value -> g TaskProgress.failedValue None
                | CancelRequestedRunQueue,  Some w1, CancelledRunQueue,        Some w2 when w1 = w2.value.value -> g TaskProgress.failedValue None
                | _ -> s |> f |> f1
            | None -> InvalidRunQueue |> f |> f2


    type WorkerNodeInfo
        with

        /// TODO kk:20200329 - Note that partitionerId is hard coded. Revisit if necessary.
        static member create (r : WorkerNodeTableRow) =
            {
                workerNodeId = r.workerNodeId |> MessagingClientId |> WorkerNodeId
                workerNodeName = r.workerNodeName |> WorkerNodeName
                noOfCores = r.numberOfCores
                partitionerId = defaultPartitionerId
                nodePriority = r.nodePriority |> WorkerNodePriority
                isInactive = r.isInactive
            }

        member w.addRow (t : WorkerNodeTable) =
            let newRow =
                t.NewRow(
                        workerNodeId = w.workerNodeId.value.value,
                        workerNodeName = w.workerNodeName.value,
                        numberOfCores = w.noOfCores,
                        nodePriority = w.nodePriority.value
                        )

            newRow.modifiedOn <- DateTime.Now
            t.Rows.Add newRow
            newRow

        member w.updateRow (r : WorkerNodeTableRow) =
            r.workerNodeName <- w.workerNodeName.value
            r.numberOfCores <- w.noOfCores
            r.nodePriority <- w.nodePriority.value
            r.modifiedOn <- DateTime.Now


    let loadClmDefaultValue connectionString (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ClmDefaultValueData(conn)
            let t = new ClmDefaultValueTable()
            d.Execute clmDefaultValueId |> t.Load

            t.Rows
            |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId)
            |> Option.bind (fun v -> ClmDefaultValue.create v |> Some)
            |> mapDbError LoadClmDefaultValueErr clmDefaultValueId

        tryDbFun g


    let upsertClmDefaultValue (ConnectionString connectionString) (p : ClmDefaultValue) =
        let g() =
            use cmd = new SqlCommandProvider<"
                merge ClmDefaultValue as target
                using (select @clmDefaultValueId, @defaultRateParams, @description, @fileStructureVersion) as source (clmDefaultValueId, defaultRateParams, description, fileStructureVersion)  
                on (target.clmDefaultValueId = source.clmDefaultValueId)
                when not matched then
                    insert (clmDefaultValueId, defaultRateParams, description, fileStructureVersion)
                    values (source.clmDefaultValueId, source.defaultRateParams, source.description, source.fileStructureVersion)
                when matched then
                    update set defaultRateParams = source.defaultRateParams, description = source.description, fileStructureVersion = source.fileStructureVersion;
            ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let result = cmd.Execute(clmDefaultValueId = p.clmDefaultValueId.value
                                    , defaultRateParams = (p.defaultRateParams |> JsonConvert.SerializeObject)
                                    , description = match p.description with | Some d -> d | None -> null
                                    , fileStructureVersion = FileStructureVersion)

            match result = 1 with
            | true -> Ok ()
            | false -> toError UpsertClmDefaultValueErr p.clmDefaultValueId.value

        tryDbFun g


    let loadCommandLineParams connectionString (ClmTaskId clmTaskId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new CommandLineParamData(conn)
            let t = new CommandLineParamTable()
            d.Execute clmTaskId |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ModelCommandLineParam.create r)
            |> Ok

        tryDbFun g


    let addCommandLineParams connectionString clmTaskId (p : ModelCommandLineParam) =
        let g() =
            use conn = getOpenConn connectionString
            use t = new CommandLineParamTable()
            p.addRow clmTaskId t |> ignore
            t.Update conn |> ignore
            Ok ()

        tryDbFun g


    let loadClmTask connectionString (ClmTaskId clmTaskId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ClmTaskData(conn)
            let t = new ClmTaskTable()
            d.Execute clmTaskId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmTaskId = clmTaskId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams connectionString) v
            | None -> toError LoadClmTaskErr clmTaskId

        tryDbFun g


    let loadClmTaskByDefault connectionString (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ClmTaskByDefaultData(conn)
            let t = new ClmTaskTable()
            d.Execute clmDefaultValueId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams connectionString) v
            | None -> toError LoadClmTaskByDefaultErr clmDefaultValueId

        tryDbFun g


    let loadIncompleteClmTasks connectionString =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ClmTaskAllIncompleteData(conn)
            let t = new ClmTaskTable()
            d.Execute() |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ClmTask.tryCreate (loadCommandLineParams connectionString) r)
            |> Ok

        tryDbFun g


    let addClmTask connectionString (clmTask : ClmTask) =
        let g() =
            use conn = getOpenConn connectionString
            use t = new ClmTaskTable()
            clmTask.addRow t |> ignore
            t.Update conn |> ignore

            clmTask.commandLineParams
            |> List.map (addCommandLineParams connectionString clmTask.clmTaskInfo.clmTaskId)
            |> ignore

            Ok()

        tryDbFun g


    /// Updates remainingRepetitions of ClmTask.
    let updateClmTask connectionString (clmTask : ClmTask) =
        let g() =
            use conn = getOpenConn connectionString
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                    UPDATE dbo.ClmTask
                    SET remainingRepetitions = @remainingRepetitions
                    WHERE clmTaskId = @clmTaskId
                ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let recordsUpdated =
                cmd.Execute(
                    clmTaskId = clmTask.clmTaskInfo.clmTaskId.value,
                    remainingRepetitions = clmTask.remainingRepetitions)

            match recordsUpdated = 1 with
            | true -> Ok ()
            | false -> toError UpdateClmTaskErr clmTask.clmTaskInfo.clmTaskId.value

        tryDbFun g


    let loadModelData connectionString (ModelDataId modelDataId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ModelDataTableData(conn)
            let t = new ModelDataTable()
            d.Execute modelDataId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId) with
            | Some v -> ModelData.tryCreate (loadClmTask connectionString) v
            | None -> toError LoadModelDataError modelDataId

        tryDbFun g


    let upsertModelData connectionString (m : ModelData) =
        let g() =
            use conn = getOpenConn connectionString
            let connectionString = conn.ConnectionString

            let recordsUpdated =
                use cmdWithBinaryData = new SqlCommandProvider<"
                    merge ModelData as target
                    using (select @modelDataId, @clmTaskId, @fileStructureVersion, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
                    as source (modelDataId, clmTaskId, fileStructureVersion, seedValue, modelDataParams, modelBinaryData, createdOn)
                    on (target.modelDataId = source.modelDataId)
                    when not matched then
                        insert (modelDataId, clmTaskId, fileStructureVersion, seedValue, modelDataParams, modelBinaryData, createdOn)
                        values (source.modelDataId, source.clmTaskId, source.fileStructureVersion, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
                    when matched then
                        update set clmTaskId = source.clmTaskId, fileStructureVersion = source.fileStructureVersion, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;
                    ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

                cmdWithBinaryData.Execute(
                    modelDataId = m.modelDataId.value,
                    clmTaskId = m.clmTaskInfo.clmTaskId.value,
                    fileStructureVersion = m.data.fileStructureVersion,
                    seedValue = (match m.data.seedValue with | Some s -> s | None -> -1),
                    modelDataParams = (m.data.modelData.modelDataParams |> JsonConvert.SerializeObject),
                    modelBinaryData = (m.data.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                    createdOn = DateTime.Now)

            match recordsUpdated = 1 with
            | true -> Ok ()
            | false -> toError UpdateModelDataErr m.modelDataId.value

        tryDbFun g


    let saveResultData connectionString (r : ResultDataWithId) =
        let g() =
            use conn = getOpenConn connectionString
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                INSERT INTO dbo.ResultData
                            (resultDataId
                            ,modelDataId
                            ,workerNodeId
                            ,y0
                            ,tEnd
                            ,useAbundant
                            ,maxEe
                            ,maxAverageEe
                            ,maxWeightedAverageAbsEe
                            ,maxLastEe
                            ,createdOn)
                        OUTPUT Inserted.resultDataId
                        VALUES
                            (@resultDataId
                            ,@modelDataId
                            ,@workerNodeId
                            ,@y0
                            ,@tEnd
                            ,@useAbundant
                            ,@maxEe
                            ,@maxAverageEe
                            ,@maxWeightedAverageAbsEe
                            ,@maxLastEe
                            ,@createdOn)
            ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let result =
                cmd.Execute(
                        resultDataId = r.resultDataId.value
                        ,modelDataId = r.resultData.modelDataId.value
                        ,workerNodeId = r.workerNodeId.messagingClientId.value
                        ,y0 = r.resultData.y0
                        ,tEnd = r.resultData.tEnd
                        ,useAbundant = r.resultData.useAbundant
                        ,maxEe = r.resultData.maxEe
                        ,maxAverageEe = r.resultData.maxAverageEe
                        ,maxWeightedAverageAbsEe = r.resultData.maxWeightedAverageAbsEe
                        ,maxLastEe = r.resultData.maxLastEe
                        ,createdOn = DateTime.Now)
                |> Seq.toList

            match result.Length = 1 with
            | true -> Ok ()
            | false -> toError SaveResultDataErr r.resultDataId.value

        tryDbFun g


    let tryLoadResultData connectionString (ResultDataId resultDataId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new ResultDataTableData(conn)
            let t = new ResultDataTable()
            d.Execute(resultDataId = resultDataId) |> t.Load

            t.Rows
            |> Seq.tryFind (fun e -> e.resultDataId = resultDataId)
            |> Option.bind (fun v -> ResultDataWithId.create v |> Some)
            |> Ok

        tryDbFun g


    let private mapRunQueue (reader : DynamicSqlDataReader) =
        match RunQueueStatus.tryCreate reader?runQueueStatusId with
        | Some s ->
            {
                runQueueId = RunQueueId reader?runQueueId
                info =
                    {
                        modelDataId = ModelDataId reader?modelDataId
                        defaultValueId = ClmDefaultValueId reader?clmDefaultValueId

                        modelCommandLineParam =
                            {
                                y0 = reader?y0
                                tEnd = reader?tEnd
                                useAbundant = reader?useAbundant
                            }
                    }
                runQueueStatus = s
                errorMessageOpt = reader?errorMessage |> Option.map ErrorMessage
                workerNodeIdOpt = reader?workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
                progress = TaskProgress.create reader?progress
                createdOn = reader?createdOn
            }
            |> Ok
        | None -> toError MapRunQueueErr (reader?runQueueId)


    let loadRunQueue connectionString =
        let g() =
            seq
                {
                    use conn = getOpenConn connectionString
                    use data = new RunQueueAllTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let loadRunQueueProgress connectionString =
        let g() =
            seq
                {
                    use conn = getOpenConn connectionString
                    use data = new RunQueueProgressTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let tryLoadFirstRunQueue connectionString =
        let g() =
            seq
                {
                    use conn = getOpenConn connectionString
                    use data = new RunQueueFirstTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    let tryLoadRunQueue connectionString (q : RunQueueId) =
        let g() =
            seq
                {
                    use conn = getOpenConn connectionString
                    use data = new RunQueueSingleTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute q.value)
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    let saveRunQueue connectionString modelDataId defaultValueId p =
        let g() =
            use conn = getOpenConn connectionString
            use t = new RunQueueTable()
            let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
            let row = r.addRow t
            t.Update conn |> ignore
            row.runQueueId |> RunQueueId |> Ok

        tryDbFun g


    let deleteRunQueue (ConnectionString connectionString) (runQueueId : RunQueueId) =
        let g() =
            use cmd = new SqlCommandProvider<"delete from dbo.RunQueue where runQueueId = @runQueueId", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            match cmd.Execute(runQueueId = runQueueId.value) = 1 with
            | true -> Ok ()
            | false -> toError DeleteRunQueueEntryErr runQueueId

        tryDbFun g


    let upsertRunQueue connectionString (w : RunQueue) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new RunQueueTableData(conn)
            let t = new RunQueueTable()
            d.Execute w.runQueueId.value |> t.Load

            let result =
                match t.Rows |> Seq.tryFind (fun e -> e.runQueueId = w.runQueueId.value) with
                | Some r -> w.tryUpdateRow r
                | None -> w.addRow t |> ignore; Ok()
                |> Rop.bind (fun () -> t.Update conn |> ignore; Ok())

            result

        tryDbFun g


    let loadWorkerNodeInfo connectionString (i : WorkerNodeId) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new WorkerNodeTableData(conn)
            let t = new WorkerNodeTable()
            d.Execute i.value.value |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.workerNodeId = i.value.value) with
            | Some v -> v |> WorkerNodeInfo.create |> Ok
            | None -> toError LoadWorkerNodeInfoErr i.value.value

        tryDbFun g


    let upsertWorkerNodeInfo connectionString (w : WorkerNodeInfo) =
        let g() =
            use conn = getOpenConn connectionString
            use d = new WorkerNodeTableData(conn)
            let t = new WorkerNodeTable()
            d.Execute w.workerNodeId.value.value |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.workerNodeId = w.workerNodeId.value.value) with
            | Some r -> w.updateRow r
            | None -> w.addRow t |> ignore

            t.Update conn |> ignore
            Ok()

        tryDbFun g


    [<Literal>]
    let availablbeWorkerNodeSql = @"
        ; with q as
        (
        select
            workerNodeId
            ,nodePriority
            ,cast(
                case
                    when numberOfCores <= 0 then 1
                    else (select count(1) as runningModels from RunQueue where workerNodeId = w.workerNodeId and runQueueStatusId in (2, 5)) / (cast(numberOfCores as money))
                end as money) as workLoad
        from WorkerNode w
        )
        select top 1
        workerNodeId
        from q
        where workLoad < 1
        order by nodePriority desc, workLoad, newid()"


    type AvailableWorkerNodeTableData = SqlCommandProvider<availablbeWorkerNodeSql, ClmConnectionStringValue, ResultType.DataReader>


    let tryGetAvailableWorkerNode connectionString =
        let g() =
            use conn = getOpenConn connectionString
            use cmd = new SqlCommandProvider<availablbeWorkerNodeSql, ClmConnectionStringValue, ResultType.DataTable>(conn)
            let table = cmd.Execute()
            
            match table.Rows |> Seq.tryHead with
            | None -> Ok None
            | Some r -> r.workerNodeId |> MessagingClientId |> WorkerNodeId |> Some |> Ok

        tryDbFun g

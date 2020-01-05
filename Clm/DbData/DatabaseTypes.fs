namespace DbData

open System.Data
open System.Data.SqlClient
open Newtonsoft.Json
open FSharp.Data
open Configuration
open System
open ClmSys.VersionInfo
open Clm.Substances
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open ClmSys.MessagingData
open DynamicSql
open ClmSys
open ClmSys.GeneralErrors
open ClmSys.Retry


module DatabaseTypes =

    let private openConnIfClosed (conn : SqlConnection) =
        match conn.State with
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let private mapDbError f i v =
        v
        |> Option.map Ok
        |> Option.defaultValue (i |> f |> DbErr |> Error)


    let private mapException e = e |> DbException |> DbErr
    let private mapExceptionToError e = e |> DbException |> DbErr |> Error
    let private toDbError f i = i |> f |> DbErr |> Error


    let private tryDbFun g =
        let w() =
            try
                g()
            with
            | e -> mapExceptionToError e

        tryRopFun mapException w


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
        where remainingRepetitions > 0 and statusId = 0
        order by createdOn", ClmConnectionStringValue, ResultType.DataReader>


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
            m.modelDataId,
            m.clmTaskId,
            m.parentModelDataId,
            isnull(p.fileStructureVersion, m.fileStructureVersion) as fileStructureVersion,
            isnull(p.seedValue, m.seedValue) as seedValue,
            isnull(p.modelDataParams, m.modelDataParams) as modelDataParams,
            isnull(p.modelBinaryData, m.modelBinaryData) as modelBinaryData,
            m.createdOn
        from
            dbo.ModelData m
            left outer join dbo.ModelData p on m.parentModelDataId = p.modelDataId
        where m.modelDataId = @modelDataId", ClmConnectionStringValue, ResultType.DataReader>


    type ResultDataTable = ClmDB.dbo.Tables.ResultData
    type ResultDataTableRow = ResultDataTable.Row


    type ResultDataTableData = SqlCommandProvider<"
        select *
        from dbo.ResultData
        where resultDataId = @resultDataId", ClmConnectionStringValue, ResultType.DataReader>


    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row


    type RunQueueTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.statusId = 0 and t.statusId = 0
        order by createdOn", ClmConnectionStringValue, ResultType.DataReader>


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

        static member create i (r : CommandLineParamTableRow) =
            {
                taskParam =
                    {
                        y0 = r.y0
                        tEnd = r.tEnd
                        useAbundant = r.useAbundant
                    }

                serviceAccessInfo = i
            }

        /// TODO kk:20190531 - Perhaps it is worth assigning commandLineParamId on the client OR by database.
        member r.addRow (ClmTaskId clmTaskId) (t : CommandLineParamTable) =
            let newRow =
                t.NewRow(
                        commandLineParamId = Guid.NewGuid(),
                        clmTaskId = clmTaskId,
                        y0 = r.taskParam.y0,
                        tEnd = r.taskParam.tEnd,
                        useAbundant = r.taskParam.useAbundant
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
                | Error e -> Error e
            | _ -> toDbError ClmTaskTryCreatError r.clmTaskId

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
                    data =
                        match r.parentModelDataId with
                        | Some p -> ParentProvided (ModelDataId p, rawData)
                        | None -> OwnData rawData
                }
                |> Ok
            | Error e -> Error e


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

        static member create i d (r : RunQueueTableRow) =
            {
                runQueueId = RunQueueId r.runQueueId
                info =
                    {
                        modelDataId = ModelDataId r.modelDataId
                        defaultValueId = d

                        modelCommandLineParam =
                            {
                                taskParam =
                                    {
                                        y0 = r.y0
                                        tEnd = r.tEnd
                                        useAbundant = r.useAbundant
                                    }

                                serviceAccessInfo = i
                            }
                    }
                statusId = r.statusId
            }

        member r.addRow (t : RunQueueTable) =
            let newRow =
                t.NewRow(
                        runQueueId = r.runQueueId.value,
                        modelDataId = r.info.modelDataId.value,
                        y0 = r.modelCommandLineParam.taskParam.y0,
                        tEnd = r.modelCommandLineParam.taskParam.tEnd,
                        useAbundant = r.modelCommandLineParam.taskParam.useAbundant
                        )

            newRow.statusId <- 0

            t.Rows.Add newRow
            newRow


    let tryLoadClmDefaultValue (ConnectionString connectionString) (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            use d = new ClmDefaultValueData(conn)
            let t = new ClmDefaultValueTable()
            d.Execute clmDefaultValueId |> t.Load

            t.Rows
            |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId)
            |> Option.bind (fun v -> ClmDefaultValue.create v |> Some)
            |> mapDbError LoadClmDefaultValueError clmDefaultValueId

        tryDbFun g


    let tryUpsertClmDefaultValue (ConnectionString connectionString) (p : ClmDefaultValue) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            let connectionString = conn.ConnectionString

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
            | false -> toDbError UpsertClmDefaultValueError p.clmDefaultValueId.value

        tryDbFun g


    let loadCommandLineParams (ConnectionString connectionString) i (ClmTaskId clmTaskId) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            use d = new CommandLineParamData(conn)
            let t = new CommandLineParamTable()
            d.Execute clmTaskId |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ModelCommandLineParam.create i r)
            |> Ok

        tryDbFun g


    let addCommandLineParams (ConnectionString connectionString) clmTaskId (p : ModelCommandLineParam) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            use t = new CommandLineParamTable()
            p.addRow clmTaskId t |> ignore
            t.Update conn |> ignore
            Ok ()

        tryDbFun g


    let tryLoadClmTask (connectionString : ConnectionString) i (ClmTaskId clmTaskId) =
        let g() =
            use conn = new SqlConnection(connectionString.value)
            openConnIfClosed conn
            use d = new ClmTaskData(conn)
            let t = new ClmTaskTable()
            d.Execute clmTaskId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmTaskId = clmTaskId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams connectionString i) v
            | None -> toDbError LoadClmTaskError clmTaskId

        tryDbFun g


    let tryLoadClmTaskByDefault (connectionString : ConnectionString) i (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = new SqlConnection(connectionString.value)
            openConnIfClosed conn
            use d = new ClmTaskByDefaultData(conn)
            let t = new ClmTaskTable()
            d.Execute clmDefaultValueId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams connectionString i) v
            | None -> toDbError LoadClmTaskByDefaultError clmDefaultValueId

        tryDbFun g


    let loadIncompleteClmTasks (connectionString : ConnectionString) i =
        let g() =
            use conn = new SqlConnection(connectionString.value)
            openConnIfClosed conn
            use d = new ClmTaskAllIncompleteData(conn)
            let t = new ClmTaskTable()
            d.Execute() |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ClmTask.tryCreate (loadCommandLineParams connectionString i) r)
            |> Ok

        tryDbFun g


    let addClmTask (connectionString : ConnectionString) (clmTask : ClmTask) =
        let g() =
            use conn = new SqlConnection(connectionString.value)
            openConnIfClosed conn
            use t = new ClmTaskTable()
            let row = clmTask.addRow t
            t.Update conn |> ignore
            let clmTaskId = row.clmTaskId |> ClmTaskId
            let newClmTask = { clmTask with clmTaskInfo = { clmTask.clmTaskInfo with clmTaskId = clmTaskId } }

            newClmTask.commandLineParams
            |> List.map (addCommandLineParams connectionString clmTaskId)
            |> ignore

            Ok newClmTask

        tryDbFun g


    let tryUpdateClmTask (ConnectionString connectionString) (clmTask : ClmTask) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
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
            | false -> toDbError UpdateClmTaskError clmTask.clmTaskInfo.clmTaskId.value

        tryDbFun g


    let tryLoadModelData (connectionString : ConnectionString) i (ModelDataId modelDataId) =
        let g() =
            use conn = new SqlConnection(connectionString.value)
            openConnIfClosed conn
            use d = new ModelDataTableData(conn)
            let t = new ModelDataTable()
            d.Execute modelDataId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId) with
            | Some v -> ModelData.tryCreate (tryLoadClmTask connectionString i) v
            | None -> toDbError LoadModelDataError modelDataId

        tryDbFun g


    let tryUpdateModelData (ConnectionString connectionString) (m : ModelData) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            let connectionString = conn.ConnectionString

            let recordsUpdated =
                match m.data with
                | OwnData d ->
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
                        fileStructureVersion = d.fileStructureVersion,
                        seedValue = (match d.seedValue with | Some s -> s | None -> -1),
                        modelDataParams = (d.modelData.modelDataParams |> JsonConvert.SerializeObject),
                        modelBinaryData = (d.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                        createdOn = DateTime.Now)
                | ParentProvided (ModelDataId parentModelDataId, _) ->
                    use cmdWithoutBinaryData = new SqlCommandProvider<"
                        merge ModelData as target
                        using (select @modelDataId, @clmTaskId, @parentModelDataId, @createdOn)
                        as source (modelDataId, clmTaskId, parentModelDataId, createdOn)
                        on (target.modelDataId = source.modelDataId)
                        when not matched then
                            insert (modelDataId, clmTaskId, parentModelDataId, createdOn)
                            values (source.modelDataId, source.clmTaskId, source.parentModelDataId, source.createdOn)
                        when matched then
                            update set clmTaskId = source.clmTaskId, parentModelDataId = source.parentModelDataId, createdOn = source.createdOn;
                        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

                    cmdWithoutBinaryData.Execute(
                        modelDataId = m.modelDataId.value,
                        clmTaskId = m.clmTaskInfo.clmTaskId.value,
                        parentModelDataId = parentModelDataId,
                        createdOn = DateTime.Now)

            match recordsUpdated = 1 with
            | true -> Ok ()
            | false -> toDbError UpdateModelDataError m.modelDataId.value

        tryDbFun g


    let saveResultData (ConnectionString connectionString) (r : ResultDataWithId) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
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
            | false -> toDbError SaveResultDataError r.resultDataId.value

        tryDbFun g

    let tryLoadResultData (ConnectionString connectionString) (ResultDataId resultDataId) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            use d = new ResultDataTableData(conn)
            let t = new ResultDataTable()
            d.Execute(resultDataId = resultDataId) |> t.Load

            t.Rows
            |> Seq.tryFind (fun e -> e.resultDataId = resultDataId)
            |> Option.bind (fun v -> ResultDataWithId.create v |> Some)
            |> mapDbError LoadResultDataError resultDataId

        tryDbFun g


    let loadRunQueue (ConnectionString connectionString) i =
        let g() =
            seq
                {
                    use conn = new SqlConnection(connectionString)
                    openConnIfClosed conn
                    use data = new RunQueueTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())

                    while (reader.Read()) do
                        yield
                            {
                                runQueueId = RunQueueId reader?runQueueId
                                info =
                                    {
                                        modelDataId = ModelDataId reader?modelDataId
                                        defaultValueId = ClmDefaultValueId reader?clmDefaultValueId

                                        modelCommandLineParam =
                                            {
                                                taskParam =
                                                    {
                                                        y0 = reader?y0
                                                        tEnd = reader?tEnd
                                                        useAbundant = reader?useAbundant
                                                    }

                                                serviceAccessInfo = i
                                            }
                                    }
                                statusId = reader?statusId
                            }
                        }
            |> List.ofSeq
            |> List.map Ok
            |> Ok

        tryDbFun g


    let saveRunQueueEntry (ConnectionString connectionString) modelDataId defaultValueId p =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn
            use t = new RunQueueTable()
            let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
            let row = r.addRow t
            t.Update conn |> ignore
            row.runQueueId |> RunQueueId |> Ok

        tryDbFun g


    let deleteRunQueueEntry (ConnectionString connectionString) (RunQueueId runQueueId) =
        let g() =
            use conn = new SqlConnection(connectionString)
            openConnIfClosed conn

            use cmd = new SqlCommandProvider<"
                DELETE FROM dbo.RunQueue where runQueueId = @runQueueId", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let rowsAffected = cmd.Execute(runQueueId = runQueueId)
            //Ok rowsAffected

            match rowsAffected = 1 with
            | true -> Ok ()
            | false -> toDbError DeleteRunQueueEntryError runQueueId

        tryDbFun g

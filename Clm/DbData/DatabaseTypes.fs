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


/// You must add reference to System.Configuration !
module DatabaseTypes =

    let openConnIfClosed (conn : SqlConnection) =
        match conn.State with
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>


    type ClmDefaultValueTable = ClmDB.dbo.Tables.ClmDefaultValue
    type ClmDefaultValueTableRow = ClmDefaultValueTable.Row
    type ClmDefaultValueData = SqlCommandProvider<"select * from dbo.ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId", ClmConnectionStringValue, ResultType.DataReader>
    //type TruncateClmDefaultValueTbl = SqlCommandProvider<"truncate table dbo.ClmDefaultValue", ClmSqlProviderName, ConfigFile = AppConfigFile>

    type ClmTaskTable = ClmDB.dbo.Tables.ClmTask
    type ClmTaskTableRow = ClmTaskTable.Row
    type ClmTaskData = SqlCommandProvider<"select * from dbo.ClmTask where clmTaskId = @clmTaskId", ClmConnectionStringValue, ResultType.DataReader>
    type ClmTaskByDefaultData = SqlCommandProvider<"select top 1 * from dbo.ClmTask where clmDefaultValueId = @clmDefaultValueId order by clmTaskId", ClmConnectionStringValue, ResultType.DataReader>
    type ClmTaskAllIncompleteData = SqlCommandProvider<"select * from dbo.ClmTask where remainingRepetitions > 0", ClmConnectionStringValue, ResultType.DataReader>

    type CommandLineParamTable = ClmDB.dbo.Tables.CommandLineParam
    type CommandLineParamTableRow = CommandLineParamTable.Row
    type CommandLineParamData = SqlCommandProvider<"select * from dbo.CommandLineParam where clmTaskId = @clmTaskId", ClmConnectionStringValue, ResultType.DataReader>

    type ModelDataTable = ClmDB.dbo.Tables.ModelData
    type ModelDataTableRow = ModelDataTable.Row
    type ModelDataTableData = SqlCommandProvider<"select * from dbo.ModelData where modelDataId = @modelDataId", ClmConnectionStringValue, ResultType.DataReader>

    type ResultDataTable = ClmDB.dbo.Tables.ResultData
    type ResultDataTableRow = ResultDataTable.Row
    type ResultDataTableData = SqlCommandProvider<"select * from dbo.ResultData where resultDataId = @resultDataId", ClmConnectionStringValue, ResultType.DataReader>

    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row
    type RunQueueTableData = SqlCommandProvider<"select * from dbo.RunQueue where statusId = 0", ClmConnectionStringValue, ResultType.DataReader>


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

        static member create(r : CommandLineParamTableRow) =
            {
                y0 = r.y0
                tEnd = r.tEnd
                useAbundant = r.useAbundant
            }

        member r.addRow (ClmTaskId clmTaskId) (t : CommandLineParamTable) =
            let newRow =
                t.NewRow(
                        clmTaskId = clmTaskId,
                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant
                        )

            t.Rows.Add newRow
            newRow


    type ClmTask
        with

        static member tryCreate (r : ClmTaskTableRow) (c : ClmTaskId -> list<ModelCommandLineParam>) =
            match r.numberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.maxPeptideLength |> MaxPeptideLength.tryCreate with
            | Some n, Some m ->
                let clmTaskId = r.clmTaskId |> ClmTaskId

                {
                    clmTaskInfo =
                        {
                            clmTaskId = clmTaskId
                            clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                            numberOfAminoAcids = n
                            maxPeptideLength = m
                        }
                    commandLineParams = c clmTaskId
                    numberOfRepetitions = r.numberOfRepetitions
                    remainingRepetitions = r.remainingRepetitions
                    createdOn = r.createdOn
                }
                |> Some
            | _ -> None

        member r.addRow (t : ClmTaskTable) =
            let newRow =
                t.NewRow(
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

        static member tryCreate (c : ClmTaskId -> ClmTask option) (r : ModelDataTableRow) =
            match r.clmTaskId |> ClmTaskId |> c with
            | Some i ->
                {
                    modelDataId = r.modelDataId |> ModelDataId
                    clmTaskInfo = i.clmTaskInfo
                    fileStructureVersion = r.fileStructureVersion
                    seedValue = r.seedValue

                    modelData =
                        {
                            modelDataParams = r.modelDataParams |> JsonConvert.DeserializeObject<ModelDataParams>
                            modelBinaryData = r.modelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                        }
                }
                |> Some
            | None -> None


    type ResultDataWithId
        with

        static member create (r : ResultDataTableRow) =
            {
                    resultDataId = r.resultDataId |> ResultDataId

                    resultData =
                    {
                        modelDataId = r.modelDataId |> ModelDataId


                        y0 = r.y0
                        tEnd = r.tEnd
                        useAbundant = r.useAbundant

                        maxEe = r.maxEe
                        maxAverageEe = r.maxAverageEe
                    }
            }


    type ResultData
        with

        member r.addRow (t : ResultDataTable) =
            let newRow =
                t.NewRow(
                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant,

                        maxEe = r.maxEe,
                        maxAverageEe = r.maxAverageEe
                        )

            newRow.modelDataId <- r.modelDataId.value

            t.Rows.Add newRow
            newRow


    type RunQueue
        with

        static member create (r : RunQueueTableRow) =
            {
                runQueueId = RunQueueId r.runQueueId
                info =
                    {
                        modelDataId = ModelDataId r.modelDataId

                        modelCommandLineParam =
                            {
                                y0 = r.y0
                                tEnd = r.tEnd
                                useAbundant = r.useAbundant
                            }
                    }
                statusId = r.statusId
            }


    type RunQueueInfo
        with

        member r.addRow (t : RunQueueTable) =
            let newRow =
                t.NewRow(
                        modelDataId = r.modelDataId.value,
                        y0 = r.modelCommandLineParam.y0,
                        tEnd = r.modelCommandLineParam.tEnd,
                        useAbundant = r.modelCommandLineParam.useAbundant
                        )

            newRow.statusId <- 0

            t.Rows.Add newRow
            newRow


    let tryLoadClmDefaultValue (ClmDefaultValueId clmDefaultValueId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new ClmDefaultValueData(conn)
        let t = new ClmDefaultValueTable()
        d.Execute clmDefaultValueId |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId) |> Option.bind (fun v -> ClmDefaultValue.create v |> Some)


    let tryUpsertClmDefaultValue (p : ClmDefaultValue) (ConnectionString connectionString) =
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

        cmd.Execute(clmDefaultValueId = p.clmDefaultValueId.value
                    , defaultRateParams = (p.defaultRateParams |> JsonConvert.SerializeObject)
                    , description = match p.description with | Some d -> d | None -> null
                    , fileStructureVersion = FileStructureVersion)


    let loadCommandLineParams (ClmTaskId clmTaskId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new CommandLineParamData(conn)
        let t = new CommandLineParamTable()
        d.Execute clmTaskId |> t.Load

        t.Rows
        |> Seq.toList
        |> List.map (fun r -> ModelCommandLineParam.create r)


    let addCommandLineParams clmTaskId (p : ModelCommandLineParam) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new CommandLineParamTable()
        p.addRow clmTaskId t |> ignore
        t.Update conn |> ignore


    let tryLoadClmTask (ClmTaskId clmTaskId) (connectionString : ConnectionString) =
        use conn = new SqlConnection(connectionString.value)
        openConnIfClosed conn
        use d = new ClmTaskData(conn)
        let t = new ClmTaskTable()
        d.Execute clmTaskId |> t.Load

        t.Rows
        |> Seq.tryFind (fun e -> e.clmTaskId = clmTaskId)
        |> Option.bind (fun v -> ClmTask.tryCreate v (fun c -> loadCommandLineParams c connectionString))


    let tryLoadClmTaskByDefault (ClmDefaultValueId clmDefaultValueId) (connectionString : ConnectionString) =
        use conn = new SqlConnection(connectionString.value)
        openConnIfClosed conn
        use d = new ClmTaskByDefaultData(conn)
        let t = new ClmTaskTable()
        d.Execute clmDefaultValueId |> t.Load

        t.Rows
        |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId)
        |> Option.bind (fun v -> ClmTask.tryCreate v (fun c -> loadCommandLineParams c connectionString))


    let loadIncompleteClmTasks (connectionString : ConnectionString) =
        use conn = new SqlConnection(connectionString.value)
        openConnIfClosed conn
        use d = new ClmTaskAllIncompleteData(conn)
        let t = new ClmTaskTable()
        d.Execute() |> t.Load

        t.Rows
        |> Seq.toList
        |> List.map (fun r -> ClmTask.tryCreate r (fun c -> loadCommandLineParams c connectionString))
        |> List.choose id


    let addClmTask (clmTask : ClmTask) (connectionString : ConnectionString) =
        use conn = new SqlConnection(connectionString.value)
        openConnIfClosed conn
        use t = new ClmTaskTable()
        let row = clmTask.addRow t
        t.Update conn |> ignore
        let clmTaskId = row.clmTaskId |> ClmTaskId
        let newClmTask = { clmTask with clmTaskInfo = { clmTask.clmTaskInfo with clmTaskId = clmTaskId } }

        newClmTask.commandLineParams
        |> List.map (fun e -> addCommandLineParams clmTaskId e connectionString)
        |> ignore

        newClmTask


    let tryUpdateClmTask (clmTask : ClmTask) (ConnectionString connectionString) =
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

        recordsUpdated = 1


    let getNewModelDataId (ClmTaskId clmTaskId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new ModelDataTable()

        let r =
            t.NewRow(
                    clmTaskId = clmTaskId,
                    fileStructureVersion = FileStructureVersion,
                    seedValue = None,
                    modelDataParams = "",
                    modelBinaryData = [||],
                    createdOn = DateTime.Now
                    )

        t.Rows.Add r
        t.Update conn |> ignore
        ModelDataId r.modelDataId


    let tryLoadModelData (ModelDataId modelDataId) (connectionString : ConnectionString) =
        use conn = new SqlConnection(connectionString.value)
        openConnIfClosed conn
        use d = new ModelDataTableData(conn)
        let t = new ModelDataTable()
        d.Execute modelDataId |> t.Load

        t.Rows
        |> Seq.tryFind (fun e -> e.modelDataId = modelDataId)
        |> Option.bind (fun v -> ModelData.tryCreate (fun c -> tryLoadClmTask c connectionString) v)


    let tryUpdateModelData (m : ModelData) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let connectionString = conn.ConnectionString

        use cmdWithBinaryData = new SqlCommandProvider<"
            UPDATE dbo.ModelData
                SET clmTaskId = @clmTaskId
                    ,fileStructureVersion = @fileStructureVersion
                    ,seedValue = @seedValue
                    ,modelDataParams = @modelDataParams
                    ,modelBinaryData = @modelBinaryData
                    ,createdOn = @createdOn
            WHERE modelDataId = @modelDataId
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let recordsUpdated =
            cmdWithBinaryData.Execute(
                clmTaskId = m.clmTaskInfo.clmTaskId.value,
                fileStructureVersion = m.fileStructureVersion,
                seedValue = (match m.seedValue with | Some s -> s | None -> -1),
                modelDataParams = (m.modelData.modelDataParams |> JsonConvert.SerializeObject),
                modelBinaryData = (m.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                createdOn = DateTime.Now,
                modelDataId = m.modelDataId.value)

        recordsUpdated = 1


    let saveResultData (r : ResultData) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let connectionString = conn.ConnectionString

        use cmd = new SqlCommandProvider<"
            INSERT INTO dbo.ResultData
                       (modelDataId
                       ,y0
                       ,tEnd
                       ,useAbundant
                       ,maxEe
                       ,maxAverageEe
                       ,createdOn)
                 OUTPUT Inserted.resultDataId
                 VALUES
                       (@modelDataId
                       ,@y0
                       ,@tEnd
                       ,@useAbundant
                       ,@maxEe
                       ,@maxAverageEe
                       ,@createdOn)
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let resultDataId =
            cmd.Execute(
                        modelDataId = r.modelDataId.value
                        ,y0 = r.y0
                        ,tEnd = r.tEnd
                        ,useAbundant = r.useAbundant
                        ,maxEe = r.maxEe
                        ,maxAverageEe = r.maxAverageEe
                        ,createdOn = DateTime.Now)

        resultDataId |> Seq.head |> ResultDataId


    let tryLoadResultData (ResultDataId resultDataId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new ResultDataTableData(conn)
        let t = new ResultDataTable()
        d.Execute(resultDataId = resultDataId) |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.resultDataId = resultDataId) |> Option.bind (fun v -> ResultDataWithId.create v |> Some)


    let loadRunQueue (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let runQueueTable = new RunQueueTable()
        (new RunQueueTableData(conn)).Execute() |> runQueueTable.Load

        runQueueTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> RunQueue.create e)


    let saveRunQueueEntry modelDataId p (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new RunQueueTable()
        let r = RunQueueInfo.fromModelCommandLineParam modelDataId p
        let row = r.addRow t
        t.Update conn |> ignore
        row.runQueueId |> RunQueueId


    let deleteRunQueueEntry (RunQueueId runQueueId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn

        use cmd = new SqlCommandProvider<"
            DELETE FROM dbo.RunQueue where runQueueId = @runQueueId", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let rowsAffected = cmd.Execute(runQueueId = runQueueId)
        rowsAffected

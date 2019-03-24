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


    type ModelDataTable = ClmDB.dbo.Tables.ModelData
    type ModelDataTableRow = ModelDataTable.Row
    type ModelDataTableData = SqlCommandProvider<"select * from dbo.ModelData where modelDataId = @modelDataId", ClmConnectionStringValue, ResultType.DataReader>

    type ResultDataTable = ClmDB.dbo.Tables.ResultData
    type ResultDataTableRow = ResultDataTable.Row
    type ResultDataTableData = SqlCommandProvider<"select * from dbo.ResultData where resultDataId = @resultDataId", ClmConnectionStringValue, ResultType.DataReader>

    type ClmDefaultValueTable = ClmDB.dbo.Tables.ClmDefaultValue
    type ClmDefaultValueTableRow = ClmDefaultValueTable.Row
    type ClmDefaultValueData = SqlCommandProvider<"select * from dbo.ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId", ClmConnectionStringValue, ResultType.DataReader>
    type TruncateClmDefaultValueTbl = SqlCommandProvider<"truncate table dbo.ClmDefaultValue", ClmSqlProviderName, ConfigFile = AppConfigFile>

    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row
    type RunQueueTableData = SqlCommandProvider<"select * from dbo.RunQueue where statusId = 0", ClmConnectionStringValue, ResultType.DataReader>

    type TaskTable = ClmDB.dbo.Tables.Task
    type TaskTableRow = TaskTable.Row
    type TaskData = SqlCommandProvider<"select * from dbo.Task where taskId = @taskId", ClmConnectionStringValue, ResultType.DataReader>
    type TaskAllIncompleteData = SqlCommandProvider<"select * from dbo.Task where completed = 0", ClmConnectionStringValue, ResultType.DataReader>
    type TruncateTaskTbl = SqlCommandProvider<"truncate table dbo.ClmDefaultValue", ClmSqlProviderName, ConfigFile = AppConfigFile>



    type ClmDefaultValue
        with
        static member create (r : ClmDefaultValueTableRow) =
            {
                clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                defaultRateParams = r.defaultRateParams |> JsonConvert.DeserializeObject<ReactionRateProviderParams>
                description = r.description
            }


    type ClmTask
        with
        static member tryCreate (r : TaskTableRow) =
            match r.numberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.maxPeptideLength |> MaxPeptideLength.tryCreate with
            | Some n, Some m ->
                {
                    clmTaskId = r.taskId |> ClmTaskId
                    clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                    numberOfAminoAcids = n
                    maxPeptideLength = m
                    y0 = r.y0
                    tEnd = r.tEnd
                    useAbundant = r.useAbundant
                    repeat = r.repeat
                    completed = r.completed
                    createdOn = r.createdOn
                }
                |> Some
            | _ -> None


    type ModelData
        with

        static member tryCreate (r : ModelDataTableRow) =
            let n() = NumberOfAminoAcids.tryCreate r.numberOfAminoAcids
            let m() = MaxPeptideLength.tryCreate r.maxPeptideLength

            match n(), m() with
            | Some numberOfAminoAcids, Some maxPeptideLength ->
                {
                    modelDataId = r.modelDataId |> ModelDataId
                    numberOfAminoAcids = numberOfAminoAcids
                    maxPeptideLength = maxPeptideLength
                    clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                    fileStructureVersion = r.fileStructureVersion
                    seedValue = r.seedValue

                    modelData =
                        {
                            modelDataParams = r.modelDataParams |> JsonConvert.DeserializeObject<ModelDataParams>
                            modelBinaryData = r.modelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                        }

                }
                |> Some
            | _ -> None


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


    let tryloadClmDefaultValue (ClmDefaultValueId clmDefaultValueId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new ClmDefaultValueData(conn)
        let t = new ClmDefaultValueTable()
        d.Execute clmDefaultValueId |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId) |> Option.bind (fun v -> ClmDefaultValue.create v |> Some)


    let truncateAllDefaults (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new TruncateClmDefaultValueTbl(conn)
        t.Execute() |> ignore


    //let saveAllParams (p : AllParams) (ConnectionString connectionString) =
    //    use conn = new SqlConnection(connectionString)
    //    openConnIfClosed conn
    //    let connectionString = conn.ConnectionString
    //
    //    use cmd = new SqlCommandProvider<"
    //        INSERT INTO dbo.DefaultSet
    //                   (defaultSetIndex
    //                   ,defaultSet
    //                   ,fileStructureVersion)
    //             VALUES
    //                   (@defaultSetIndex
    //                   ,@defaultSet
    //                   ,@fileStructureVersion)
    //    ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
    //
    //    cmd.Execute(defaultSetIndex = p.modelGenerationParams.defaultSetIndex
    //                , defaultSet = (p.modelGenerationParams |> JsonConvert.SerializeObject)
    //                , fileStructureVersion = p.modelGenerationParams.fileStructureVersion)


    let getNewModelDataId (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new ModelDataTable()

        let r =
            t.NewRow(
                    numberOfAminoAcids = 0,
                    maxPeptideLength = 0,
                    fileStructureVersion = FileStructureVersion,
                    clmDefaultValueId = -1L,
                    seedValue = None,
                    modelDataParams = "",
                    modelBinaryData = [||],
                    createdOn = DateTime.Now
                    )

        t.Rows.Add r
        t.Update conn |> ignore
        ModelDataId r.modelDataId


    let tryLoadModelData (ModelDataId modelDataId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new ModelDataTableData(conn)
        let t = new ModelDataTable()
        d.Execute modelDataId |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId) |> Option.bind (fun v -> ModelData.tryCreate v)


    let tryUpdateModelData (m : ModelData) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let connectionString = conn.ConnectionString

        use cmdWithBinaryData = new SqlCommandProvider<"
            UPDATE dbo.ModelData
                SET numberOfAminoAcids = @numberOfAminoAcids
                    ,maxPeptideLength = @maxPeptideLength
                    ,clmDefaultValueId = @clmDefaultValueId
                    ,fileStructureVersion = @fileStructureVersion
                    ,seedValue = @seedValue
                    ,modelDataParams = @modelDataParams
                    ,modelBinaryData = @modelBinaryData
                    ,createdOn = @createdOn
            WHERE modelDataId = @modelDataId
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let recordsUpdated =
            cmdWithBinaryData.Execute(
                numberOfAminoAcids = m.numberOfAminoAcids.length,
                maxPeptideLength = m.maxPeptideLength.length,
                clmDefaultValueId = m.clmDefaultValueId.value,
                fileStructureVersion = m.fileStructureVersion,
                seedValue = (match m.seedValue with | Some s -> s | None -> -1),
                modelDataParams = (m.modelData.modelDataParams |> JsonConvert.SerializeObject),
                modelBinaryData = (m.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                createdOn = DateTime.Now,
                modelDataId = m.modelDataId.value)

        if recordsUpdated = 1 then true else false


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

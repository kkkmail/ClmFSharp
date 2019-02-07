namespace DbData

open System.Data
open System.Data.SqlClient
open Newtonsoft.Json
open FSharp.Data
open Configuration
open System
open ClmSys.VersionInfo
open ClmSys.IndeterministicData
open Clm.Substances
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open Clm.SettingsExt


/// You must add reference to System.Configuration !
module DatabaseTypes =
    open Clm.ReactionTypes


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

    type ResultSettingTable = ClmDB.dbo.Tables.ResultSetting
    type ResultSettingTableRow = ResultSettingTable.Row
    type ResultSettingTableData = SqlCommandProvider<"select * from dbo.ResultSetting where resultDataId = @resultDataId", ClmConnectionStringValue, ResultType.DataReader>

    type ModelSettingTable = ClmDB.dbo.Tables.ModelSetting
    type ModelSettingTableRow = ModelSettingTable.Row
    type ModelSettingTableData = SqlCommandProvider<"select * from dbo.ModelSetting where modelDataId = @modelDataId", ClmConnectionStringValue, ResultType.DataReader>

    type SettingTable = ClmDB.dbo.Tables.Setting
    type SettingTableRow = SettingTable.Row
    type SettingTableAllData = SqlCommandProvider<"select * from dbo.Setting", ClmConnectionStringValue, ResultType.DataReader>
    type TruncateSettingTbl = SqlCommandProvider<"truncate table dbo.Setting", ClmSqlProviderName, ConfigFile = AppConfigFile>

    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row
    type RunQueueTableData = SqlCommandProvider<"select * from dbo.RunQueue where statusId = 0", ClmConnectionStringValue, ResultType.DataReader>


    type Setting
        with

        static member create (r : SettingTableRow) =
            {
                settingId = Some r.settingId
                settingPath =
                    match r.settingField1 with | EmptyString -> [] | s -> [ s, r.settingOrderId1 ]
                    @ match r.settingField2 with | EmptyString -> [] | s -> [ s, r.settingOrderId2 ]
                    @ match r.settingField3 with | EmptyString -> [] | s -> [ s, r.settingOrderId3 ]
                    @ match r.settingField4 with | EmptyString -> [] | s -> [ s, r.settingOrderId4 ]
                    @ match r.settingField5 with | EmptyString -> [] | s -> [ s, r.settingOrderId5 ]
                    @ match r.settingField6 with | EmptyString -> [] | s -> [ s, r.settingOrderId6 ]
                    @ match r.settingField7 with | EmptyString -> [] | s -> [ s, r.settingOrderId7 ]
                    @ match r.settingField8 with | EmptyString -> [] | s -> [ s, r.settingOrderId8 ]
                    @ match r.settingField9 with | EmptyString -> [] | s -> [ s, r.settingOrderId9 ]
                    @ match r.settingField10 with | EmptyString -> [] | s -> [ s, r.settingOrderId10 ]

                settingBit = r.settingBit
                settingLong = r.settingLong
                settingMoney = r.settingMoney
                settingFloat = r.settingFloat
                settingDate = r.settingDate
                settingText = r.settingText
                settingMemo = r.settingMemo
                settingGUID = r.settingGUID
            }

        member r.addRow (t : SettingTable) = 
            let r0 = r.settingPath

            let getSR (x : list<string * int>) =
                match x with
                | [] -> EmptyString, 0, []
                | (h, i) :: t -> h, i, t

            let s1, i1, r1 = getSR r0
            let s2, i2, r2 = getSR r1
            let s3, i3, r3 = getSR r2
            let s4, i4, r4 = getSR r3
            let s5, i5, r5 = getSR r4
            let s6, i6, r6 = getSR r5
            let s7, i7, r7 = getSR r6
            let s8, i8, r8 = getSR r7
            let s9, i9, r9 = getSR r8
            let s10, i10, r10 = getSR r9

            if r10.IsEmpty |> not then failwith (sprintf "Path is too long: %A" r0)

            let newRow =
                t.NewRow(
                        settingField1 = s1,
                        settingOrderId1 = i1,
                        settingField2 = s2,
                        settingOrderId2 = i2,
                        settingField3 = s3,
                        settingOrderId3 = i3,
                        settingField4 = s4,
                        settingOrderId4 = i4,
                        settingField5 = s5,
                        settingOrderId5 = i5,
                        settingField6 = s6,
                        settingOrderId6 = i6,
                        settingField7 = s7,
                        settingOrderId7 = i7,
                        settingField8 = s8,
                        settingOrderId8 = i8,
                        settingField9 = s9,
                        settingOrderId9 = i9,
                        settingField10 = s10,
                        settingOrderId10 = i10,
                        settingBit = r.settingBit,
                        settingLong = r.settingLong,
                        settingMoney = r.settingMoney,
                        settingFloat = r.settingFloat,
                        settingDate = r.settingDate,
                        settingText = r.settingText,
                        settingMemo = r.settingMemo,
                        settingGUID = r.settingGUID
                        )

            t.Rows.Add newRow


    type ResultSettings
        with

        static member createSetting (r : ResultSettingTableRow) =
            {
                settingId = Some r.resultSettingId
                settingPath =
                    match r.settingField1 with | EmptyString -> [] | s -> [ s, r.settingOrderId1 ]
                    @ match r.settingField2 with | EmptyString -> [] | s -> [ s, r.settingOrderId2 ]
                    @ match r.settingField3 with | EmptyString -> [] | s -> [ s, r.settingOrderId3 ]
                    @ match r.settingField4 with | EmptyString -> [] | s -> [ s, r.settingOrderId4 ]
                    @ match r.settingField5 with | EmptyString -> [] | s -> [ s, r.settingOrderId5 ]
                    @ match r.settingField6 with | EmptyString -> [] | s -> [ s, r.settingOrderId6 ]
                    @ match r.settingField7 with | EmptyString -> [] | s -> [ s, r.settingOrderId7 ]
                    @ match r.settingField8 with | EmptyString -> [] | s -> [ s, r.settingOrderId8 ]
                    @ match r.settingField9 with | EmptyString -> [] | s -> [ s, r.settingOrderId9 ]
                    @ match r.settingField10 with | EmptyString -> [] | s -> [ s, r.settingOrderId10 ]

                settingBit = r.settingBit
                settingLong = r.settingLong
                settingMoney = r.settingMoney
                settingFloat = r.settingFloat
                settingDate = r.settingDate
                settingText = r.settingText
                settingMemo = r.settingMemo
                settingGUID = r.settingGUID
            }


        member rs.addRows (t : ResultSettingTable) =
            let addRow (r : Setting) =
                let r0 = r.settingPath

                let getSR (x : list<string * int>) =
                    match x with
                    | [] -> EmptyString, 0, []
                    | (h, i) :: t -> h, i, t

                let s1, i1, r1 = getSR r0
                let s2, i2, r2 = getSR r1
                let s3, i3, r3 = getSR r2
                let s4, i4, r4 = getSR r3
                let s5, i5, r5 = getSR r4
                let s6, i6, r6 = getSR r5
                let s7, i7, r7 = getSR r6
                let s8, i8, r8 = getSR r7
                let s9, i9, r9 = getSR r8
                let s10, i10, r10 = getSR r9

                if r10.IsEmpty |> not then failwith (sprintf "Path is too long: %A" r0)

                let resultSettingId =
                    match r.settingId with
                    | Some g -> g
                    | None -> Guid.NewGuid()

                let newRow =
                    t.NewRow(
                            resultSettingId = resultSettingId,
                            resultDataId = rs.resultDataId.value,
                            settingField1 = s1,
                            settingOrderId1 = i1,
                            settingField2 = s2,
                            settingOrderId2 = i2,
                            settingField3 = s3,
                            settingOrderId3 = i3,
                            settingField4 = s4,
                            settingOrderId4 = i4,
                            settingField5 = s5,
                            settingOrderId5 = i5,
                            settingField6 = s6,
                            settingOrderId6 = i6,
                            settingField7 = s7,
                            settingOrderId7 = i7,
                            settingField8 = s8,
                            settingOrderId8 = i8,
                            settingField9 = s9,
                            settingOrderId9 = i9,
                            settingField10 = s10,
                            settingOrderId10 = i10,
                            settingBit = r.settingBit,
                            settingLong = r.settingLong,
                            settingMoney = r.settingMoney,
                            settingFloat = r.settingFloat,
                            settingDate = r.settingDate,
                            settingText = r.settingText,
                            settingMemo = r.settingMemo,
                            settingGUID = r.settingGUID
                            )

                newRow.resultDataId <- rs.resultDataId.value
                t.Rows.Add newRow

            rs.settings |> Map.toList |> List.map (fun (_, s) -> addRow s) |> ignore


    type ModelSettings
        with

        static member createSetting (r : ModelSettingTableRow) =
            {
                settingId = Some r.modelSettingId
                settingPath =
                    match r.settingField1 with | EmptyString -> [] | s -> [ s, r.settingOrderId1 ]
                    @ match r.settingField2 with | EmptyString -> [] | s -> [ s, r.settingOrderId2 ]
                    @ match r.settingField3 with | EmptyString -> [] | s -> [ s, r.settingOrderId3 ]
                    @ match r.settingField4 with | EmptyString -> [] | s -> [ s, r.settingOrderId4 ]
                    @ match r.settingField5 with | EmptyString -> [] | s -> [ s, r.settingOrderId5 ]
                    @ match r.settingField6 with | EmptyString -> [] | s -> [ s, r.settingOrderId6 ]
                    @ match r.settingField7 with | EmptyString -> [] | s -> [ s, r.settingOrderId7 ]
                    @ match r.settingField8 with | EmptyString -> [] | s -> [ s, r.settingOrderId8 ]
                    @ match r.settingField9 with | EmptyString -> [] | s -> [ s, r.settingOrderId9 ]
                    @ match r.settingField10 with | EmptyString -> [] | s -> [ s, r.settingOrderId10 ]

                settingBit = r.settingBit
                settingLong = r.settingLong
                settingMoney = r.settingMoney
                settingFloat = r.settingFloat
                settingDate = r.settingDate
                settingText = r.settingText
                settingMemo = r.settingMemo
                settingGUID = r.settingGUID
            }


        member rs.addRows (t : ModelSettingTable) =
            let addRow (r : Setting) =
                let r0 = r.settingPath

                let getSR (x : list<string * int>) =
                    match x with
                    | [] -> EmptyString, 0, []
                    | (h, i) :: t -> h, i, t

                let s1, i1, r1 = getSR r0
                let s2, i2, r2 = getSR r1
                let s3, i3, r3 = getSR r2
                let s4, i4, r4 = getSR r3
                let s5, i5, r5 = getSR r4
                let s6, i6, r6 = getSR r5
                let s7, i7, r7 = getSR r6
                let s8, i8, r8 = getSR r7
                let s9, i9, r9 = getSR r8
                let s10, i10, r10 = getSR r9

                if r10.IsEmpty |> not then failwith (sprintf "Path is too long: %A" r0)

                let modelSettingId =
                    match r.settingId with
                    | Some g -> g
                    | None -> Guid.NewGuid()

                let newRow =
                    t.NewRow(
                            modelSettingId = modelSettingId,
                            modelDataId = rs.modelDataId.value,
                            settingField1 = s1,
                            settingOrderId1 = i1,
                            settingField2 = s2,
                            settingOrderId2 = i2,
                            settingField3 = s3,
                            settingOrderId3 = i3,
                            settingField4 = s4,
                            settingOrderId4 = i4,
                            settingField5 = s5,
                            settingOrderId5 = i5,
                            settingField6 = s6,
                            settingOrderId6 = i6,
                            settingField7 = s7,
                            settingOrderId7 = i7,
                            settingField8 = s8,
                            settingOrderId8 = i8,
                            settingField9 = s9,
                            settingOrderId9 = i9,
                            settingField10 = s10,
                            settingOrderId10 = i10,
                            settingBit = r.settingBit,
                            settingLong = r.settingLong,
                            settingMoney = r.settingMoney,
                            settingFloat = r.settingFloat,
                            settingDate = r.settingDate,
                            settingText = r.settingText,
                            settingMemo = r.settingMemo,
                            settingGUID = r.settingGUID
                            )

                newRow.modelDataId <- rs.modelDataId.value
                t.Rows.Add newRow

            rs.settings |> Map.toList |> List.map (fun (_, s) -> addRow s) |> ignore


    type ModelData
        with

        static member tryCreate (r : ModelDataTableRow) (s : ModelSettings) =
            let seeder = Seeder.create r.seedValue
            let n() = NumberOfAminoAcids.tryCreate r.numberOfAminoAcids
            let m() = MaxPeptideLength.tryCreate r.maxPeptideLength
            let p() = ModelDataParams.tryCreate s seeder

            match n(), m(), p() with
            | Some numberOfAminoAcids, Some maxPeptideLength, Some modelDataParams ->
                {
                    modelDataId = r.modelDataId |> ModelDataId
                    numberOfAminoAcids = numberOfAminoAcids
                    maxPeptideLength = maxPeptideLength
                    seedValue = r.seedValue
                    fileStructureVersion = r.fileStructureVersion

                    modelData =
                        {
                            modelDataParams = modelDataParams
                            modelBinaryData = r.modelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                        }

                    defaultSetIndex = r.defaultSetIndex
                }
                |> Some
            | _ -> None


    type ResultData
        with

        static member tryCreate (r : ResultDataTableRow) =
            match NumberOfAminoAcids.tryCreate r.numberOfAminoAcids, MaxPeptideLength.tryCreate r.maxPeptideLength with
            | Some numberOfAminoAcids, Some maxPeptideLength ->
                let a() = r.aminoAcids |> Option.bind (fun v -> v |> unZip |> JsonConvert.DeserializeObject<list<AminoAcid>> |> Some)
                let b() = r.allSubst |> Option.bind (fun v -> v  |> unZip |> JsonConvert.DeserializeObject<list<Substance>> |> Some)
                let c() = r.allInd |> Option.bind (fun v -> v  |> unZip |> JsonConvert.DeserializeObject<list<Substance * int>> |> Map.ofList |> Some)
                let d() = r.allRawReactions |> Option.bind (fun v -> v |> unZip |> JsonConvert.DeserializeObject<list<ReactionName * int>> |> Some)
                let e() = r.allReactions |> Option.bind (fun v -> v |> unZip |> JsonConvert.DeserializeObject<list<ReactionName * int>> |> Some)
                let f() = r.x |> Option.bind (fun v -> v  |> unZip |> JsonConvert.DeserializeObject<double [,]> |> Some)
                let g() = r.t |> Option.bind (fun v -> v  |> unZip |> JsonConvert.DeserializeObject<double []> |> Some)

                {
                    simpleData =
                        {
                            resultDataId = r.resultDataId |> ResultDataId |> Some
                            modelDataId = r.modelDataId |> ModelDataId
                            numberOfAminoAcids = numberOfAminoAcids
                            maxPeptideLength = maxPeptideLength

                            y0 = r.y0
                            tEnd = r.tEnd
                            useAbundant = r.useAbundant

                            maxEe = r.maxEe
                            maxAverageEe = r.maxAverageEe
                        }

                    binaryDataOpt =
                        match a(), b(), c(), d(), e(), f(), g() with
                        | Some a1, Some b1, Some c1, Some d1, Some e1, Some f1, Some g1 ->
                            {
                                aminoAcids = a1
                                allSubst = b1
                                allInd = c1
                                allRawReactions = d1
                                allReactions = e1

                                x = f1
                                t = g1
                            }
                            |> Some
                        | _ -> None
                }
                |> Some
            | _ -> None

        member r.addRow (t : ResultDataTable) =
            let newRow =
                t.NewRow(
                        numberOfAminoAcids = r.simpleData.numberOfAminoAcids.length,
                        maxPeptideLength = r.simpleData.maxPeptideLength.length,

                        y0 = r.simpleData.y0,
                        tEnd = r.simpleData.tEnd,
                        useAbundant = r.simpleData.useAbundant,

                        maxEe = r.simpleData.maxEe,
                        maxAverageEe = r.simpleData.maxAverageEe,

                        aminoAcids = (r.binaryDataOpt |> Option.bind (fun v -> v.aminoAcids |> JsonConvert.SerializeObject |> zip |> Some)),
                        allSubst = (r.binaryDataOpt |> Option.bind (fun v -> v.allSubst |> JsonConvert.SerializeObject |> zip |> Some)),
                        allInd = (r.binaryDataOpt |> Option.bind (fun v -> v.allInd |> Map.toList |> JsonConvert.SerializeObject |> zip |> Some)),
                        allRawReactions = (r.binaryDataOpt |> Option.bind (fun v -> v.allRawReactions |> JsonConvert.SerializeObject |> zip |> Some)),
                        allReactions = (r.binaryDataOpt |> Option.bind (fun v -> v.allReactions |> JsonConvert.SerializeObject |> zip |> Some)),

                        x = (r.binaryDataOpt |> Option.bind (fun v -> v.x |> JsonConvert.SerializeObject |> zip |> Some)),
                        t = (r.binaryDataOpt |> Option.bind (fun v -> v.t |> JsonConvert.SerializeObject |> zip |> Some))
                        )

            newRow.modelDataId <- r.simpleData.modelDataId.value

            match r.simpleData.resultDataId with
            | Some v -> newRow.modelDataId <- v.value
            | None -> ignore()

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
                        y0 = r.y0
                        tEnd = r.tEnd
                        useAbundant = r.useAbundant
                    }
                statusId = r.statusId
            }


    type RunQueueInfo
        with

        member r.addRow (t : RunQueueTable) =
            let newRow =
                t.NewRow(
                        modelDataId = r.modelDataId.value,
                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant
                        )

            newRow.statusId <- 0

            t.Rows.Add newRow
            newRow


    let loadSettings (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let settingTable = new SettingTable()
        (new SettingTableAllData(conn)).Execute() |> settingTable.Load

        settingTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> Setting.create e)
        |> List.map (fun e -> e.settingPath, e)
        |> Map.ofList


    let truncateSettings (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use truncateSettingTbl = new TruncateSettingTbl(conn)
        truncateSettingTbl.Execute() |> ignore


    let saveSettings (settings : list<Setting>) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let settingTable = new SettingTable()
        settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
        let inserted = settingTable.Update(conn)
        printfn "inserted = %A" inserted


    let loadResultSettings (ResultDataId resultDataId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let settingTable = new ResultSettingTable()
        (new ResultSettingTableData(conn)).Execute resultDataId |> settingTable.Load

        let settings =
            settingTable.Rows
            |> List.ofSeq
            |> List.map (fun e -> ResultSettings.createSetting e)
            |> List.map (fun e -> e.settingPath, e)
            |> Map.ofList

        {
            resultDataId = ResultDataId resultDataId
            settings = settings
        }


    let saveResultSettings (rs : ResultSettings) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let t = new ResultSettingTable()
        rs.addRows(t) |> ignore
        t.Update(conn) |> ignore


    let loadModelSettings (ModelDataId modelDataId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let settingTable = new ModelSettingTable()
        (new ModelSettingTableData(conn)).Execute modelDataId |> settingTable.Load

        let settings =
            settingTable.Rows
            |> List.ofSeq
            |> List.map (fun e -> ModelSettings.createSetting e)
            |> List.map (fun e -> e.settingPath, e)
            |> Map.ofList

        {
            modelDataId = ModelDataId modelDataId
            settings = settings
        }


    let saveModelSettings (rs : ModelSettings) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn

        use cmd = new SqlCommandProvider<"
            DELETE FROM dbo.ModelSetting where modelDataId = @modelDataId", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        cmd.Execute(modelDataId = rs.modelDataId.value) |> ignore

        let t = new ModelSettingTable()
        rs.addRows(t) |> ignore
        t.Update(conn) |> ignore


    let getNewModelDataId (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new ModelDataTable()

        let r =
            t.NewRow(
                    numberOfAminoAcids = 0,
                    maxPeptideLength = 0,
                    seedValue = None,
                    fileStructureVersion = FileStructureVersionNumber,
                    defaultSetIndex = -1,
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
        d.Execute(modelDataId = modelDataId) |> t.Load
        let s = loadModelSettings (ModelDataId modelDataId) (ConnectionString connectionString)
        t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId) |> Option.bind (fun v -> ModelData.tryCreate v s)


    let tryUpdateModelData (m : ModelData) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let connectionString = conn.ConnectionString

        use cmdWithBinaryData = new SqlCommandProvider<"
            UPDATE dbo.ModelData
                SET numberOfAminoAcids = @numberOfAminoAcids
                    ,maxPeptideLength = @maxPeptideLength
                    ,seedValue = @seedValue
                    ,defaultSetIndex = @defaultSetIndex
                    ,fileStructureVersion = @fileStructureVersion
                    ,modelBinaryData = @modelBinaryData
                    ,createdOn = @createdOn
            WHERE modelDataId = @modelDataId
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let recordsUpdated =
            cmdWithBinaryData.Execute(
                numberOfAminoAcids = m.numberOfAminoAcids.length,
                maxPeptideLength = m.maxPeptideLength.length,
                seedValue = (match m.seedValue with | Some s -> s | None -> -1),
                defaultSetIndex = m.defaultSetIndex,
                fileStructureVersion = m.fileStructureVersion,
                modelBinaryData = (m.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                createdOn = DateTime.Now,
                modelDataId = m.modelDataId.value)

        if recordsUpdated = 1 then true else false


    let saveResultData (r : ResultData) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let connectionString = conn.ConnectionString

        use cmdWithBinary = new SqlCommandProvider<"
            INSERT INTO dbo.ResultData
                       (modelDataId
                       ,numberOfAminoAcids
                       ,maxPeptideLength
                       ,y0
                       ,tEnd
                       ,useAbundant
                       ,maxEe
                       ,maxAverageEe
                       ,createdOn
                       ,aminoAcids
                       ,allSubst
                       ,allInd
                       ,allRawReactions
                       ,allReactions
                       ,x
                       ,t)
                 OUTPUT Inserted.resultDataId
                 VALUES
                       (@modelDataId
                       ,@numberOfAminoAcids
                       ,@maxPeptideLength
                       ,@y0
                       ,@tEnd
                       ,@useAbundant
                       ,@maxEe
                       ,@maxAverageEe
                       ,@createdOn
                       ,@aminoAcids
                       ,@allSubst
                       ,@allInd
                       ,@allRawReactions
                       ,@allReactions
                       ,@x
                       ,@t)
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        use cmdWithoutBinary = new SqlCommandProvider<"
            INSERT INTO dbo.ResultData
                       (modelDataId
                       ,numberOfAminoAcids
                       ,maxPeptideLength
                       ,y0
                       ,tEnd
                       ,useAbundant
                       ,maxEe
                       ,maxAverageEe
                       ,createdOn)
                 OUTPUT Inserted.resultDataId
                 VALUES
                       (@modelDataId
                       ,@numberOfAminoAcids
                       ,@maxPeptideLength
                       ,@y0
                       ,@tEnd
                       ,@useAbundant
                       ,@maxEe
                       ,@maxAverageEe
                       ,@createdOn)
        ", ClmConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

        let resultDataId =
            match r.binaryDataOpt with
            | Some b ->
                cmdWithBinary.Execute(
                            modelDataId = r.simpleData.modelDataId.value
                           ,numberOfAminoAcids = r.simpleData.numberOfAminoAcids.length
                           ,maxPeptideLength = r.simpleData.maxPeptideLength.length
                           ,y0 = r.simpleData.y0
                           ,tEnd = r.simpleData.tEnd
                           ,useAbundant = r.simpleData.useAbundant
                           ,maxEe = r.simpleData.maxEe
                           ,maxAverageEe = r.simpleData.maxAverageEe
                           ,createdOn = DateTime.Now
                           ,aminoAcids = (b.aminoAcids |> JsonConvert.SerializeObject |> zip)
                           ,allSubst =(b.allSubst |> JsonConvert.SerializeObject |> zip)
                           ,allInd =(b.allInd |> Map.toList |> JsonConvert.SerializeObject |> zip)
                           ,allRawReactions =(b.allRawReactions |> JsonConvert.SerializeObject |> zip)
                           ,allReactions =(b.allReactions |> JsonConvert.SerializeObject |> zip)
                           ,x = (b.x |> JsonConvert.SerializeObject |> zip)
                           ,t = (b.t |> JsonConvert.SerializeObject |> zip)
                    )
            | None ->
                cmdWithoutBinary.Execute(
                            modelDataId = r.simpleData.modelDataId.value
                           ,numberOfAminoAcids = r.simpleData.numberOfAminoAcids.length
                           ,maxPeptideLength = r.simpleData.maxPeptideLength.length
                           ,y0 = r.simpleData.y0
                           ,tEnd = r.simpleData.tEnd
                           ,useAbundant = r.simpleData.useAbundant
                           ,maxEe = r.simpleData.maxEe
                           ,maxAverageEe = r.simpleData.maxAverageEe
                           ,createdOn = DateTime.Now
                    )

        resultDataId |> Seq.head |> ResultDataId


    let tryLoadResultData (ResultDataId resultDataId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use d = new ResultDataTableData(conn)
        let t = new ResultDataTable()
        d.Execute(resultDataId = resultDataId) |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.resultDataId = resultDataId) |> Option.bind ResultData.tryCreate


    let loadRunQueue (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        let runQueueTable = new RunQueueTable()
        (new RunQueueTableData(conn)).Execute() |> runQueueTable.Load

        runQueueTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> RunQueue.create e)


    let saveRunQueueEntry (p : ModelCommandLineParam) (ModelDataId modelId) (ConnectionString connectionString) =
        use conn = new SqlConnection(connectionString)
        openConnIfClosed conn
        use t = new RunQueueTable()
        let r = RunQueueInfo.fromModelCommandLineParam p (ModelDataId modelId)
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

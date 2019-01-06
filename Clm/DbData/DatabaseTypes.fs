namespace DbData

open System.Data
open System.Data.SqlClient
open Newtonsoft.Json
open FSharp.Data
open Configuration
open System
open Clm.VersionInfo
open Clm.Substances
open Clm.GeneralData
open Clm.ModelParams


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
    type ModelDataTableData = SqlCommandProvider<"select * from dbo.ModelData where modelDataId = @modelDataId", ClmConnectionString, ResultType.DataReader>

    type ResultDataTable = ClmDB.dbo.Tables.ResultData
    type ResultDataTableRow = ResultDataTable.Row
    type ResultDataTableData = SqlCommandProvider<"select * from dbo.ResultData where resultDataId = @resultDataId", ClmConnectionString, ResultType.DataReader>

    type ResultSettingTable = ClmDB.dbo.Tables.ResultSetting
    type ResultSettingTableRow = ResultSettingTable.Row
    type ResultSettingTableData = SqlCommandProvider<"select * from dbo.ResultSetting where resultDataId = @resultDataId", ClmConnectionString, ResultType.DataReader>

    type SettingTable = ClmDB.dbo.Tables.Setting
    type SettingTableRow = SettingTable.Row
    type SettingTableAllData = SqlCommandProvider<"select * from dbo.Setting", ClmConnectionString, ResultType.DataReader>
    type TruncateSettingTbl = SqlCommandProvider<"truncate table dbo.Setting", ClmSqlProviderName, ConfigFile = AppConfigFile>

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
                            resultDataId = rs.resultDataId,
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

                newRow.resultDataId <- rs.resultDataId
                t.Rows.Add newRow

            rs.settings |> Map.toList |> List.map (fun (_, s) -> addRow s) |> ignore


    type ResultData
        with

        static member tryCreate (r : ResultDataTableRow) =
            match NumberOfAminoAcids.tryCreate r.numberOfAminoAcids, MaxPeptideLength.tryCreate r.maxPeptideLength with
            | Some numberOfAminoAcids, Some maxPeptideLength -> 
                {
                    resultDataId = r.resultDataId |> Some
                    modelDataId = r.modelDataId
                    numberOfAminoAcids = numberOfAminoAcids
                    maxPeptideLength = maxPeptideLength

                    aminoAcids = JsonConvert.DeserializeObject<list<AminoAcid>>(r.aminoAcids)
                    allSubst = JsonConvert.DeserializeObject<list<Substance>>(r.allSubst)
                    allInd = JsonConvert.DeserializeObject<list<Substance * int>>(r.allInd) |> Map.ofList
                    allRawReactions = JsonConvert.DeserializeObject<list<ReactionName * int>>(r.allRawReactions)
                    allReactions = JsonConvert.DeserializeObject<list<ReactionName * int>>(r.allReactions)

                    y0 = r.y0
                    tEnd = r.tEnd
                    useAbundant = r.useAbundant
                    x = JsonConvert.DeserializeObject<double [,]>(r.x)
                    t = JsonConvert.DeserializeObject<double []>(r.t)
                    maxEe = r.maxEe
                }
                |> Some
            | _ -> None

        member r.addRow (t : ResultDataTable) =
            let newRow =
                t.NewRow(
                        numberOfAminoAcids = r.numberOfAminoAcids.length,
                        maxPeptideLength = r.maxPeptideLength.length,

                        aminoAcids = JsonConvert.SerializeObject r.aminoAcids,
                        allSubst = JsonConvert.SerializeObject r.allSubst,
                        allInd = (r.allInd |> Map.toList |> JsonConvert.SerializeObject),
                        allRawReactions = JsonConvert.SerializeObject r.allRawReactions,
                        allReactions = JsonConvert.SerializeObject r.allReactions,

                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant,
                        x = JsonConvert.SerializeObject r.x,
                        t = JsonConvert.SerializeObject r.t,
                        maxEe = r.maxEe
                        )

            newRow.modelDataId <- r.modelDataId

            match r.resultDataId with
            | Some v -> newRow.modelDataId <- v
            | None -> ignore()

            t.Rows.Add newRow


    let loadSettings (conn : SqlConnection) =
        let settingTable = new SettingTable()
        (new SettingTableAllData(conn)).Execute() |> settingTable.Load

        settingTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> Setting.create e)
        |> List.map (fun e -> e.settingPath, e)
        |> Map.ofList


    let loadResultSettings (conn : SqlConnection) resultDataId =
        let settingTable = new ResultSettingTable()
        (new ResultSettingTableData(conn)).Execute resultDataId |> settingTable.Load

        let settings =
            settingTable.Rows
            |> List.ofSeq
            |> List.map (fun e -> ResultSettings.createSetting e)
            |> List.map (fun e -> e.settingPath, e)
            |> Map.ofList

        {
            resultDataId = resultDataId
            settings = settings
        }


    let truncateSettings (conn : SqlConnection) =
        use truncateSettingTbl = new TruncateSettingTbl(conn)
        truncateSettingTbl.Execute() |> ignore


    let saveSettings (conn : SqlConnection) (settings : list<Setting>) =
        let settingTable = new SettingTable()
        settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
        let inserted = settingTable.Update(conn)
        printfn "inserted = %A" inserted


    let saveResultSettings (conn : SqlConnection) (rs : ResultSettings) =
        let t = new ResultSettingTable()
        rs.addRows(t) |> ignore
        t.Update(conn) |> ignore


    let getNewModelDataId conn =
        openConnIfClosed conn
        use t = new ModelDataTable()

        let r =
            t.NewRow(
                    numberOfAminoAcids = 0,
                    maxPeptideLength = 0,
                    seedValue = None,
                    fileStructureVersion = FileStructureVersionNumber,
                    modelData = EmptyString,
                    createdOn = DateTime.Now
                    )

        t.Rows.Add r
        t.Update conn |> ignore
        r.modelDataId


    let tryLoadModelData conn modelDataId =
        openConnIfClosed conn
        use d = new ModelDataTableData(conn)
        let t = new ModelDataTable()
        d.Execute(modelDataId = modelDataId) |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId)


    let tryUpdateModelData conn (m : ModelData) =
        openConnIfClosed conn
        use d = new ModelDataTableData(conn)
        let t = new ModelDataTable()
        d.Execute(modelDataId = m.modelDataId) |> t.Load

        match t.Rows |> Seq.tryFind (fun e -> e.modelDataId = m.modelDataId) with
        | Some r ->
            r.numberOfAminoAcids <- m.numberOfAminoAcids.length
            r.maxPeptideLength <- m.maxPeptideLength.length
            r.seedValue <- m.seedValue
            r.fileStructureVersion <- m.fileStructureVersion
            r.modelData <- m.modelData
            t.Update(conn) |> ignore
            true
        | None -> false


    let saveResultData (r : ResultData) (conn : SqlConnection) =
        let t = new ResultDataTable()
        r.addRow t |> ignore
        t.Update(conn)


    let tryLoadResultData conn resultDataId = 
        openConnIfClosed conn
        use d = new ResultDataTableData(conn)
        let t = new ResultDataTable()
        d.Execute(resultDataId = resultDataId) |> t.Load
        t.Rows |> Seq.tryFind (fun e -> e.resultDataId = resultDataId)
        |> Option.bind ResultData.tryCreate

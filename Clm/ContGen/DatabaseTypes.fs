namespace ContGen

open System.Data
open System.Data.SqlClient
open FSharp.Data
//open Microsoft.SqlServer.Types

open Configuration

module DatabaseTypes = 
    open System

    [<Literal>]
    let EmptyString = ""

    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>


    type SettingTable = ClmDB.dbo.Tables.Setting
    type SettingTableRow = SettingTable.Row
    type SettingTableData = SqlCommandProvider<"select * from dbo.Setting", ClmConnectionString, ResultType.DataReader>

    type TruncateSettingTbl = SqlCommandProvider<"truncate table dbo.Setting", ClmSqlProviderName, ConfigFile = AppConfigFile>

    type ModelDataTable = ClmDB.dbo.Tables.ModelData
    type ModelDataTableRow = ModelDataTable.Row


    type Setting = 
        {
            settingId : Guid option
            settingPath : list<string * int>
            settingBit : bool
            settingLong : int64
            settingMoney : decimal
            settingFloat : float
            settingDate : DateTime option
            settingText : string option
            settingMemo : string option
            settingGUID : Guid option
        }

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

        static member defaultValue() = 
            {
                settingId = Guid.NewGuid() |> Some
                settingPath = []
                settingBit = false
                settingLong = 0L
                settingMoney = 0m
                settingFloat = 0.0
                settingDate = None
                settingText = None
                settingMemo = None
                settingGUID = None
            }

        member r.addRow (t : SettingTable) = 
            let r0 = r.settingPath

            let getSR x = 
                match x with
                | [] -> EmptyString, []
                | (h, _) :: t -> h, t

            let s1, r1 = getSR r0
            let s2, r2 = getSR r1
            let s3, r3 = getSR r2
            let s4, r4 = getSR r3
            let s5, r5 = getSR r4
            let s6, r6 = getSR r5
            let s7, r7 = getSR r6
            let s8, r8 = getSR r7
            let s9, r9 = getSR r8
            let s10, r10 = getSR r9

            if r10.IsEmpty |> not then failwith (sprintf "Path is too long: %A" r0)

            let newRow = 
                t.NewRow(
                        settingField1 = s1,
                        settingField2 = s2,
                        settingField3 = s3,
                        settingField4 = s4,
                        settingField5 = s5,
                        settingField6 = s6,
                        settingField7 = s7,
                        settingField8 = s8,
                        settingField9 = s9,
                        settingField10 = s10,
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


    type SettingMap = Map<list<string * int>, Setting>


    let openConnIfClosed (conn : SqlConnection) = 
        match conn.State with 
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let loadSettings (connStr : string) =
        use conn : SqlConnection = new SqlConnection (connStr)
        conn.Open()

        let settingTable = new SettingTable()
        (new SettingTableData(conn)).Execute() |> settingTable.Load

        settingTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> Setting.create e)
        |> List.map (fun e -> e.settingPath, e)
        |> Map.ofList



    let truncateSettings (conn : SqlConnection) =
        use truncateSettingTbl = new TruncateSettingTbl(conn)
        truncateSettingTbl.Execute() |> ignore


    let saveSettings (settings : list<Setting>) (conn : SqlConnection) =
        let settingTable = new SettingTable()
        settings |> List.map (fun s -> s.addRow(settingTable)) |> ignore
        let inserted = settingTable.Update(conn)
        printfn "inserted = %A" inserted


    let addDefaultModelDataTable (t : ModelDataTable) =
        let newRow = 
            t.NewRow(
                    numberOfAminoAcids = 0,
                    maxPeptideLength = 0,
                    seed = 0,
                    fileStructureVersion = "",
                    modelData = "",
                    createdOn = DateTime.Now
                    )

        t.Rows.Add newRow
        newRow


    let saveDetaultModelDataTable (conn : SqlConnection) =
        let t = new ModelDataTable()
        let r = addDefaultModelDataTable t
        t.Update(conn) |> ignore
        r

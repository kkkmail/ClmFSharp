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


    type Setting = 
        {
            settingId : int64
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
                settingId = r.settingId
                settingPath =
                    match r.settingField1 with | EmptyString -> [] | s -> [ s, r.settingOrderId1 ]
                    @ match r.settingField2 with | EmptyString -> [] | s -> [ s, r.settingOrderId2 ]
                    @ match r.settingField3 with | EmptyString -> [] | s -> [ s, r.settingOrderId3 ]
                    @ match r.settingField4 with | EmptyString -> [] | s -> [ s, r.settingOrderId4 ]
                    @ match r.settingField5 with | EmptyString -> [] | s -> [ s, r.settingOrderId5 ]
                    @ match r.settingField6 with | EmptyString -> [] | s -> [ s, r.settingOrderId6 ]
                    @ match r.settingField7 with | EmptyString -> [] | s -> [ s, r.settingOrderId7 ]
                    @ match r.settingField8 with | EmptyString -> [] | s -> [ s, r.settingOrderId8 ]

                settingBit = r.settingBit
                settingLong = r.settingLong
                settingMoney = r.settingMoney
                settingFloat = r.settingFloat
                settingDate = r.settingDate
                settingText = r.settingText
                settingMemo = r.settingMemo
                settingGUID = r.settingGUID
            }

        static member defaultValue = 
            {
                settingId = 0L
                settingPath  = []
                settingBit = false
                settingLong = 0L
                settingMoney = 0m
                settingFloat = 0.0
                settingDate = None
                settingText = None
                settingMemo = None
                settingGUID = None
            }


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


    //let loadSettings (conn : SqlConnection) = 
    //    openConnIfClosed conn
    //    use data = new SystemSettingTableData(conn)
    //    use reader = new DynamicSqlDataReader(data.Execute())
    //    [ while reader.Read() do yield (reader?City, reader?CityOriginalName) |> City.tryCreate ]



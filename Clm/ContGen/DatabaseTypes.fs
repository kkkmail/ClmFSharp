namespace ContGen

open System.Data
open System.Data.SqlClient
open FSharp.Data
open Microsoft.SqlServer.Types

open Configuration
open DynamicSql

module DatabaseTypes = 
    open System

    [<Literal>]
    let EmptyString = ""

    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>


    type SystemSettingTable = ClmDB.dbo.Tables.SystemSetting
    type SystemSettingTableRow = SystemSettingTable.Row
    type SystemSettingTableData = SqlCommandProvider<"select * from dbo.SystemSetting", ClmConnectionString, ResultType.DataReader>


    type SystemSetting = 
        {
            systemSettingId : int64
            settingName : string
            settingOrderId : int64
            settingBit : bool
            settingLong : int64
            settingMoney : decimal
            settingFloat : float
            settingDate : DateTime option
            settingText : string option
            settingMemo : string option
            settingGUID : Guid option
            settingInfo : string option
        }

        static member separator = "."

        static member create (r : SystemSettingTableRow) = 
            {
                systemSettingId = r.systemSettingId
                settingName = 
                    r.settingName + 
                    match r.settingField1 with 
                    | EmptyString -> EmptyString
                    | s -> 
                        SystemSetting.separator + s +
                        match r.settingField2 with 
                        | EmptyString -> EmptyString
                        | s -> SystemSetting.separator + s

                settingOrderId = r.settingOrderId
                settingBit = r.settingBit
                settingLong = r.settingLong
                settingMoney = r.settingMoney
                settingFloat = r.settingFloat
                settingDate = r.settingDate
                settingText = r.settingText
                settingMemo = r.settingMemo
                settingGUID = r.settingGUID
                settingInfo = r.settingInfo
            }


    type SystemSettingMap = Map<string, Map<int64, SystemSetting>>


    let openConnIfClosed (conn : SqlConnection) = 
        match conn.State with 
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let loadSettings (connStr : string) = //: SystemSettingMap =
        use conn : SqlConnection = new SqlConnection (connStr)
        conn.Open()

        let systemSettingTable = new SystemSettingTable()
        (new SystemSettingTableData(conn)).Execute() |> systemSettingTable.Load

        systemSettingTable.Rows
        |> List.ofSeq
        |> List.map (fun e -> SystemSetting.create e)
        |> List.groupBy (fun e -> e.settingName)
        |> List.map (fun (e, v) -> e, v |> List.map (fun r -> r.settingOrderId, r) |> Map.ofList)
        |> Map.ofList


    //let loadSettings (conn : SqlConnection) = 
    //    openConnIfClosed conn
    //    use data = new SystemSettingTableData(conn)
    //    use reader = new DynamicSqlDataReader(data.Execute())
    //    [ while reader.Read() do yield (reader?City, reader?CityOriginalName) |> City.tryCreate ]



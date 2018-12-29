namespace ContGen

open System.Data
open System.Data.SqlClient
open FSharp.Data
open Microsoft.SqlServer.Types

open Configuration
open DynamicSql

module DatabaseTypes = 

    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>


    type SystemSettingTable = ClmDB.dbo.Tables.SystemSetting
    type SystemSettingTableRow = SystemSettingTable.Row
    type SystemSettingTableData = SqlCommandProvider<"select * from dbo.SystemSetting", ClmConnectionString, ResultType.DataReader>


    let openConnIfClosed (conn : SqlConnection) = 
        match conn.State with 
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let loadSettings (connStr : string) =
        use conn : SqlConnection = new SqlConnection (connStr)
        conn.Open()

        let systemSettingTable = new SystemSettingTable()
        (new SystemSettingTableData(conn)).Execute() |> systemSettingTable.Load

        systemSettingTable.Rows
        |> List.ofSeq
        |> List.groupBy (fun e -> e.SettingName)
        |> List.map (fun (e, v) -> e, v |> List.groupBy (fun r -> r.SettingOrderId) |> Map.ofList)
        |> Map.ofList


    //let loadSettings (conn : SqlConnection) = 
    //    openConnIfClosed conn
    //    use data = new SystemSettingTableData(conn)
    //    use reader = new DynamicSqlDataReader(data.Execute())
    //    [ while reader.Read() do yield (reader?City, reader?CityOriginalName) |> City.tryCreate ]



﻿namespace DbData

open System
open System.Data.SQLite
open ClmSys.VersionInfo
open ClmSys.GeneralPrimitives
open System.Data
open System.Data.SqlClient
open ClmSys.GeneralErrors
open ClmSys.Retry
open ClmSys.ClmErrors
open ClmSys.GeneralData
open ContGenServiceInfo.ServiceInfo
open MessagingServiceInfo.ServiceInfo


module Configuration =

    /// Note that the base name is used.
    /// If the base name is changed, then a new database must be generated!
    [<Literal>]
    let ClmDbName = ClmBaseName


    [<Literal>]
    let ContGenAppConfigFile : string = __SOURCE_DIRECTORY__ + "\..\ContGenService\App.config"


    [<Literal>]
    let MsgAppConfigFile : string = __SOURCE_DIRECTORY__ + "\..\MessagingService\App.config"


    [<Literal>]
    let ClmConnectionStringValue = "Server=localhost;Database=" + ClmDbName + ";Integrated Security=SSPI"


    let private getClmConnectionStringImpl() =
        ContGenAppSettings.SelectExecutableFile(getFileName contGenServiceProgramName)

        match ContGenAppSettings.ConnectionStrings.Clm with
        | EmptyString -> ClmConnectionStringValue
        | s -> s
        |> ConnectionString


    let private clmConnectionString = new Lazy<ConnectionString>(getClmConnectionStringImpl)
    let getClmConnectionString() = clmConnectionString.Value


    [<Literal>]
    let ClmCommandTimeout = 7200


    [<Literal>]
    let ClmSqlProviderName : string = "name=Clm"


    [<Literal>]
    let MsgSvcDbName = MsgSvcBaseName


    [<Literal>]
    let MsgSvcConnectionStringValue = "Server=localhost;Database=" + MsgSvcDbName + ";Integrated Security=SSPI"


    let private getMsgSvcConnectionStringImpl() =
        MsgAppSettings.SelectExecutableFile(getFileName messagingProgramName)

        match MsgAppSettings.ConnectionStrings.MsgSvc with
        | EmptyString -> MsgSvcConnectionStringValue
        | s -> s
        |> ConnectionString


    let private msgSvcConnectionString = new Lazy<ConnectionString>(getMsgSvcConnectionStringImpl)
    let getMsgSvcConnectionString() = msgSvcConnectionString.Value

    [<Literal>]
    let MsgSvcSqlProviderName : string = "name=MsgSvc"


    let buildConnectionString (key : string) : string =
        [
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key)
        ]
        |> List.pick (fun x -> x)


    let openConnIfClosed (conn : SqlConnection) =
        match conn.State with
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let getOpenConn (c : unit -> ConnectionString) =
        let conn = new SqlConnection(c().value)
        openConnIfClosed conn
        conn


    let getOpenSqliteConn (SqliteConnectionString connectionString) =
        let conn = new SQLiteConnection(connectionString)
        conn.Open()
        conn


    let toError g f = f |> g |> DbErr |> Error
    let addError g f e = ((f |> g |> DbErr) + e) |> Error
    let mapException e = e |> DbExn |> DbErr
    let mapExceptionToError e = e |> DbExn |> DbErr |> Error


    /// Maps missing value (None) to DbErr.
    let mapDbError f i v =
        v
        |> Option.map Ok
        |> Option.defaultValue (i |> f |> DbErr |> Error)


    let tryDbFun g =
        let w() =
            try
                g()
            with
            | e -> mapExceptionToError e

        tryRopFun mapException w

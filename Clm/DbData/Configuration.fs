namespace DbData
open ClmSys.VersionInfo
open ClmSys.GeneralPrimitives
open System.Data
open System.Data.SqlClient
open ClmSys.GeneralErrors
open ClmSys.Retry
open ClmSys.ClmErrors

module Configuration =

    /// Note that the base name is used.
    /// If the base name is changed, then a new database must be generated!
    [<Literal>]
    let ClmDbName = ClmBaseName


    [<Literal>]
    let AppConfigFile : string = __SOURCE_DIRECTORY__ + "\.\App.config"


    [<Literal>]
    let ClmConnectionStringValue = "Server=localhost;Database=" + ClmDbName + ";Integrated Security=SSPI"


    let clmConnectionString = ConnectionString ClmConnectionStringValue

    [<Literal>]
    let ClmCommandTimeout = 7200


    [<Literal>]
    let ClmSqlProviderName : string = "name=Clm"


    [<Literal>]
    let MsgSvcDbName = MsgSvcBaseName


    [<Literal>]
    let MsgSvcConnectionStringValue = "Server=localhost;Database=" + MsgSvcDbName + ";Integrated Security=SSPI"


    let msgSvcConnectionString = ConnectionString MsgSvcConnectionStringValue

    [<Literal>]
    let MsgSvcSqlProviderName : string = "name=MsgSvc"


    /// Number of minutes for worker node errors to expire before the node can be again included in work distribution.
    [<Literal>]
    let lastAllowedNodeErrInMinutes = "60"


    let buildConnectionString (key : string) : string =
        [
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key)
        ]
        |> List.pick (fun x -> x)


    let openConnIfClosed (conn : SqlConnection) =
        match conn.State with
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ignore ()


    let getOpenConn (ConnectionString connectionString) =
        let conn = new SqlConnection(connectionString)
        openConnIfClosed conn
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

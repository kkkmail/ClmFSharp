namespace DbData
open ClmSys.VersionInfo
open ClmSys.GeneralData

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
    let ClmSqlProviderName : string = "name=" + "Clm"


    let buildConnectionString (key : string) : string =
        [ 
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key) 
        ]
        |> List.pick (fun x -> x)

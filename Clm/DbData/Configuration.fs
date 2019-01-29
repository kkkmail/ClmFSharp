namespace DbData

module Configuration =
    open ClmSys.GeneralData

    [<Literal>]
    let ClmDbName : string = "Clm"

    [<Literal>]
    let AppConfigFile : string = __SOURCE_DIRECTORY__ + "\.\App.config"

    [<Literal>]
    let ClmConnectionStringValue = "Server=localhost;Database=" + ClmDbName + ";Integrated Security=SSPI"

    let clmConnectionString = ConnectionString ClmConnectionStringValue

    [<Literal>]
    let ClmCommandTimeout = 7200

    [<Literal>]
    let ClmSqlProviderName : string = "name=" + ClmDbName


    let buildConnectionString (key : string) : string = 
        [ 
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key) 
        ]
        |> List.pick (fun x -> x)

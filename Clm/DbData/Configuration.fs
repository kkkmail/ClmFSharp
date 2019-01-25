namespace DbData

module Configuration =

    [<Literal>]
    let ClmDbName : string = "Clm"

    [<Literal>]
    let AppConfigFile : string = __SOURCE_DIRECTORY__ + "\.\App.config"

    [<Literal>]
    let ClmConnectionString : string = "Server=localhost;Database=" + ClmDbName + ";Integrated Security=SSPI"

    [<Literal>]
    let ClmCommandTimeout = 3600

    [<Literal>]
    let ClmSqlProviderName : string = "name=" + ClmDbName


    let buildConnectionString (key : string) : string = 
        [ 
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key) 
        ]
        |> List.pick (fun x -> x)


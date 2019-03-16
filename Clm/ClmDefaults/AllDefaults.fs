namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [
            Defaults_000_000.defaultValue
            Defaults_000_001.defaultValue
            Defaults_000_002.defaultValue
            Defaults_000_003.defaultValue
            Defaults_000_004.defaultValue
            Defaults_000_005.defaultValue
        ]
        |> List.map (fun e -> e.defaultId, e)
        |> Map.ofList


    let getDefaultValues i = defaultValues.[i], i

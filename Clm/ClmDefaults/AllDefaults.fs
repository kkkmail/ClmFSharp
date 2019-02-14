namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [|
            Defaults_000.defaultValue
            Defaults_001.defaultValue
            Defaults_002.defaultValue
            Defaults_003.defaultValue
            Defaults_004.defaultValue
        |]


    let getDefaultValues i = defaultValues.[i], i

namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [|
            Defaults_000.defaultValue
            Defaults_001.defaultValue
            Defaults_002.defaultValue
            Defaults_003.defaultValue
            Defaults_004.defaultValue
            Defaults_005.defaultValue
            Defaults_006.defaultValue
            Defaults_007.defaultValue
            Defaults_008.defaultValue
            Defaults_009.defaultValue
            Defaults_010.defaultValue
        |]


    let getDefaultValues i = defaultValues.[i], i

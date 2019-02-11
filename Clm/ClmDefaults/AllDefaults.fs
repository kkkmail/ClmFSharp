namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [|
            Defaults_000.defaultValue
        |]


    let getDefaultValues i = defaultValues.[i], i

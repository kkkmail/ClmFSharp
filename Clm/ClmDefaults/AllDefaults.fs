namespace ClmDefaults
open System

module AllDefaults =

    let defaultValues=
        [|
            Defaults_000.defaultValue
            Defaults_001.defaultValue
        |]


    let getDefaultValues i = defaultValues.[i], i

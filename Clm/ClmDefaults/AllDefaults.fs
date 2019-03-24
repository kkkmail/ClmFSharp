﻿namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [
            Defaults_000_000.defaultValue
            Defaults_000_001.defaultValue
            Defaults_000_002.defaultValue
            Defaults_000_003.defaultValue
            Defaults_000_004.defaultValue
            Defaults_000_005.defaultValue

            Defaults_001_000.defaultValue
            Defaults_001_001.defaultValue
            Defaults_001_002.defaultValue
            Defaults_001_003.defaultValue
        ]
        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i

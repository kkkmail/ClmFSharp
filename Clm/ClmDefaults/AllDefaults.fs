namespace ClmDefaults

module AllDefaults =

    let defaultValues=
        [
            // Catalytic destruction for n = 20
            Defaults_000_000.defaultValue
            Defaults_000_001.defaultValue
            Defaults_000_002.defaultValue
            Defaults_000_003.defaultValue
            Defaults_000_004.defaultValue
            Defaults_000_005.defaultValue

            // Catalytic destruction + sedimentation direct for n = 20
            Defaults_001_000.defaultValue
            Defaults_001_001.defaultValue
            Defaults_001_002.defaultValue
            Defaults_001_003.defaultValue

            // Catalytic destruction for n = 25
            Defaults_002_000.defaultValue

            // Tests
            Defaults_999_000.defaultValue
        ]
        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i

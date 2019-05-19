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

            Defaults_000_006.defaultValue
            Defaults_000_007.defaultValue
            Defaults_000_008.defaultValue
            Defaults_000_009.defaultValue
            Defaults_000_010.defaultValue

            Defaults_000_011.defaultValue
            Defaults_000_012.defaultValue
            Defaults_000_013.defaultValue
            Defaults_000_014.defaultValue
            Defaults_000_015.defaultValue

            Defaults_000_016.defaultValue
            Defaults_000_017.defaultValue
            Defaults_000_018.defaultValue
            Defaults_000_019.defaultValue
            Defaults_000_020.defaultValue


            // Catalytic destruction + sedimentation direct for n = 20
            Defaults_001_000.defaultValue
            Defaults_001_001.defaultValue
            Defaults_001_002.defaultValue
            Defaults_001_003.defaultValue


            // Catalytic destruction for n = 25
            Defaults_002_000.defaultValue
            Defaults_002_001.defaultValue
            Defaults_002_002.defaultValue
            Defaults_002_003.defaultValue
            Defaults_002_004.defaultValue
            Defaults_002_005.defaultValue
            Defaults_002_006.defaultValue
            Defaults_002_007.defaultValue
            Defaults_002_008.defaultValue
            Defaults_002_009.defaultValue
            Defaults_002_010.defaultValue
            Defaults_002_011.defaultValue
            Defaults_002_012.defaultValue
            Defaults_002_013.defaultValue
            Defaults_002_014.defaultValue


            // Catalytic destruction + catalytic synthesis for n = 20
            Defaults_003_000.defaultValue
            

            // Tests
            Defaults_999_000.defaultValue
        ]
        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i

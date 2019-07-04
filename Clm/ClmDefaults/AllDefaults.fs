namespace ClmDefaults
open Clm.ModelParams

module AllDefaults =

    let updateDescription d (lst : List<ClmDefaultValue>) = lst |> List.map (fun e -> { e with description = Some d })

    let defaultValues =
        updateDescription "Catalytic destruction for n = 20."
            [
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
            ]
        @
        updateDescription "Catalytic destruction + sedimentation direct for n = 20."
            [
                Defaults_001_000.defaultValue
                Defaults_001_001.defaultValue
                Defaults_001_002.defaultValue
                Defaults_001_003.defaultValue
            ]
        @
        updateDescription "Catalytic destruction for n = 25."
            [
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
            ]
        @
        updateDescription "Catalytic synthesis + catalytic destruction for n = 20."
            [
                Defaults_003_000.defaultValue
            ]
        @
        updateDescription "Catalytic synthesis + destruction for n = 20."
            [
                Defaults_004_000.defaultValue
                Defaults_004_001.defaultValue
                Defaults_004_002.defaultValue
                Defaults_004_003.defaultValue
                Defaults_004_004.defaultValue
                Defaults_004_005.defaultValue
                Defaults_004_006.defaultValue
                Defaults_004_007.defaultValue
                Defaults_004_008.defaultValue
                Defaults_004_009.defaultValue
                Defaults_004_010.defaultValue
                Defaults_004_011.defaultValue
                Defaults_004_012.defaultValue
                Defaults_004_013.defaultValue
                Defaults_004_014.defaultValue
                Defaults_004_015.defaultValue
                Defaults_004_016.defaultValue
                Defaults_004_017.defaultValue
                Defaults_004_018.defaultValue
                Defaults_004_019.defaultValue
                Defaults_004_020.defaultValue
            ]
        @
        updateDescription "Catalytic synthesis for n = 20."
            [
                Defaults_005_000.defaultValue
            ]
        @
        updateDescription "Catalytic synthesis + sedimentation direct for n = 11."
            [
                Defaults_006_000.defaultValue
                Defaults_006_001.defaultValue
                Defaults_006_002.defaultValue
                Defaults_006_003.defaultValue
                Defaults_006_004.defaultValue
                Defaults_006_005.defaultValue
                Defaults_006_006.defaultValue
            ]
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (sim param 0.20)."
            [
                Defaults_007_000.defaultValue
                Defaults_007_001.defaultValue
                Defaults_007_002.defaultValue
                Defaults_007_003.defaultValue

            ]
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20."
            [
                Defaults_008_000.defaultValue
                Defaults_008_001.defaultValue
                Defaults_008_002.defaultValue
                Defaults_008_003.defaultValue
            ]
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param 0.20)."
            [
                Defaults_009_000.defaultValue
                Defaults_009_001.defaultValue
                Defaults_009_002.defaultValue
                Defaults_009_003.defaultValue
                Defaults_009_004.defaultValue
                Defaults_009_005.defaultValue
                Defaults_009_006.defaultValue
                Defaults_009_007.defaultValue
                Defaults_009_008.defaultValue
            ]
        @
        updateDescription "Catalytic synthesis / forward only only for n = 20 (sim param 0.20)."
            [
                Defaults_010_000.defaultValue
                Defaults_010_001.defaultValue
            ]
        @
        updateDescription "Tests."
            [
                Defaults_999_000.defaultValue
            ]
        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i

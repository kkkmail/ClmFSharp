namespace ClmDefaults
open Clm.ModelParams

module AllDefaults =

    let updateDescription d (lst : List<ClmDefaultValue>) = lst |> List.map (fun e -> { e with description = Some d })

    let defaultValues =
        updateDescription "Catalytic destruction for n = 20."
            [
                Defaults_000_000.defaultValue
            ]
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrSim = 0.20, vary catDestrScarcity)."
            (Defaults_000_007.nd |> List.map Defaults_000_007.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param)."
            (Defaults_000_009.nsd |> List.map Defaults_000_009.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthSim = 0.20, vary catSynthScarcity)."
            (Defaults_000_010.ns |> List.map Defaults_000_010.getGefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, vary catDestrSim)."
            (Defaults_000_011.nd |> List.map Defaults_000_011.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, vary catSynthSim)."
            (Defaults_000_012.ns |> List.map Defaults_000_012.getGefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, catDestrSim = 0.20, vary wasteRecyclingRate)."
            (Defaults_000_013.nw |> List.map Defaults_000_013.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, catSynthSim = 0.20, vary wasteRecyclingRate)."
            (Defaults_000_014.nw |> List.map Defaults_000_014.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param), wasteRecyclingRate upped to 10.0."
            (Defaults_000_015.nsd |> List.map Defaults_000_015.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param)."
            (Defaults_000_016.nsd |> List.map Defaults_000_016.getGefaultValue)


        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrSim = 0.20, vary catDestrScarcity) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_007.nd |> List.map Defaults_001_000_007.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_009.nsd |> List.map Defaults_001_000_009.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthSim = 0.20, vary catSynthScarcity) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_010.ns |> List.map Defaults_001_000_010.getGefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, vary catDestrSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_011.nd |> List.map Defaults_001_000_011.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_012.ns |> List.map Defaults_001_000_012.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_016.nsd |> List.map Defaults_001_000_016.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 20, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_017.nsd |> List.map Defaults_001_000_017.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_018.nsd |> List.map Defaults_001_000_018.getGefaultValue)


        @
        updateDescription "Tests."
            [
                Defaults_999_999.defaultValue
            ]
        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i

namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_002 =
    let defaultValues =
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 1.0."
            (Defaults_002_000_016.nsd |> List.map Defaults_002_000_016.getGefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 1.0."
            (Defaults_002_000_018.nsd |> List.map Defaults_002_000_018.getGefaultValue)

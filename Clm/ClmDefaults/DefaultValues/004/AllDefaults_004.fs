namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_004 =
    let defaultValues =
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.1, vary cat ligation."
            Defaults_004_000_000.defaultValues
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 5, m = 5 (vary both scarcity param and both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.1."
            Defaults_004_001_000.defaultValues
        @
        updateDescription "Cat lig with similarity playground."
            Defaults_004_002_000.defaultValues

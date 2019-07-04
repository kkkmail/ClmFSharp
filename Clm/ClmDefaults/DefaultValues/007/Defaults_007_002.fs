namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_007_002 =

    let clmDefaultValueId = 007_002L |> ClmDefaultValueId
    let description = None
    let catRateGenType = ByEnantiomerPairs

    let defaultRateParams =
        //===========================================================
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
        let catDestrRndParam = (destrParam, (Some 0.000_005), 100_000.0)
        let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20) catRateGenType
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        //===========================================================
        let rates =
            [
                wasteRecyclingParam

                synthParam |> SynthesisRateParam

                destrParam |> DestructionRateParam
                catDestrParam

                ligParam |> LigationRateParam
            ]
        //===========================================================

        {
            rateParams = rates
        }


    let defaultValue =
        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

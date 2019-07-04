namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_009_002 =

    let clmDefaultValueId = 009_002L |> ClmDefaultValueId
    let description = None
    let catRateGenType = ByEnantiomerPairs

    let defaultRateParams =
        //===========================================================
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
        let catSynthRndParam = (synthParam, (Some 0.000_010), 100_000.0)
        let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.20) catRateGenType
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
        let catDestrRndParam = (destrParam, (Some 0.000_020), 100_000.0)
        let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20) catRateGenType
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        //===========================================================
        let rates =
            [
                wasteRecyclingParam

                synthParam |> SynthesisRateParam
                catSynthParam

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

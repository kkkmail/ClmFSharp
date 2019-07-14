namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_010_004 =

    let clmDefaultValueId = 010_004L |> ClmDefaultValueId
    let description = None
    let catRateGenType = ByEnantiomerPairs

    let defaultRateParams =
        //===========================================================
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
        let catSynthRndParam = (synthParam, (Some 0.000_002), 100_000.0)
        let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.20) catRateGenType
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        //===========================================================
        let rates =
            [
                wasteRecyclingParam

                synthParam |> SynthesisRateParam
                catSynthParam

                destrParam |> DestructionRateParam

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

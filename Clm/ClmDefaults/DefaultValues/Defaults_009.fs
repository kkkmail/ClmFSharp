namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_009 =

    let nsd =
        [
            ( 0L,  10,  10)
            ( 1L,  10,   5)
            ( 2L,  10,  20)
            ( 3L,   5,  10)
            ( 4L,   5,   5)
            ( 5L,   5,  20)
            ( 6L,  20,  10)
            ( 7L,  20,   5)
            ( 8L,  20,  20)

            (10L,  10,  50)
            (13L,   5,  50)
            (16L,  20,  50)
            (18L,   2,   2)
            (25L,  50,   5)
            (26L,  50,  10)
            (27L,  50,  20)
            (28L,  50,  50)
            (35L, 100, 100)
        ]


    let getGefaultValue (n, s, d) =
        let clmDefaultValueId = (9_000L + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, (Some ((double s) / 1_000_000.0)), 100_000.0)
            let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.20) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
            let catDestrRndParam = (destrParam, (Some ((double d) / 1_000_000.0)), 100_000.0)
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

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

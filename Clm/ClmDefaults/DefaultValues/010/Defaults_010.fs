namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_010 =

    let ns =
        [
            (010_000L,  10)
            (010_001L,   5)
            (010_002L,  20)
            (010_003L,  50)
            (010_004L,   2)
            (010_005L, 100)
        ]


    let getGefaultValue (n, s) =
        let clmDefaultValueId = n |> ClmDefaultValueId
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

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

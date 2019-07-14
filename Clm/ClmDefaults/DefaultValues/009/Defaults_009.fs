namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_009 =

    let nsd =
        [
            //(009_000L, 10, 10)
            //(009_001L, 10,  5)
            //(009_002L, 10, 20)
            //(009_003L,  5, 10)
            //(009_004L,  5,  5)
            //(009_005L,  5, 20)
            //(009_006L, 20, 10)
            //(009_007L, 20,  5)
            //(009_008L, 20, 20)

            (009_010L, 10, 50)
            (009_013L,  5, 50)
            (009_016L, 20, 50)
            (009_025L, 50,  5)
            (009_026L, 50, 10)
            (009_027L, 50, 20)
            (009_028L, 50, 50)
        ]


    let getGefaultValue (n, s, d) =
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

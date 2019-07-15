﻿namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_013 =

    let nw =
        [
            ( 0L,  0.1)
            ( 1L,  1.0)
            ( 2L, 10.0)
        ]


    let getGefaultValue (n, w) =
        let clmDefaultValueId = (13_000L + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam w
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
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
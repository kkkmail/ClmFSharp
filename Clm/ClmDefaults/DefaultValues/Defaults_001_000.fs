﻿namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_001_000 =

    let defaultSetIndex = 001_000

    let defaultRateParams =
        //===========================================================
        let foodParam = ReactionRateProviderParams.defaultFoodCreationParam 0.01
        let wasteParam = ReactionRateProviderParams.defaultWasteRemovalParam 10.0
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (0.001, 0.001)
        let catSynthRndParam = (synthParam, (Some 0.002), 10_000.0)
        //let catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParam catSynthRndParam
        let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.3)
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (0.001, 0.001)
        let catDestrRndParam = (destrParam, (Some 0.000_010), 100_000.0)
        //let catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParam catDestrRndParam
        let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20)
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        let catLigParam = ReactionRateProviderParams.defaultCatLigRndParam (ligParam, (Some 0.000_05), 2_000.0)
        //===========================================================
        //let sedDirParam = ReactionRateProviderParams.defaultSedDirSimParam (Some 0.00001, 10_000.0) (Some 0.20)
        let sedDirParam = ReactionRateProviderParams.defaultSedDirSimParam (Some 0.001, 10_000.0) (Some 0.20)
        let sedAllParam = ReactionRateProviderParams.defaultSedAllRndParam 0.1
        //===========================================================
        let racemParam = ReactionRateProviderParams.defaultRacemRndParamImpl 0.001
        let catRacemRndParam = (racemParam, (Some 0.000_5), 1_000.0)
        //let catRacemParam = ReactionRateProviderParams.defaultCatRacemRndParam catRacemRndParam
        let catRacemParam = ReactionRateProviderParams.defaultCatRacemSimParam catRacemRndParam (Some 0.2)
        //===========================================================
        let rates =
            [
                //foodParam
                //wasteParam
                wasteRecyclingParam

                synthParam |> SynthesisRateParam
                //catSynthParam

                destrParam |> DestructionRateParam
                catDestrParam

                ligParam |> LigationRateParam
                //catLigParam

                sedDirParam
                //sedAllParam

                //racemParam |> RacemizationRateParam
                //catRacemParam
            ]
        //===========================================================

        {
            rateParams = rates
        }


    let defaultValue =
        {
            defaultSetIndex = defaultSetIndex

            modelCommandLineParams =
                [
                    {
                        tEnd = 250_000.0m
                        y0 = 10.0m
                        useAbundant = false
                    }

                    {
                        tEnd = 250_000.0m
                        y0 = 5.0m
                        useAbundant = false
                    }

                    {
                        tEnd = 250_000.0m
                        y0 = 20.0m
                        useAbundant = false
                    }
                ]

            defaultRateParams = defaultRateParams
            description = None
        }

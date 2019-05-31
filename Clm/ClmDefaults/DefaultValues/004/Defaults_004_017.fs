namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_004_017 =

    let clmDefaultValueId = 004_017L |> ClmDefaultValueId
    let description = None

    let defaultRateParams =
        //===========================================================
        let foodParam = ReactionRateProviderParams.defaultFoodCreationParam 0.01
        let wasteParam = ReactionRateProviderParams.defaultWasteRemovalParam 10.0
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (0.001, 0.001)
        let catSynthRndParam = (synthParam, (Some 0.000_010), 100_000.0)
        //let catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParam catSynthRndParam
        let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.80)
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (0.001, 0.001)
        let catDestrRndParam = (destrParam, (Some 0.000_010), 100_000.0)
        //let catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParam catDestrRndParam
        let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20)
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        let catLigParam = ReactionRateProviderParams.defaultCatLigRndParam (ligParam, (Some 0.000_05), 2_000.0)
        //===========================================================
        let sedDirParam = ReactionRateProviderParams.defaultSedDirRndParam (Some 0.00001, 10_000.0)
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
                catSynthParam

                destrParam |> DestructionRateParam
                //catDestrParam

                ligParam |> LigationRateParam
                //catLigParam

                //sedDirParam
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
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

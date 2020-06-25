namespace ClmDefaults

open Clm.ReactionRateParams
open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_004_000 =
    let sdSim = 0.1

    type DefaultDataParam =
        {
            enCatSynthSimilarity : double
            enCatSynthScarcity : double
            enCatSynthMultiplier : double

            ligForward : float
            ligBackward : float

            enCatLigSimilarity : float
            enCatLigScarcity : float
            enCatLigMultiplier : float

            sugarForward : float
            sugarBackward : float
            sugarScarcity : float
        }

        static member defaultValue =
            {
                enCatSynthScarcity = 0.000_100
                enCatSynthMultiplier = 100_000.0
                enCatSynthSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.003

                enCatLigScarcity = 0.000_000_001
                enCatLigMultiplier = 100_000.0
                enCatLigSimilarity = 0.0010

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


    let data =
            [
                DefaultDataParam.defaultValue
                { DefaultDataParam.defaultValue with sugarForward = 10.0 }
                { DefaultDataParam.defaultValue with enCatLigScarcity = 0.000_000_002 }
                { DefaultDataParam.defaultValue with sugarForward = 10.0; enCatLigScarcity = 0.000_000_002 }
            ]
            |> withRowNumber


//    /// Max 19 rows.
//    let nSim =
//        [
//            0.0000M
//            0.0010M
//            0.0020M
//            0.0030M
//            0.0050M
//            0.0070M
//            0.0100M
//            0.0130M
//            0.0160M
//            0.0200M
//        ]
//        |> withRowNumber
//
//
//    /// Max 49 rows.
//    let mScMult =
//        [
//            (0.0M,                 0.0M)
////            (0.000_000_001M, 100_000.0M)
////            (0.000_000_002M, 100_000.0M)
////            (0.000_000_005M, 100_000.0M)
////            (0.000_000_010M, 100_000.0M)
////            (0.000_000_020M, 100_000.0M)
////            (0.000_000_050M, 100_000.0M)
////            (0.000_000_100M, 100_000.0M)
//        ]
//        |> withRowNumber

    let getDefaultValue (n, e) =
        let clmDefaultValueId = (4_004_000_000L + n) |> ClmDefaultValueId
        printfn "clmDefaultValueId = %A, e = %A" clmDefaultValueId e

        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, Some 0.000_000_001)

            let enCatSynthRndParam = (synthParam, (Some e.enCatSynthScarcity), e.enCatSynthMultiplier)

            let enCatSynthParam =
                ReactionRateProviderParams.defaultEnCatSynthSimParam enCatSynthRndParam (Some e.enCatSynthSimilarity) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)

            let catDestrRndParam = (destrParam, (Some 0.000_100), 100_000.0)

            let catDestrParam =
                ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some sdSim) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (e.ligForward, e.ligBackward)

            let enCatLigParam =
                ReactionRateProviderParams.defaultEnCatLigSimParam (ligParam, Some (e.enCatLigScarcity), (e.enCatLigMultiplier)) (Some e.enCatLigSimilarity) catRateGenType
            //===========================================================
            let sugParam = ReactionRateProviderParams.defaultSugarSynthRndParamImpl ((Some e.sugarForward, Some e.sugarBackward), Some e.sugarScarcity)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam

                    synthParam |> SynthesisRateParam
                    enCatSynthParam

                    destrParam |> DestructionRateParam
                    catDestrParam

                    ligParam |> LigationRateParam
                    if (e.enCatLigScarcity > 0.0) then enCatLigParam

                    sugParam |> SugarSynthesisRateParam
                ]
            //===========================================================

            {
                rateParams = rates
                successNumberType = successNumberType
            }

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

    let defaultValues =
        printfn "\n"
        
        data
        |> List.map getDefaultValue
        |> updateDescription "Cat lig with similarity + all sugars playground."

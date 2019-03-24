namespace ClmDefaults

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.ModelParams

module DefaultValuesExt =

    let defaultRateMultiplierDistr threshold mult =
        Distribution.createTriangular { threshold = threshold; scale = Some mult; shift = None } |> RateMultDistr


    let defaultEeDistribution = EeDistribution.createBiDelta (Some 0.95)
    let defaultEeDistributionGetter = DeltaEeDistributionGetter
    let deltaRateMultDistrGetter = DeltaRateMultDistrGetter


    type ReactionRateProviderParams
        with

        static member defaultFoodCreationParam forward =
            {
                foodCreationRate = forward
            }
            |> FoodCreationRateParam

        static member defaultWasteRemovalParam forward =
            {
                wasteRemovalRate = forward
            }
            |> WasteRemovalRateParam

        static member defaultWasteRecyclingParam forward =
            {
                wasteRecyclingRate = forward
            }
            |> WasteRecyclingRateParam

        static member defaultSynthRndParamImpl (forward, backward) =
            {
                synthesisDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SynthRndParam

        static member defaultSynthRndParam (forward, backward) =
            ReactionRateProviderParams.defaultSynthRndParamImpl (forward, backward)
            |> SynthesisRateParam

        static member defaultDestrRndParamImpl (forward, backward) =
            {
                destructionDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> DestrRndParam

        static member defaultDestrRndParam (forward, backward) =
            ReactionRateProviderParams.defaultDestrRndParamImpl (forward, backward)
            |> DestructionRateParam

        static member defaultCatSynthRndParamImpl (m, threshold, mult) =
            {
                synthesisParam = m

                catSynthRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatSynthRndParam (m, threshold, mult) = 
            ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult)
            |> CatSynthRndParam
            |> CatalyticSynthesisRateParam

        static member defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold =
            {
                catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult)

                catSynthSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatSynthSimParam (m, threshold, mult) simThreshold =
            ReactionRateProviderParams.defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold
            |> CatSynthSimParam
            |> CatalyticSynthesisRateParam

        static member defaultCatDestrRndParamImpl (m, threshold, mult) =
            {
                destructionParam = m

                catDestrRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatDestrRndParam (m, threshold, mult) =
            ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult)
            |> CatDestrRndParam
            |> CatalyticDestructionRateParam

        static member defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold =
            {
                catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult)

                catDestrSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatDestrSimParam (m, threshold, mult) simThreshold =
            ReactionRateProviderParams.defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold
            |> CatDestrSimParam
            |> CatalyticDestructionRateParam

        static member defaultLigRndParamImpl (forward, backward) =
            {
                ligationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigRndParam

        static member defaultLigRndParam (forward, backward) =
            ReactionRateProviderParams.defaultLigRndParamImpl (forward, backward)
            |> LigationRateParam

        static member defaultCatLigRndParamImpl (m, threshold, mult) =
            {
                ligationParam = m

                catLigRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatLigRndParam (m, threshold, mult) =
            ReactionRateProviderParams.defaultCatLigRndParamImpl (m, threshold, mult)
            |> CatLigRndParam
            |> CatalyticLigationRateParam

        static member defaultSedDirRndParamImpl (threshold, mult) =
            {
                sedDirRatesEeParam =
                    {
                        sedDirRateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                    }
                sedDirDistribution = Distribution.createTriangular { threshold = threshold; scale = None; shift = None }
                forwardScale = Some mult
            }

        static member defaultSedDirRndParam (threshold, mult) =
            ReactionRateProviderParams.defaultSedDirRndParamImpl (threshold, mult)
            |> SedDirRndParam
            |> SedimentationDirectRateParam

        static member defaultSedDirSimParamImpl (threshold, mult) simThreshold =
            {
                sedDirParam = ReactionRateProviderParams.defaultSedDirRndParamImpl (threshold, mult)
                sedDirSimParam =
                    {
                        sedDirSimBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getForwardEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultSedDirSimParam (threshold, mult) simThreshold =
            ReactionRateProviderParams.defaultSedDirSimParamImpl (threshold, mult) simThreshold
            |> SedDirSimParam
            |> SedimentationDirectRateParam

        static member defaultSedAllRndParamImpl mult =
            {
                sedimentationAllDistribution = Distribution.createTriangular { threshold = None; scale = None; shift = None }
                forwardScale = Some mult
            }

        static member defaultSedAllRndParam mult =
            ReactionRateProviderParams.defaultSedAllRndParamImpl mult
            |> SedAllRndParam
            |> SedimentationAllRateParam

        static member defaultRacemRndParamImpl forward =
            {
                racemizationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
            }
            |> RacemRndParam

        static member defaultRacemRndParam forward =
            ReactionRateProviderParams.defaultRacemRndParamImpl forward
            |> RacemizationRateParam

        static member defaultCatRacemRndParamImpl (m, threshold, mult) =
            {
                racemizationParam = m
                catRacemRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatRacemRndParam (m, threshold, mult) =
            ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult)
            |> CatRacemRndParam
            |> CatalyticRacemizationRateParam

        static member defaultCatRacemSimParam (m, threshold, mult) simThreshold =
            {
                catRacemParam = ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult)

                catRacemSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }
            |> CatRacemSimParam
            |> CatalyticRacemizationRateParam

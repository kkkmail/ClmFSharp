namespace ClmDefaults

open Clm.Distributions
open Clm.ReactionRates

module DefaultValuesExt =

    let defaultRateMultiplierDistr threshold mult =
        Distribution.createTriangular { threshold = threshold; scale = Some mult; shift = None } |> RateMultDistr


    let defaultEeDistribution = EeDistribution.createBiDelta (Some 0.95)
    let defaultEeDistributionGetter = DeltaEeDistributionGetter
    let deltaRateMultDistrGetter = DeltaRateMultDistrGetter


    let defaultEeBackwardDistribution catRateGenType =
        match catRateGenType with
        | ByIndividualCatalyst -> defaultEeDistribution |> Some
        | ByEnantiomerPairs -> None


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

        static member defaultCatSynthRndParamImpl (m, threshold, mult) catRateGenType =
            {
                synthesisParam = m

                catSynthRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeBackwardDistribution catRateGenType
                    }
            }

        static member defaultCatSynthRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult) catRateGenType
            |> CatSynthRndParam
            |> CatalyticSynthesisRateParam

        static member defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult) catRateGenType

                catSynthSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatSynthSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> CatSynthSimParam
            |> CatalyticSynthesisRateParam

        static member defaultCatDestrRndParamImpl (m, threshold, mult) catRateGenType =
            {
                destructionParam = m

                catDestrRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeBackwardDistribution catRateGenType
                    }
            }

        static member defaultCatDestrRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult) catRateGenType
            |> CatDestrRndParam
            |> CatalyticDestructionRateParam

        static member defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult) catRateGenType

                catDestrSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatDestrSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType
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

        static member defaultCatLigRndParamImpl (m, threshold, mult) catRateGenType =
            {
                ligationParam = m

                catLigRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeBackwardDistribution catRateGenType
                    }
            }

        static member defaultCatLigRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatLigRndParamImpl (m, threshold, mult) catRateGenType
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

        static member defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType =
            {
                racemizationParam = m
                catRacemRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeForwardDistribution = defaultEeDistribution |> Some
                        eeBackwardDistribution = defaultEeBackwardDistribution catRateGenType
                    }
            }

        static member defaultCatRacemRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType
            |> CatRacemRndParam
            |> CatalyticRacemizationRateParam

        static member defaultCatRacemSimParam (m, threshold, mult) simThreshold catRateGenType =
            {
                catRacemParam = ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType

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

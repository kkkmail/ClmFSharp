namespace ClmDefaults

open System
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.ModelParams


module DefaultValuesExt =

    type ClmDefaultValue =
        {
            modelCommandLineParams : list<ModelCommandLineParam>
            getDefaultRateModels : NumberOfAminoAcids -> ReactionRateProviderParams
            description : string option
        }


    type NumberOfAminoAcids
        with
        static member defaultValue = EightAminoAcids


    type MaxPeptideLength
        with
        static member defaultValue = ThreeMax


    let defaultRateMultiplierDistr threshold mult =
        Distribution.createTriangular { threshold = threshold; scale = Some mult; shift = None } |> RateMultDistr

    let defaultEeDistribution = EeDistribution.createBiDelta (Some 0.95)
    let defaultEeDistributionGetter = DeltaEeDistributionGetter
    let deltaRateMultDistrGetter = DeltaRateMultDistrGetter


    type ReactionRateProvider
        with

        static member defaultFoodCreationModel forward =
            {
                foodCreationRate = forward
            }
            |> FoodCreationModel

        static member defaultWasteRemovalModel forward =
            {
                wasteRemovalRate = forward
            }
            |> WasteRemovalModel

        static member defaultWasteRecyclingModel forward =
            {
                wasteRecyclingRate = forward
            }
            |> WasteRecyclingModel

        static member defaultSynthRndModel (forward, backward) =
            {
                synthesisDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SynthRndParam
            |> SynthesisModel.create

        static member defaultDestrRndModel (forward, backward) =
            {
                destructionDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> DestrRndParam
            |> DestructionModel.create

        static member defaultCatSynthRndParams (m, threshold, mult) =
            {
                catSynthRndParam = 
                    {
                        catSynthRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                                eeForwardDistribution = defaultEeDistribution |> Some
                                eeBackwardDistribution = defaultEeDistribution |> Some
                            }
                    }
                synthesisModel = m
            }

        static member defaultCatSynthRndModel (m, threshold, mult) = 
            ReactionRateProvider.defaultCatSynthRndParams (m, threshold, mult)
            |> CatSynthRndParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultCatSynthSimModel (m, threshold, mult) (simThreshold, n) =
            {
                catSynthSimParam = 
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catSynthModel = ReactionRateProvider.defaultCatSynthRndParams (m, threshold, mult) |> CatalyticSynthesisRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatSynthSimParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultCatDestrRndParams (m, threshold, mult) =
            {
                catDestrRndParam = 
                    {
                        catDestrRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                                eeForwardDistribution = defaultEeDistribution |> Some
                                eeBackwardDistribution = defaultEeDistribution |> Some
                            }

                    }
                destructionModel = m
            }

        static member defaultCatDestrRndModel (m, threshold, mult) =
            ReactionRateProvider.defaultCatDestrRndParams (m, threshold, mult)
            |> CatDestrRndParamWithModel
            |> CatalyticDestructionModel.create

        static member defaultCatDestrSimModel (m, threshold, mult) (simThreshold, n) =
            {
                catDestrSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catDestrModel = ReactionRateProvider.defaultCatDestrRndParams (m, threshold, mult) |> CatalyticDestructionRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatDestrSimParamWithModel
            |> CatalyticDestructionModel.create

        static member defaultLigRndModel (forward, backward) =
            {
                ligationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigRndParam
            |> LigationModel.create

        static member defaultCatLigRndModel (m, threshold, mult) =
            {
                catLigationParam =
                    {
                        catLigRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                                eeForwardDistribution = defaultEeDistribution |> Some
                                eeBackwardDistribution = defaultEeDistribution |> Some
                            }
                    }
                ligationModel = m
            }
            |> CatLigRndParamWithModel
            |> CatalyticLigationModel.create

        static member defaultSedDirRndModel (threshold, mult) =
            {
                sedimentationDirectDistribution = Distribution.createTriangular { threshold = Some threshold; scale = None; shift = None }
                forwardScale = Some mult
            }
            |> SedDirRndParam
            |> SedimentationDirectModel.create

        static member defaultSedAllRndModel mult =
            {
                sedimentationAllDistribution = Distribution.createTriangular { threshold = None; scale = None; shift = None }
                forwardScale = Some mult
            }
            |> SedAllRndParam
            |> SedimentationAllModel.create

        static member defaultRacemRndModel forward =
            {
                racemizationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
            }
            |> RacemRndParam
            |> RacemizationModel.create

        static member defaultCatRacemRndParams (m, threshold, mult) n =
            {
                catRacemRndParam =
                    {
                        catRacemRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                                eeForwardDistribution = defaultEeDistribution |> Some
                                eeBackwardDistribution = defaultEeDistribution |> Some
                            }

                    }
                racemizationModel = m
                aminoAcids = AminoAcid.getAminoAcids n
            }

        static member defaultCatRacemRndModel (m, threshold, mult) n =
            ReactionRateProvider.defaultCatRacemRndParams (m, threshold, mult) n
            |> CatRacemRndParamWithModel
            |> CatalyticRacemizationModel.create

        static member defaultCatRacemSimModel (m, threshold, mult) (simThreshold, n) =
            {
                catRacemSimParam =
                    {
                        simBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catRacemModel = ReactionRateProvider.defaultCatRacemRndParams (m, threshold, mult) n |> CatalyticRacemizationRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatRacemSimParamWithModel
            |> CatalyticRacemizationModel.create

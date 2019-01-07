namespace Clm.Generator

open System
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.ModelParams


module DefaultValuesExt =

    type NumberOfAminoAcids
        with
        static member defaultValue = EightAminoAcids


    type MaxPeptideLength
        with
        static member defaultValue = ThreeMax


    type ModelCommandLineParam
        with

        static member defaultValues =
            [
                //{
                //    tEnd = 10_000.0
                //    y0 = 10.0
                //    useAbundant = false
                //    saveModelSettings = false
                //}

                {
                    tEnd = 50_000.0
                    y0 = 10.0
                    useAbundant = false
                    saveModelSettings = false
                }

                {
                    tEnd = 50_000.0
                    y0 = 5.0
                    useAbundant = false
                    saveModelSettings = false
                }

                {
                    tEnd = 50_000.0
                    y0 = 20.0
                    useAbundant = false
                    saveModelSettings = false
                }
            ]

    let defaultRateMultiplierDistr (rnd : Random) threshold mult =
        TriangularDistribution(rnd.Next(), { threshold = threshold; scale = Some mult; shift = None }) |> Triangular |> RateMultDistr

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

        static member defaultSynthRndModel (rnd : Random) (forward, backward) =
            {
                synthesisDistribution = DeltaDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                //synthesisDistribution = UniformDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SynthRndParam
            |> SynthesisModel.create

        static member defaultDestrRndModel (rnd : Random) (forward, backward) =
            {
                destructionDistribution = DeltaDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> DestrRndParam
            |> DestructionModel.create

        static member defaultCatSynthRndParams (rnd : Random) (m, threshold, mult) =
            {
                catSynthRndParam = 
                    {
                        catSynthRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr rnd threshold mult
                                eeForwardDistribution = defaultEeDistribution rnd.Next |> Some
                                eeBackwardDistribution = defaultEeDistribution rnd.Next |> Some
                            }
                    }
                synthesisModel = m
            }

        static member defaultCatSynthRndModel (rnd : Random) (m, threshold, mult) = 
            ReactionRateProvider.defaultCatSynthRndParams rnd (m, threshold, mult)
            |> CatSynthRndParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultCatSynthSimModel (rnd : Random) (m, threshold, mult) (simThreshold, n) =
            {
                catSynthSimParam = 
                    {
                        simBaseDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold; scale = None; shift = Some 1.0 }) |> Uniform
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catSynthModel = ReactionRateProvider.defaultCatSynthRndParams rnd (m, threshold, mult) |> CatalyticSynthesisRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatSynthSimParamWithModel
            |> CatalyticSynthesisModel.create

        static member defaultCatDestrRndParams (rnd : Random) (m, threshold, mult) =
            {
                catDestrRndParam = 
                    {
                        catDestrRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr rnd threshold mult
                                eeForwardDistribution = defaultEeDistribution rnd.Next |> Some
                                eeBackwardDistribution = defaultEeDistribution rnd.Next |> Some
                            }

                    }
                destructionModel = m
            }

        static member defaultCatDestrRndModel (rnd : Random) (m, threshold, mult) = 
            ReactionRateProvider.defaultCatDestrRndParams rnd (m, threshold, mult)
            |> CatDestrRndParamWithModel
            |> CatalyticDestructionModel.create

        static member defaultCatDestrSimModel (rnd : Random) (m, threshold, mult) (simThreshold, n) =
            {
                catDestrSimParam = 
                    {
                        simBaseDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold; scale = None; shift = Some 1.0 }) |> Uniform
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catDestrModel = ReactionRateProvider.defaultCatDestrRndParams rnd (m, threshold, mult) |> CatalyticDestructionRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatDestrSimParamWithModel
            |> CatalyticDestructionModel.create

        static member defaultLigRndModel (rnd : Random) (forward, backward) =
            {
                ligationDistribution = DeltaDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                //ligationDistribution = UniformDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigRndParam
            |> LigationModel.create

        static member defaultCatLigRndModel (rnd : Random) (m, threshold, mult) =
            {
                catLigationParam = 
                    {
                        catLigRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr rnd threshold mult
                                eeForwardDistribution = defaultEeDistribution rnd.Next |> Some
                                eeBackwardDistribution = defaultEeDistribution rnd.Next |> Some
                            }
                    }
                ligationModel = m
            }
            |> CatLigRndParamWithModel
            |> CatalyticLigationModel.create

        static member defaultSedDirRndModel (rnd : Random) (threshold, mult) =
            {
                sedimentationDirectDistribution = TriangularDistribution(rnd.Next(), { threshold = Some threshold; scale = None; shift = None }) |> Triangular
                forwardScale = Some mult
            }
            |> SedDirRndParam
            |> SedimentationDirectModel.create

        static member defaultSedAllRndModel (rnd : Random) mult =
            {
                sedimentationAllDistribution = TriangularDistribution(rnd.Next(), { threshold = None; scale = None; shift = None }) |> Triangular
                forwardScale = Some mult
            }
            |> SedAllRndParam
            |> SedimentationAllModel.create

        static member defaultRacemRndModel (rnd : Random) forward =
            {
                racemizationDistribution = DeltaDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                //racemizationDistribution = UniformDistribution(rnd.Next(), { threshold = None; scale = None; shift = Some 1.0 }) |> Uniform
                forwardScale = Some forward
            }
            |> RacemRndParam
            |> RacemizationModel.create

        static member defaultCatRacemRndParams (rnd : Random) (m, threshold, mult) n =
            {
                catRacemRndParam = 
                    {
                        catRacemRndEeParams =
                            {
                                rateMultiplierDistr = defaultRateMultiplierDistr rnd threshold mult
                                eeForwardDistribution = defaultEeDistribution rnd.Next |> Some
                                eeBackwardDistribution = defaultEeDistribution rnd.Next |> Some
                            }

                    }
                racemizationModel = m
                aminoAcids = AminoAcid.getAminoAcids n
            }

        static member defaultCatRacemRndModel (rnd : Random) (m, threshold, mult) n = 
            ReactionRateProvider.defaultCatRacemRndParams rnd (m, threshold, mult) n
            |> CatRacemRndParamWithModel
            |> CatalyticRacemizationModel.create

        static member defaultCatRacemSimModel (rnd : Random) (m, threshold, mult) (simThreshold, n) =
            {
                catRacemSimParam = 
                    {
                        simBaseDistribution = UniformDistribution(rnd.Next(), { threshold = simThreshold; scale = None; shift = Some 1.0 }) |> Uniform
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
                catRacemModel = ReactionRateProvider.defaultCatRacemRndParams rnd (m, threshold, mult) n |> CatalyticRacemizationRandomModel
                aminoAcids = AminoAcid.getAminoAcids n
            }
            |> CatRacemSimParamWithModel
            |> CatalyticRacemizationModel.create

        static member getDefaultRateModels (rnd : Random) numberOfAminoAcids = 
            //===========================================================
            let foodModel = ReactionRateProvider.defaultFoodCreationModel 0.01
            let wasteModel = ReactionRateProvider.defaultWasteRemovalModel 10.0
            let wasteRecyclingModel = ReactionRateProvider.defaultWasteRecyclingModel 0.1
            //===========================================================
            let synthModel = ReactionRateProvider.defaultSynthRndModel rnd (0.001, 0.001)
            //let catSynthRndParams = (synthModel, (Some 0.02), 1000.0)
            let catSynthRndParams = (synthModel, (Some 0.002), 10000.0)
            //let catSynthRndParams = (synthModel, (Some 0.0005), 1000.0)
            //let catSynthModel = ReactionRateProvider.defaultCatSynthRndModel rnd catSynthRndParams
            let catSynthModel = ReactionRateProvider.defaultCatSynthSimModel rnd catSynthRndParams (Some 0.3, numberOfAminoAcids)
            //===========================================================
            let destrModel = ReactionRateProvider.defaultDestrRndModel rnd (0.001, 0.001)
            //let catDestrRndParams = (destrModel, (Some 0.02), 1000.0)
            let catDestrRndParams = (destrModel, (Some 0.002), 10000.0)
            //let catDestrRndParams = (destrModel, (Some 0.0005), 1000.0)
            //let catDestrModel = ReactionRateProvider.defaultCatDestrRndModel rnd catDestrRndParams
            let catDestrModel = ReactionRateProvider.defaultCatDestrSimModel rnd catDestrRndParams (Some 0.3, numberOfAminoAcids)
            //===========================================================
            //let ligModel = ReactionRateProvider.defaultLigRndModel rnd (0.001, 0.0001)
            //let ligModel = ReactionRateProvider.defaultLigRndModel rnd (1.0, 0.1)
            let ligModel = ReactionRateProvider.defaultLigRndModel rnd (1.0, 1.0)
            let catLigModel = ReactionRateProvider.defaultCatLigRndModel rnd (ligModel, (Some 0.00005), 2000.0)
            //===========================================================
            let sedDirModel = ReactionRateProvider.defaultSedDirRndModel rnd (0.00001, 10000.0)
            let sedAllModel = ReactionRateProvider.defaultSedAllRndModel rnd 0.1
            //===========================================================
            let racemModel = ReactionRateProvider.defaultRacemRndModel rnd 0.001
            //let catRacemRndParams = (racemModel, (Some 0.02), 1000.0)
            let catRacemRndParams = (racemModel, (Some 0.0005), 1000.0)
            //let catRacemModel = ReactionRateProvider.defaultCatRacemRndModel rnd catRacemRndParams
            let catRacemModel = ReactionRateProvider.defaultCatRacemSimModel rnd catRacemRndParams (Some 0.2, numberOfAminoAcids)
            //===========================================================
            let rates = 
                [
                    //foodModel |> FoodCreationRateModel
                    //wasteModel |> WasteRemovalRateModel
                    wasteRecyclingModel |> WasteRecyclingRateModel

                    synthModel |> SynthesisRateModel
                    catSynthModel |> CatalyticSynthesisRateModel

                    destrModel |> DestructionRateModel
                    catDestrModel |> CatalyticDestructionRateModel

                    ligModel |> LigationRateModel
                    //catLigModel |> CatalyticLigationRateModel

                    sedDirModel |> SedimentationDirectRateModel
                    //sedAllModel |> SedimentationAllRateModel

                    //racemModel |> RacemizationRateModel
                    //catRacemModel |> CatalyticRacemizationRateModel
                ]
            //===========================================================

            {
                rateModels = rates
            }

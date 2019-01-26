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
            getDefaultRateModels : Random -> NumberOfAminoAcids -> ReactionRateProviderParams
            description : string option
        }


    type NumberOfAminoAcids
        with
        static member defaultValue = EightAminoAcids


    type MaxPeptideLength
        with
        static member defaultValue = ThreeMax


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

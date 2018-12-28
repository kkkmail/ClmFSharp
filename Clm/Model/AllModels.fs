﻿namespace Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionRates

module AllModels = 

    /// !!! This file grows automatically at the end. Do not modify it without extreme need !!!
    let allModelData : list<ModelDataParams> = 
        []

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.4.0.0"
                        versionNumber = "1.4.0.0"
                        seedValue = 296048539
                        modelName = "20181228_001"
                        numberOfSubstances = 2957
                        numberOfAminoAcids = NumberOfAminoAcids.SevenAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1564553590, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(273711569, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(682600652, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1810175435, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(993352825, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(2007085943, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(826854255, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(2115989694, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1868873990, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1323386060, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(1558384740, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(366186617, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.4.0.0"
                        versionNumber = "1.4.0.0"
                        seedValue = 1066329302
                        modelName = "20181228_002"
                        numberOfSubstances = 2957
                        numberOfAminoAcids = NumberOfAminoAcids.SevenAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(757357497, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1632873158, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1602395540, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(2046720184, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1670843967, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1116901141, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1666063870, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1791608639, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1086495220, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1077894528, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(1962442992, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(2100421908, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.4.0.0"
                        versionNumber = "1.4.0.0"
                        seedValue = 1442476290
                        modelName = "20181228_003"
                        numberOfSubstances = 2957
                        numberOfAminoAcids = NumberOfAminoAcids.SevenAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1588854593, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(57297288, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(420438452, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(194999974, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1248771863, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1214419710, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(4015523, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1146978312, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1579610761, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.SevenAminoAcids
                            simBaseDistribution = UniformDistribution(1617167837, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(2075908917, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(2049758365, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

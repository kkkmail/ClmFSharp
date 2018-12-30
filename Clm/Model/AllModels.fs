namespace Model

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
                            simBaseDistribution = UniformDistribution(1868873990, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
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
                            simBaseDistribution = UniformDistribution(1086495220, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
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
                            simBaseDistribution = UniformDistribution(1579610761, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
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

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.4.0.0"
                        versionNumber = "1.4.0.0"
                        seedValue = 896839875
                        modelName = "20181228_004"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(430486633, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1663094897, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1597825899, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1334792596, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(573079032, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1185754058, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(241186297, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1265467711, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(299503970, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(1211517322, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(1888980690, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1160909273, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 361829328
                        modelName = "20181228_005"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(192169931, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1379965993, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1499466551, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(57871579, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1485017795, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1676715370, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1182721800, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(285307679, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(91537954, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(1503993738, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(287883917, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1665530585, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 80551969
                        modelName = "20181228_006"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(944080121, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1521159744, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1902561282, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1115750241, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1271197955, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1944565269, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1705286060, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(557696666, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(441227815, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(258585997, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(2018638564, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(23051459, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 1352682477
                        modelName = "20181228_007"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1655930713, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(258753120, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1289822598, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(4592437, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(745475978, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(245089532, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1588949805, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(920995185, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(551824354, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(937158627, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(1523214645, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(297448484, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 1749018091
                        modelName = "20181228_008"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(943595153, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1084586828, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1177169595, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1899067220, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(283320736, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(2014346281, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(344830934, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(270756842, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(469334260, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(1679171839, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(1832945637, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(692606361, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 1271013664
                        modelName = "20181228_009"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1031059051, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1367800739, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1753346816, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(1179113213, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1700957000, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(935487659, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = BiDeltaDistribution(1436278334, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                eeBackwardDistribution = BiDeltaDistribution(2130268062, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            simBaseDistribution = UniformDistribution(99390125, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            simBaseDistribution = UniformDistribution(1691466797, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(2129140428, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1570939913, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

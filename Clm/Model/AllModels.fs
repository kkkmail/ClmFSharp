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
                        seedValue = 1668782939
                        modelName = "20181226_001"
                        numberOfSubstances = 87
                        numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Model\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(2033200790, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1810128402, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(1222591761, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1993742753, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1675656516, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1335989957, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(1928254665, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1624763383, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(1796547180, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(1173167686, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(2063961807, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(602411644, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 1483953118
                        modelName = "20181226_002"
                        numberOfSubstances = 87
                        numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Model\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1372965110, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1464123118, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(1966000591, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(995376366, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(1286031712, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(539760072, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(656424599, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(828545544, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(578412407, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(715490468, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(765466863, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1361334534, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
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
                        seedValue = 1561763670
                        modelName = "20181226_001"
                        numberOfSubstances = 87
                        numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Model\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1535808504, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(133609752, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(353994536, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(203551817, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(705267423, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1211534244, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(121350976, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1812411326, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(1870681015, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(643879647, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(882242312, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1911881595, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

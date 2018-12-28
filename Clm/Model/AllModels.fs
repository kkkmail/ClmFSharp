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

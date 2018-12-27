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
                        seedValue = 1545800024
                        modelName = "20181227_001"
                        numberOfSubstances = 87
                        numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                        updateAllModels = true
                        allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(2014366441, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(480996371, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(1674549614, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1226476112, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(219813534, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(760006962, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(959517780, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1844898300, { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution |> Some
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
                            simBaseDistribution = UniformDistribution(1997345854, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            simBaseDistribution = UniformDistribution(1699945714, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                            getRateMultiplierDistr = DeltaRateMultDistrGetter
                            getForwardEeDistr = DeltaEeDistributionGetter
                            getBackwardEeDistr = DeltaEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(140392241, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1752195026, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

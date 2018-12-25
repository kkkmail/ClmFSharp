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
                        fileStructureVersionNumber = "1.3.0.0"
                        versionNumber = "1.3.0.0"
                        seedValue = 1642449958
                        modelName = "20181225_005"
                        numberOfSubstances = 4371
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(159604056, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            catSynthRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(795497436, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultiplierDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(2004939701, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(1119685725, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                            }
                        }
                        |> CatSynthRndParam
                        |> CatalyticSynthesisRateParam

                        {
                            destructionDistribution = DeltaDistribution(946985345, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> DestrRndParam
                        |> DestructionRateParam

                        {
                            catDestrRndEeParams = 
                            {
                                rateMultiplierDistr = TriangularDistribution(1663864288, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultiplierDistr
                                eeForwardDistribution = SymmetricTriangularDistribution(808586257, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                eeBackwardDistribution = SymmetricTriangularDistribution(233318278, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                            }
                        }
                        |> CatDestrRndParam
                        |> CatalyticDestructionRateParam

                        {
                            wasteRecyclingRate = 0.1
                        }
                        |> WasteRecyclingRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.EightAminoAcids
                            simBaseDistribution = UniformDistribution(2049852946, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DefaultRateMultiplierDistributionGetter
                            getForwardEeDistr = DefaultEeDistributionGetter
                            getBackwardEeDistr = DefaultEeDistributionGetter
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.EightAminoAcids
                            simBaseDistribution = UniformDistribution(89721071, { threshold = Some 0.3; scale = None; shift = None }) |> Uniform
                            getRateMultiplierDistr = DefaultRateMultiplierDistributionGetter
                            getForwardEeDistr = DefaultEeDistributionGetter
                            getBackwardEeDistr = DefaultEeDistributionGetter
                        }
                        |> CatDestrSimParam
                        |> CatalyticDestructionRateParam

                        {
                            ligationDistribution = DeltaDistribution(2075981483, { threshold = None; scale = None; shift = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(944687071, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                            forwardScale = Some 10000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

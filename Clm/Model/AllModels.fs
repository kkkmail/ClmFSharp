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
                        fileStructureVersionNumber = "1.1.0.0"
                        versionNumber = "1.1.1.1"
                        seedValue = 808991140
                        modelName = "20181207_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(465935409, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            similarityDistribution = UniformDistribution(66123702, { threshold = Some 0.3 }) |> Uniform
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.NineAminoAcids
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(924538616, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

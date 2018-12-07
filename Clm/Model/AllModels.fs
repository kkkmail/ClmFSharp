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
                        seedValue = 1578742997
                        modelName = "20181207_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(2056992107, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            similarityDistribution = UniformDistribution(1131838359, { threshold = Some 0.3 }) |> Uniform
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.NineAminoAcids
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1028838218, { threshold = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 1.0
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(2045826034, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
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
                        fileStructureVersionNumber = "1.1.0.0"
                        versionNumber = "1.1.1.1"
                        seedValue = 1978189886
                        modelName = "20181207_002"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(956829149, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            similarityDistribution = UniformDistribution(1069625908, { threshold = Some 0.3 }) |> Uniform
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.NineAminoAcids
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(172849280, { threshold = None }) |> Delta
                            forwardScale = Some 1.0
                            backwardScale = Some 0.1
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(291930028, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
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
                        fileStructureVersionNumber = "1.1.0.0"
                        versionNumber = "1.1.1.1"
                        seedValue = 195809352
                        modelName = "20181207_003"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(2074498820, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthRndParam
                        |> SynthesisRateParam

                        {
                            similarityDistribution = UniformDistribution(413113090, { threshold = Some 0.3 }) |> Uniform
                            aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.NineAminoAcids
                        }
                        |> CatSynthSimParam
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1041697553, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigRndParam
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(744632255, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatLigRndParam
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1717173744, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
                        }
                        |> SedDirRndParam
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

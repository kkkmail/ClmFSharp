namespace Model

open Clm.Substances
open Clm.Model
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
                        fileStructureVersionNumber = "1.0.0.0"
                        versionNumber = "1.0.1.0"
                        seedValue = 931836828
                        modelName = "20181201_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.01
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.01
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(788444922, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1521689090, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1341634634, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.0.0.0"
                        versionNumber = "1.0.1.1"
                        seedValue = 1004674863
                        modelName = "20181202_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.01
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1475197436, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(528856703, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(729571335, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 1000.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

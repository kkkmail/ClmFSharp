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
                        versionNumber = "1.0.1.3"
                        seedValue = 1466918266
                        modelName = "20181202_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1569963708, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1169633412, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = "1.0.0.0"
                        versionNumber = "1.0.1.3"
                        seedValue = 2030601908
                        modelName = "20181202_002"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1087047718, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1156254973, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1934640995, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1404621921, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1736339285, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 638792
                        modelName = "20181202_003"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1798613707, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(607507982, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1512764458, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(418062945, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1470402250, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 2034239469
                        modelName = "20181202_004"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(976038139, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1413994920, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(761319550, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(717477240, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1562450979, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 1151741151
                        modelName = "20181202_005"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(863227964, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(355042818, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(978931550, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(222168155, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1682721387, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 2007999828
                        modelName = "20181203_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1633951851, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1009667870, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(904249223, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(991174019, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(2146427344, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 1805282523
                        modelName = "20181203_002"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(10026253, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(162093612, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1640876548, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1722212776, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 10000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1263418113, { threshold = Some 0.0001 }) |> Triangular
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
                        versionNumber = "1.0.1.3"
                        seedValue = 1611646993
                        modelName = "20181203_003"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(872392824, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1173733312, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1641388442, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1746552531, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(174805283, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
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
                        versionNumber = "1.0.1.3"
                        seedValue = 564720148
                        modelName = "20181203_004"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(1018072411, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(2080351893, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catSynthDistribution = TriangularDistribution(431097964, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1949759694, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(2117992622, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

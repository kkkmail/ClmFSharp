namespace Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 1033016831
    let numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 15

    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids

    let allSubst = 
        [ Substance.food ]
        @
        (chiralAminoAcids |> List.map (fun a -> Chiral a))
        @
        (peptides |> List.map (fun p -> PeptideChain p))

    let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList


    let getTotalSubst (x : array<double>) = 
        [|
            x.[0] // Y
            x.[1] // A
            x.[2] // a
            2.0 * x.[3] // AA
            2.0 * x.[4] // Aa
            2.0 * x.[5] // aA
            2.0 * x.[6] // aa
            3.0 * x.[7] // AAA
            3.0 * x.[8] // AAa
            3.0 * x.[9] // AaA
            3.0 * x.[10] // Aaa
            3.0 * x.[11] // aAA
            3.0 * x.[12] // aAa
            3.0 * x.[13] // aaA
            3.0 * x.[14] // aaa
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[3] // AA
                    x.[4] // Aa
                    x.[5] // aA
                    3.0 * x.[7] // AAA
                    2.0 * x.[8] // AAa
                    2.0 * x.[9] // AaA
                    x.[10] // Aaa
                    2.0 * x.[11] // aAA
                    x.[12] // aAa
                    x.[13] // aaA
                |]
                |> Array.sum
                ,
                [|
                    x.[2] // a
                    x.[4] // Aa
                    x.[5] // aA
                    2.0 * x.[6] // aa
                    x.[8] // AAa
                    x.[9] // AaA
                    2.0 * x.[10] // Aaa
                    x.[11] // aAA
                    2.0 * x.[12] // aAa
                    2.0 * x.[13] // aaA
                    3.0 * x.[14] // aaa
                |]
                |> Array.sum
            )
        |]



    // 0 - Y
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[2] // a | synthesis: Y <-> a
            -0.001 * x.[0] // Y | synthesis: Y <-> a
            0.0001 * x.[1] // A | synthesis: Y <-> A
            -0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 1 - A
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[10] // Aaa | ligation: A + aa <-> Aaa
            -0.001 * x.[1] * x.[6] // A + aa | ligation: A + aa <-> Aaa
            0.0001 * x.[9] // AaA | ligation: A + aA <-> AaA
            -0.001 * x.[1] * x.[5] // A + aA | ligation: A + aA <-> AaA
            0.0001 * x.[8] // AAa | ligation: A + Aa <-> AAa
            -0.001 * x.[1] * x.[4] // A + Aa | ligation: A + Aa <-> AAa
            0.0001 * x.[7] // AAA | ligation: A + AA <-> AAA
            -0.001 * x.[1] * x.[3] // A + AA | ligation: A + AA <-> AAA
            0.0001 * x.[5] // aA | ligation: a + A <-> aA
            -0.001 * x.[2] * x.[1] // a + A | ligation: a + A <-> aA
            0.0001 * x.[4] // Aa | ligation: A + a <-> Aa
            -0.001 * x.[1] * x.[2] // A + a | ligation: A + a <-> Aa
            0.0001 * x.[3] // AA | ligation: A + A <-> AA
            0.0001 * x.[3] // AA | ligation: A + A <-> AA
            -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
            -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
            -0.0001 * x.[1] // A | synthesis: Y <-> A
            0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 2 - a
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[11] // aAA | ligation: a + AA <-> aAA
            -0.001 * x.[2] * x.[3] // a + AA | ligation: a + AA <-> aAA
            0.0001 * x.[12] // aAa | ligation: a + Aa <-> aAa
            -0.001 * x.[2] * x.[4] // a + Aa | ligation: a + Aa <-> aAa
            0.0001 * x.[13] // aaA | ligation: a + aA <-> aaA
            -0.001 * x.[2] * x.[5] // a + aA | ligation: a + aA <-> aaA
            0.0001 * x.[14] // aaa | ligation: a + aa <-> aaa
            -0.001 * x.[2] * x.[6] // a + aa | ligation: a + aa <-> aaa
            0.0001 * x.[5] // aA | ligation: a + A <-> aA
            -0.001 * x.[2] * x.[1] // a + A | ligation: a + A <-> aA
            0.0001 * x.[4] // Aa | ligation: A + a <-> Aa
            -0.001 * x.[1] * x.[2] // A + a | ligation: A + a <-> Aa
            0.0001 * x.[6] // aa | ligation: a + a <-> aa
            0.0001 * x.[6] // aa | ligation: a + a <-> aa
            -0.001 * x.[2] * x.[2] // a + a | ligation: a + a <-> aa
            -0.001 * x.[2] * x.[2] // a + a | ligation: a + a <-> aa
            -0.0001 * x.[2] // a | synthesis: Y <-> a
            0.001 * x.[0] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 3 - AA
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[11] // aAA | ligation: a + AA <-> aAA
            -0.001 * x.[2] * x.[3] // a + AA | ligation: a + AA <-> aAA
            0.0001 * x.[7] // AAA | ligation: A + AA <-> AAA
            -0.001 * x.[1] * x.[3] // A + AA | ligation: A + AA <-> AAA
            -0.0001 * x.[3] // AA | ligation: A + A <-> AA
            0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 4 - Aa
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[12] // aAa | ligation: a + Aa <-> aAa
            -0.001 * x.[2] * x.[4] // a + Aa | ligation: a + Aa <-> aAa
            0.0001 * x.[8] // AAa | ligation: A + Aa <-> AAa
            -0.001 * x.[1] * x.[4] // A + Aa | ligation: A + Aa <-> AAa
            -0.0001 * x.[4] // Aa | ligation: A + a <-> Aa
            0.001 * x.[1] * x.[2] // A + a | ligation: A + a <-> Aa
        |]
        |> Array.sum


    // 5 - aA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[9] // AaA | ligation: A + aA <-> AaA
            -0.001 * x.[1] * x.[5] // A + aA | ligation: A + aA <-> AaA
            0.0001 * x.[13] // aaA | ligation: a + aA <-> aaA
            -0.001 * x.[2] * x.[5] // a + aA | ligation: a + aA <-> aaA
            -0.0001 * x.[5] // aA | ligation: a + A <-> aA
            0.001 * x.[2] * x.[1] // a + A | ligation: a + A <-> aA
        |]
        |> Array.sum


    // 6 - aa
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.0001 * x.[10] // Aaa | ligation: A + aa <-> Aaa
            -0.001 * x.[1] * x.[6] // A + aa | ligation: A + aa <-> Aaa
            0.0001 * x.[14] // aaa | ligation: a + aa <-> aaa
            -0.001 * x.[2] * x.[6] // a + aa | ligation: a + aa <-> aaa
            -0.0001 * x.[6] // aa | ligation: a + a <-> aa
            0.001 * x.[2] * x.[2] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 7 - AAA
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[7] // AAA | ligation: A + AA <-> AAA
            0.001 * x.[1] * x.[3] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 8 - AAa
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[8] // AAa | ligation: A + Aa <-> AAa
            0.001 * x.[1] * x.[4] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 9 - AaA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[9] // AaA | ligation: A + aA <-> AaA
            0.001 * x.[1] * x.[5] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 10 - Aaa
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[10] // Aaa | ligation: A + aa <-> Aaa
            0.001 * x.[1] * x.[6] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 11 - aAA
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[11] // aAA | ligation: a + AA <-> aAA
            0.001 * x.[2] * x.[3] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 12 - aAa
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[12] // aAa | ligation: a + Aa <-> aAa
            0.001 * x.[2] * x.[4] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 13 - aaA
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[13] // aaA | ligation: a + aA <-> aaA
            0.001 * x.[2] * x.[5] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 14 - aaa
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.0001 * x.[14] // aaa | ligation: a + aa <-> aaa
            0.001 * x.[2] * x.[6] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

        let xSum = (x |> Array.sum) - x.[0]


        let xSumN = 
            [|
                1.0 * x.[1] // A
                1.0 * x.[2] // a
                2.0 * x.[3] // AA
                2.0 * x.[4] // Aa
                2.0 * x.[5] // aA
                2.0 * x.[6] // aa
                3.0 * x.[7] // AAA
                3.0 * x.[8] // AAa
                3.0 * x.[9] // AaA
                3.0 * x.[10] // Aaa
                3.0 * x.[11] // aAA
                3.0 * x.[12] // aAa
                3.0 * x.[13] // aaA
                3.0 * x.[14] // aaa
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[1] * x.[1] // A
                1.0 * x.[2] * x.[2] // a
                2.0 * x.[3] * x.[3] // AA
                2.0 * x.[4] * x.[4] // Aa
                2.0 * x.[5] * x.[5] // aA
                2.0 * x.[6] * x.[6] // aa
                3.0 * x.[7] * x.[7] // AAA
                3.0 * x.[8] * x.[8] // AAa
                3.0 * x.[9] * x.[9] // AaA
                3.0 * x.[10] * x.[10] // Aaa
                3.0 * x.[11] * x.[11] // aAA
                3.0 * x.[12] * x.[12] // aAa
                3.0 * x.[13] * x.[13] // aaA
                3.0 * x.[14] * x.[14] // aaa
            |]
            |> Array.sum

        [|
            d0 x xSum xSumN xSumSquaredN
            d1 x xSum xSumN xSumSquaredN
            d2 x xSum xSumN xSumSquaredN
            d3 x xSum xSumN xSumSquaredN
            d4 x xSum xSumN xSumSquaredN
            d5 x xSum xSumN xSumSquaredN
            d6 x xSum xSumN xSumSquaredN
            d7 x xSum xSumN xSumSquaredN
            d8 x xSum xSumN xSumSquaredN
            d9 x xSum xSumN xSumSquaredN
            d10 x xSum xSumN xSumSquaredN
            d11 x xSum xSumN xSumSquaredN
            d12 x xSum xSumN xSumSquaredN
            d13 x xSum xSumN xSumSquaredN
            d14 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = "1.1.0.0"
                            versionNumber = "1.1.0.0"
                            seedValue = seedValue
                            modelName = "20181204_005"
                            numberOfSubstances = 15
                            numberOfAminoAcids = OneAminoAcid
                            maxPeptideLength = ThreeMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution(481209614, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.0001
                            }
                            |> SynthRndParam
                            |> SynthesisRateParam

                            {
                                ligationDistribution = DeltaDistribution(679914380, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.0001
                            }
                            |> LigRndParam
                            |> LigationRateParam

                            {
                                catSynthDistribution = TriangularDistribution(1284695742, { threshold = Some 0.0005 }) |> Triangular
                                multiplier = 1000.0
                                maxEe = 0.05
                            }
                            |> CatSynthRndParam
                            |> CatalyticSynthesisRateParam

                            {
                                catLigationDistribution = TriangularDistribution(667633144, { threshold = Some 0.0001 }) |> Triangular
                                multiplier = 1000.0
                                maxEe = 0.05
                            }
                            |> CatLigRndParam
                            |> CatalyticLigationRateParam

                            {
                                sedimentationDirectDistribution = TriangularDistribution(730634666, { threshold = Some 0.0001 }) |> Triangular
                                forwardScale = Some 1000.0
                            }
                            |> SedDirRndParam
                            |> SedimentationDirectRateParam

                        ]
                }

            getTotals = getTotals
            getTotalSubst = getTotalSubst
            allSubst = allSubst
            allInd = allInd

            allRawReactions = 
                [
                    (SynthesisName, 2)
                    (CatalyticSynthesisName, 24)
                    (LigationName, 6)
                    (CatalyticLigationName, 72)
                    (SedimentationDirectName, 63)
                    (SedimentationAllName, 2)
                ]

            allReactions = 
                [
                    (SynthesisName, 2)
                    (LigationName, 12)
                ]
        }


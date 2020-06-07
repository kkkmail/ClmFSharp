namespace Clm.Model

open System
open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmSys.ContGenPrimitives

module ModelData = 
    let seedValue = 810165255
    let numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 17

    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids

    let allSubst =
        Substance.allSimple
        @
        (chiralAminoAcids |> List.map (fun a -> Chiral a))
        @
        (peptides |> List.map (fun p -> PeptideChain p))

    let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList


    let getTotalSubst (x : array<double>) = 
        [|
            x.[0] // abundant
            x.[1] // food
            x.[2] // waste
            x.[3] // A
            x.[4] // a
            2.0 * x.[5] // AA
            2.0 * x.[6] // Aa
            2.0 * x.[7] // aA
            2.0 * x.[8] // aa
            3.0 * x.[9] // AAA
            3.0 * x.[10] // AAa
            3.0 * x.[11] // AaA
            3.0 * x.[12] // Aaa
            3.0 * x.[13] // aAA
            3.0 * x.[14] // aAa
            3.0 * x.[15] // aaA
            3.0 * x.[16] // aaa
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[3] // A
                    2.0 * x.[5] // AA
                    x.[6] // Aa
                    x.[7] // aA
                    3.0 * x.[9] // AAA
                    2.0 * x.[10] // AAa
                    2.0 * x.[11] // AaA
                    x.[12] // Aaa
                    2.0 * x.[13] // aAA
                    x.[14] // aAa
                    x.[15] // aaA
                |]
                |> Array.sum
                ,
                [|
                    x.[4] // a
                    x.[6] // Aa
                    x.[7] // aA
                    2.0 * x.[8] // aa
                    x.[10] // AAa
                    x.[11] // AaA
                    2.0 * x.[12] // Aaa
                    x.[13] // aAA
                    2.0 * x.[14] // aAa
                    2.0 * x.[15] // aaA
                    3.0 * x.[16] // aaa
                |]
                |> Array.sum
            )
        |]


    // 0 - abundant
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 1 - food
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 2 - waste
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            6.67495988002384 * x.[3] * x.[16] // A + aaa | catalytic destruction: A + aaa -> waste + aaa
            6.67495988002384 * x.[4] * x.[9] // a + AAA | catalytic destruction: a + AAA -> waste + AAA
            260.323435320929 * x.[3] * x.[9] // A + AAA | catalytic destruction: A + AAA -> waste + AAA
            260.323435320929 * x.[4] * x.[16] // a + aaa | catalytic destruction: a + aaa -> waste + aaa
            0.001 * x.[4] // a | destruction: a -> waste
            0.001 * x.[3] // A | destruction: A -> waste
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -6.67495988002384 * x.[3] * x.[16] // A + aaa | catalytic destruction: A + aaa -> waste + aaa
            -260.323435320929 * x.[3] * x.[9] // A + AAA | catalytic destruction: A + AAA -> waste + AAA
            -0.001 * x.[3] // A | destruction: A -> waste
        |]
        |> Array.sum


    // 4 - a
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -6.67495988002384 * x.[4] * x.[9] // a + AAA | catalytic destruction: a + AAA -> waste + AAA
            -260.323435320929 * x.[4] * x.[16] // a + aaa | catalytic destruction: a + aaa -> waste + aaa
            -0.001 * x.[4] // a | destruction: a -> waste
        |]
        |> Array.sum


    // 5 - AA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 6 - Aa
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 7 - aA
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 8 - aa
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 9 - AAA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 10 - AAa
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 11 - AaA
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 12 - Aaa
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 13 - aAA
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 14 - aAa
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 15 - aaA
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 16 - aaa
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum



    let update (xRaw : array<double>) : array<double> = 
        // printfn "update::Starting..."
        let x = xRaw |> Array.map (fun e -> max e 0.0)
        let xSum = (x |> Array.sum) - (x.[1] + x.[2] + x.[0])


        let xSumN = 
            [|
                1.0 * x.[3] // A
                1.0 * x.[4] // a
                2.0 * x.[5] // AA
                2.0 * x.[6] // Aa
                2.0 * x.[7] // aA
                2.0 * x.[8] // aa
                3.0 * x.[9] // AAA
                3.0 * x.[10] // AAa
                3.0 * x.[11] // AaA
                3.0 * x.[12] // Aaa
                3.0 * x.[13] // aAA
                3.0 * x.[14] // aAa
                3.0 * x.[15] // aaA
                3.0 * x.[16] // aaa
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[3] * x.[3] // A
                1.0 * x.[4] * x.[4] // a
                2.0 * x.[5] * x.[5] // AA
                2.0 * x.[6] * x.[6] // Aa
                2.0 * x.[7] * x.[7] // aA
                2.0 * x.[8] * x.[8] // aa
                3.0 * x.[9] * x.[9] // AAA
                3.0 * x.[10] * x.[10] // AAa
                3.0 * x.[11] * x.[11] // AaA
                3.0 * x.[12] * x.[12] // Aaa
                3.0 * x.[13] * x.[13] // aAA
                3.0 * x.[14] * x.[14] // aAa
                3.0 * x.[15] * x.[15] // aaA
                3.0 * x.[16] * x.[16] // aaa
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
            d15 x xSum xSumN xSumSquaredN
            d16 x xSum xSumN xSumSquaredN
        |]

    let modelDataParamsWithExtraData =
                    {
                        regularParams = 
                            {
                                modelDataParams = 
                                    {
                                        modelInfo =
                                            {
                                                fileStructureVersion = 4.0000m
                                                versionNumber = "5.0.3"
                                                modelDataId = ("b7a42bfa-0be6-4d09-91b0-e1d74f70329c" |> Guid |> ModelDataId)
                                                numberOfSubstances = 17
                                                numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
                                                maxPeptideLength = MaxPeptideLength.ThreeMax
                                                seedValue = 810165255
                                                clmDefaultValueId = ClmDefaultValueId 4002000020L
                                            }

                                        allParams =
                                            [|
                                                {
                                                    modelParam = 
                                                        {
                                                            destructionDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                            forwardScale = Some 0.001
                                                            backwardScale = None
                                                        }
                                                        |> DestrRndParam
                                                        |> DestructionRateParam
                                                    usage = PrimaryParam
                                                }

                                                {
                                                    modelParam = 
                                                        {
                                                            destructionParam = 
                                                                {
                                                                    destructionDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                                    forwardScale = Some 0.001
                                                                    backwardScale = None
                                                                }
                                                                |> DestrRndParam

                                                            catDestrRndEeParams = 
                                                                {
                                                                    rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.3; scale = Some 100000.0; shift = None } } |> Distribution |> RateMultDistr
                                                                    eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                                    eeBackwardDistribution = None
                                                                }
                                                        }
                                                        |> CatDestrRndParam
                                                        |> CatalyticDestructionRateParam
                                                    usage = DependsOnParam
                                                }

                                                {
                                                    modelParam = 
                                                        {
                                                            catDestrParam = 
                                                                {
                                                                    destructionParam = 
                                                                        {
                                                                            destructionDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                                            forwardScale = Some 0.001
                                                                            backwardScale = None
                                                                        }
                                                                        |> DestrRndParam

                                                                    catDestrRndEeParams = 
                                                                        {
                                                                            rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.3; scale = Some 100000.0; shift = None } } |> Distribution |> RateMultDistr
                                                                            eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                                            eeBackwardDistribution = None
                                                                        }
                                                                }

                                                            catDestrSimParam = 
                                                                {
                                                                    catRatesSimGeneration = { distributionType = Uniform; distributionParams = { threshold = Some 0.0; scale = None; shift = Some 1.0 } } |> Distribution |> FixedValue
                                                                    getRateMultiplierDistr = DeltaRateMultDistrGetter
                                                                    getForwardEeDistr = DeltaEeDistributionGetter
                                                                    getBackwardEeDistr = DeltaEeDistributionGetter
                                                                }

                                                        }
                                                        |> CatDestrSimParam
                                                        |> CatalyticDestructionRateParam
                                                    usage = PrimaryParam
                                                }

                                            |]
                                    }

                                allSubstData = 
                                    {
                                        allSubst = allSubst
                                        allInd = allInd
                                        allRawReactions =
                                            [
                                                (FoodCreationName, 1L)
                                                (WasteRemovalName, 1L)
                                                (WasteRecyclingName, 1L)
                                                (SynthesisName, 2L)
                                                (DestructionName, 2L)
                                                (CatalyticSynthesisName, 16L)
                                                (CatalyticDestructionName, 16L)
                                                (LigationName, 6L)
                                                (CatalyticLigationName, 48L)
                                                (SedimentationDirectName, 196L)
                                                (SedimentationAllName, 2L)
                                                (RacemizationName, 2L)
                                                (CatalyticRacemizationName, 16L)
                                            ]
                                        allReactions =
                                            [
                                                (DestructionName, 2L)
                                                (CatalyticDestructionName, 4L)
                                            ]
                                    }

                            }

                        funcParams = 
                            {
                                getTotals = getTotals
                                getTotalSubst = getTotalSubst
                                getDerivative = update
                            }

                    }


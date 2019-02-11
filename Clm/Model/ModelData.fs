namespace Clm.Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmSys.GeneralData

module ModelData = 
    let seedValue = 750664970
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 87

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
            x.[0] // X
            x.[1] // Y
            x.[2] // Z
            x.[3] // A
            x.[4] // B
            x.[5] // a
            x.[6] // b
            2.0 * x.[7] // AA
            2.0 * x.[8] // AB
            2.0 * x.[9] // Aa
            2.0 * x.[10] // Ab
            2.0 * x.[11] // BA
            2.0 * x.[12] // BB
            2.0 * x.[13] // Ba
            2.0 * x.[14] // Bb
            2.0 * x.[15] // aA
            2.0 * x.[16] // aB
            2.0 * x.[17] // aa
            2.0 * x.[18] // ab
            2.0 * x.[19] // bA
            2.0 * x.[20] // bB
            2.0 * x.[21] // ba
            2.0 * x.[22] // bb
            3.0 * x.[23] // AAA
            3.0 * x.[24] // AAB
            3.0 * x.[25] // AAa
            3.0 * x.[26] // AAb
            3.0 * x.[27] // ABA
            3.0 * x.[28] // ABB
            3.0 * x.[29] // ABa
            3.0 * x.[30] // ABb
            3.0 * x.[31] // AaA
            3.0 * x.[32] // AaB
            3.0 * x.[33] // Aaa
            3.0 * x.[34] // Aab
            3.0 * x.[35] // AbA
            3.0 * x.[36] // AbB
            3.0 * x.[37] // Aba
            3.0 * x.[38] // Abb
            3.0 * x.[39] // BAA
            3.0 * x.[40] // BAB
            3.0 * x.[41] // BAa
            3.0 * x.[42] // BAb
            3.0 * x.[43] // BBA
            3.0 * x.[44] // BBB
            3.0 * x.[45] // BBa
            3.0 * x.[46] // BBb
            3.0 * x.[47] // BaA
            3.0 * x.[48] // BaB
            3.0 * x.[49] // Baa
            3.0 * x.[50] // Bab
            3.0 * x.[51] // BbA
            3.0 * x.[52] // BbB
            3.0 * x.[53] // Bba
            3.0 * x.[54] // Bbb
            3.0 * x.[55] // aAA
            3.0 * x.[56] // aAB
            3.0 * x.[57] // aAa
            3.0 * x.[58] // aAb
            3.0 * x.[59] // aBA
            3.0 * x.[60] // aBB
            3.0 * x.[61] // aBa
            3.0 * x.[62] // aBb
            3.0 * x.[63] // aaA
            3.0 * x.[64] // aaB
            3.0 * x.[65] // aaa
            3.0 * x.[66] // aab
            3.0 * x.[67] // abA
            3.0 * x.[68] // abB
            3.0 * x.[69] // aba
            3.0 * x.[70] // abb
            3.0 * x.[71] // bAA
            3.0 * x.[72] // bAB
            3.0 * x.[73] // bAa
            3.0 * x.[74] // bAb
            3.0 * x.[75] // bBA
            3.0 * x.[76] // bBB
            3.0 * x.[77] // bBa
            3.0 * x.[78] // bBb
            3.0 * x.[79] // baA
            3.0 * x.[80] // baB
            3.0 * x.[81] // baa
            3.0 * x.[82] // bab
            3.0 * x.[83] // bbA
            3.0 * x.[84] // bbB
            3.0 * x.[85] // bba
            3.0 * x.[86] // bbb
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[3] // A
                    2.0 * x.[7] // AA
                    x.[8] // AB
                    x.[9] // Aa
                    x.[10] // Ab
                    x.[11] // BA
                    x.[15] // aA
                    x.[19] // bA
                    3.0 * x.[23] // AAA
                    2.0 * x.[24] // AAB
                    2.0 * x.[25] // AAa
                    2.0 * x.[26] // AAb
                    2.0 * x.[27] // ABA
                    x.[28] // ABB
                    x.[29] // ABa
                    x.[30] // ABb
                    2.0 * x.[31] // AaA
                    x.[32] // AaB
                    x.[33] // Aaa
                    x.[34] // Aab
                    2.0 * x.[35] // AbA
                    x.[36] // AbB
                    x.[37] // Aba
                    x.[38] // Abb
                    2.0 * x.[39] // BAA
                    x.[40] // BAB
                    x.[41] // BAa
                    x.[42] // BAb
                    x.[43] // BBA
                    x.[47] // BaA
                    x.[51] // BbA
                    2.0 * x.[55] // aAA
                    x.[56] // aAB
                    x.[57] // aAa
                    x.[58] // aAb
                    x.[59] // aBA
                    x.[63] // aaA
                    x.[67] // abA
                    2.0 * x.[71] // bAA
                    x.[72] // bAB
                    x.[73] // bAa
                    x.[74] // bAb
                    x.[75] // bBA
                    x.[79] // baA
                    x.[83] // bbA
                |]
                |> Array.sum
                ,
                [|
                    x.[5] // a
                    x.[9] // Aa
                    x.[13] // Ba
                    x.[15] // aA
                    x.[16] // aB
                    2.0 * x.[17] // aa
                    x.[18] // ab
                    x.[21] // ba
                    x.[25] // AAa
                    x.[29] // ABa
                    x.[31] // AaA
                    x.[32] // AaB
                    2.0 * x.[33] // Aaa
                    x.[34] // Aab
                    x.[37] // Aba
                    x.[41] // BAa
                    x.[45] // BBa
                    x.[47] // BaA
                    x.[48] // BaB
                    2.0 * x.[49] // Baa
                    x.[50] // Bab
                    x.[53] // Bba
                    x.[55] // aAA
                    x.[56] // aAB
                    2.0 * x.[57] // aAa
                    x.[58] // aAb
                    x.[59] // aBA
                    x.[60] // aBB
                    2.0 * x.[61] // aBa
                    x.[62] // aBb
                    2.0 * x.[63] // aaA
                    2.0 * x.[64] // aaB
                    3.0 * x.[65] // aaa
                    2.0 * x.[66] // aab
                    x.[67] // abA
                    x.[68] // abB
                    2.0 * x.[69] // aba
                    x.[70] // abb
                    x.[73] // bAa
                    x.[77] // bBa
                    x.[79] // baA
                    x.[80] // baB
                    2.0 * x.[81] // baa
                    x.[82] // bab
                    x.[85] // bba
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[4] // B
                    x.[8] // AB
                    x.[11] // BA
                    2.0 * x.[12] // BB
                    x.[13] // Ba
                    x.[14] // Bb
                    x.[16] // aB
                    x.[20] // bB
                    x.[24] // AAB
                    x.[27] // ABA
                    2.0 * x.[28] // ABB
                    x.[29] // ABa
                    x.[30] // ABb
                    x.[32] // AaB
                    x.[36] // AbB
                    x.[39] // BAA
                    2.0 * x.[40] // BAB
                    x.[41] // BAa
                    x.[42] // BAb
                    2.0 * x.[43] // BBA
                    3.0 * x.[44] // BBB
                    2.0 * x.[45] // BBa
                    2.0 * x.[46] // BBb
                    x.[47] // BaA
                    2.0 * x.[48] // BaB
                    x.[49] // Baa
                    x.[50] // Bab
                    x.[51] // BbA
                    2.0 * x.[52] // BbB
                    x.[53] // Bba
                    x.[54] // Bbb
                    x.[56] // aAB
                    x.[59] // aBA
                    2.0 * x.[60] // aBB
                    x.[61] // aBa
                    x.[62] // aBb
                    x.[64] // aaB
                    x.[68] // abB
                    x.[72] // bAB
                    x.[75] // bBA
                    2.0 * x.[76] // bBB
                    x.[77] // bBa
                    x.[78] // bBb
                    x.[80] // baB
                    x.[84] // bbB
                |]
                |> Array.sum
                ,
                [|
                    x.[6] // b
                    x.[10] // Ab
                    x.[14] // Bb
                    x.[18] // ab
                    x.[19] // bA
                    x.[20] // bB
                    x.[21] // ba
                    2.0 * x.[22] // bb
                    x.[26] // AAb
                    x.[30] // ABb
                    x.[34] // Aab
                    x.[35] // AbA
                    x.[36] // AbB
                    x.[37] // Aba
                    2.0 * x.[38] // Abb
                    x.[42] // BAb
                    x.[46] // BBb
                    x.[50] // Bab
                    x.[51] // BbA
                    x.[52] // BbB
                    x.[53] // Bba
                    2.0 * x.[54] // Bbb
                    x.[58] // aAb
                    x.[62] // aBb
                    x.[66] // aab
                    x.[67] // abA
                    x.[68] // abB
                    x.[69] // aba
                    2.0 * x.[70] // abb
                    x.[71] // bAA
                    x.[72] // bAB
                    x.[73] // bAa
                    2.0 * x.[74] // bAb
                    x.[75] // bBA
                    x.[76] // bBB
                    x.[77] // bBa
                    2.0 * x.[78] // bBb
                    x.[79] // baA
                    x.[80] // baB
                    x.[81] // baa
                    2.0 * x.[82] // bab
                    2.0 * x.[83] // bbA
                    2.0 * x.[84] // bbB
                    2.0 * x.[85] // bba
                    3.0 * x.[86] // bbb
                |]
                |> Array.sum
            )
        |]


    // 0 - X
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 1 - Y
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            0.001 * x.[6] // b | synthesis: Y <-> b
            -0.001 * x.[1] // Y | synthesis: Y <-> b
            0.001 * x.[4] // B | synthesis: Y <-> B
            -0.001 * x.[1] // Y | synthesis: Y <-> B
            0.001 * x.[5] // a | synthesis: Y <-> a
            -0.001 * x.[1] // Y | synthesis: Y <-> a
            0.001 * x.[3] // A | synthesis: Y <-> A
            -0.001 * x.[1] // Y | synthesis: Y <-> A
            0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 2 - Z
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -124.52147491494 * x.[2] * x.[79] // Z + baA | catalytic destruction: B + baA <-> Z + baA
            3.19285833115232 * x.[4] * x.[79] // B + baA | catalytic destruction: B + baA <-> Z + baA
            -124.52147491494 * x.[2] * x.[41] // Z + BAa | catalytic destruction: b + BAa <-> Z + BAa
            3.19285833115232 * x.[6] * x.[41] // b + BAa | catalytic destruction: b + BAa <-> Z + BAa
            -3.19285833115232 * x.[2] * x.[41] // Z + BAa | catalytic destruction: B + BAa <-> Z + BAa
            124.52147491494 * x.[4] * x.[41] // B + BAa | catalytic destruction: B + BAa <-> Z + BAa
            -3.19285833115232 * x.[2] * x.[79] // Z + baA | catalytic destruction: b + baA <-> Z + baA
            124.52147491494 * x.[6] * x.[79] // b + baA | catalytic destruction: b + baA <-> Z + baA
            -13.426527118448 * x.[2] * x.[85] // Z + bba | catalytic destruction: A + bba <-> Z + bba
            523.63455761947 * x.[3] * x.[85] // A + bba | catalytic destruction: A + bba <-> Z + bba
            -13.426527118448 * x.[2] * x.[43] // Z + BBA | catalytic destruction: a + BBA <-> Z + BBA
            523.63455761947 * x.[5] * x.[43] // a + BBA | catalytic destruction: a + BBA <-> Z + BBA
            -523.63455761947 * x.[2] * x.[43] // Z + BBA | catalytic destruction: A + BBA <-> Z + BBA
            13.426527118448 * x.[3] * x.[43] // A + BBA | catalytic destruction: A + BBA <-> Z + BBA
            -523.63455761947 * x.[2] * x.[85] // Z + bba | catalytic destruction: a + bba <-> Z + bba
            13.426527118448 * x.[5] * x.[85] // a + bba | catalytic destruction: a + bba <-> Z + bba
            -3.39498740258425 * x.[2] * x.[46] // Z + BBb | catalytic destruction: a + BBb <-> Z + BBb
            132.404508700785 * x.[5] * x.[46] // a + BBb | catalytic destruction: a + BBb <-> Z + BBb
            -3.39498740258425 * x.[2] * x.[84] // Z + bbB | catalytic destruction: A + bbB <-> Z + bbB
            132.404508700785 * x.[3] * x.[84] // A + bbB | catalytic destruction: A + bbB <-> Z + bbB
            -132.404508700785 * x.[2] * x.[84] // Z + bbB | catalytic destruction: a + bbB <-> Z + bbB
            3.39498740258425 * x.[5] * x.[84] // a + bbB | catalytic destruction: a + bbB <-> Z + bbB
            -132.404508700785 * x.[2] * x.[46] // Z + BBb | catalytic destruction: A + BBb <-> Z + BBb
            3.39498740258425 * x.[3] * x.[46] // A + BBb | catalytic destruction: A + BBb <-> Z + BBb
            -6.29040747236337 * x.[2] * x.[76] // Z + bBB | catalytic destruction: b + bBB <-> Z + bBB
            245.325891422171 * x.[6] * x.[76] // b + bBB | catalytic destruction: b + bBB <-> Z + bBB
            -6.29040747236337 * x.[2] * x.[54] // Z + Bbb | catalytic destruction: B + Bbb <-> Z + Bbb
            245.325891422171 * x.[4] * x.[54] // B + Bbb | catalytic destruction: B + Bbb <-> Z + Bbb
            -245.325891422171 * x.[2] * x.[54] // Z + Bbb | catalytic destruction: b + Bbb <-> Z + Bbb
            6.29040747236337 * x.[6] * x.[54] // b + Bbb | catalytic destruction: b + Bbb <-> Z + Bbb
            -245.325891422171 * x.[2] * x.[76] // Z + bBB | catalytic destruction: B + bBB <-> Z + bBB
            6.29040747236337 * x.[4] * x.[76] // B + bBB | catalytic destruction: B + bBB <-> Z + bBB
            -0.58043074863127 * x.[2] * x.[77] // Z + bBa | catalytic destruction: A + bBa <-> Z + bBa
            22.6367991966195 * x.[3] * x.[77] // A + bBa | catalytic destruction: A + bBa <-> Z + bBa
            -0.58043074863127 * x.[2] * x.[51] // Z + BbA | catalytic destruction: a + BbA <-> Z + BbA
            22.6367991966195 * x.[5] * x.[51] // a + BbA | catalytic destruction: a + BbA <-> Z + BbA
            -22.6367991966195 * x.[2] * x.[51] // Z + BbA | catalytic destruction: A + BbA <-> Z + BbA
            0.58043074863127 * x.[3] * x.[51] // A + BbA | catalytic destruction: A + BbA <-> Z + BbA
            -22.6367991966195 * x.[2] * x.[77] // Z + bBa | catalytic destruction: a + bBa <-> Z + bBa
            0.58043074863127 * x.[5] * x.[77] // a + bBa | catalytic destruction: a + bBa <-> Z + bBa
            -23.3162479818138 * x.[2] * x.[57] // Z + aAa | catalytic destruction: a + aAa <-> Z + aAa
            23.3162479818138 * x.[5] * x.[57] // a + aAa | catalytic destruction: a + aAa <-> Z + aAa
            -23.3162479818138 * x.[2] * x.[31] // Z + AaA | catalytic destruction: A + AaA <-> Z + AaA
            23.3162479818138 * x.[3] * x.[31] // A + AaA | catalytic destruction: A + AaA <-> Z + AaA
            -0.597852512354202 * x.[2] * x.[31] // Z + AaA | catalytic destruction: a + AaA <-> Z + AaA
            0.597852512354202 * x.[5] * x.[31] // a + AaA | catalytic destruction: a + AaA <-> Z + AaA
            -0.597852512354202 * x.[2] * x.[57] // Z + aAa | catalytic destruction: A + aAa <-> Z + aAa
            0.597852512354202 * x.[3] * x.[57] // A + aAa | catalytic destruction: A + aAa <-> Z + aAa
            -23.3162479818138 * x.[2] * x.[57] // Z + aAa | catalytic destruction: b + aAa <-> Z + aAa
            23.3162479818138 * x.[6] * x.[57] // b + aAa | catalytic destruction: b + aAa <-> Z + aAa
            -23.3162479818138 * x.[2] * x.[31] // Z + AaA | catalytic destruction: B + AaA <-> Z + AaA
            23.3162479818138 * x.[4] * x.[31] // B + AaA | catalytic destruction: B + AaA <-> Z + AaA
            -0.597852512354202 * x.[2] * x.[31] // Z + AaA | catalytic destruction: b + AaA <-> Z + AaA
            0.597852512354202 * x.[6] * x.[31] // b + AaA | catalytic destruction: b + AaA <-> Z + AaA
            -0.597852512354202 * x.[2] * x.[57] // Z + aAa | catalytic destruction: B + aAa <-> Z + aAa
            0.597852512354202 * x.[4] * x.[57] // B + aAa | catalytic destruction: B + aAa <-> Z + aAa
            -3.62732552426672 * x.[2] * x.[45] // Z + BBa | catalytic destruction: B + BBa <-> Z + BBa
            3.62732552426672 * x.[4] * x.[45] // B + BBa | catalytic destruction: B + BBa <-> Z + BBa
            -3.62732552426672 * x.[2] * x.[83] // Z + bbA | catalytic destruction: b + bbA <-> Z + bbA
            3.62732552426672 * x.[6] * x.[83] // b + bbA | catalytic destruction: b + bbA <-> Z + bbA
            -141.465695446402 * x.[2] * x.[83] // Z + bbA | catalytic destruction: B + bbA <-> Z + bbA
            141.465695446402 * x.[4] * x.[83] // B + bbA | catalytic destruction: B + bbA <-> Z + bbA
            -141.465695446402 * x.[2] * x.[45] // Z + BBa | catalytic destruction: b + BBa <-> Z + BBa
            141.465695446402 * x.[6] * x.[45] // b + BBa | catalytic destruction: b + BBa <-> Z + BBa
            -282.906972423916 * x.[2] * x.[34] // Z + Aab | catalytic destruction: A + Aab <-> Z + Aab
            7.25402493394657 * x.[3] * x.[34] // A + Aab | catalytic destruction: A + Aab <-> Z + Aab
            -282.906972423916 * x.[2] * x.[56] // Z + aAB | catalytic destruction: a + aAB <-> Z + aAB
            7.25402493394657 * x.[5] * x.[56] // a + aAB | catalytic destruction: a + aAB <-> Z + aAB
            -7.25402493394657 * x.[2] * x.[56] // Z + aAB | catalytic destruction: A + aAB <-> Z + aAB
            282.906972423916 * x.[3] * x.[56] // A + aAB | catalytic destruction: A + aAB <-> Z + aAB
            -7.25402493394657 * x.[2] * x.[34] // Z + Aab | catalytic destruction: a + Aab <-> Z + Aab
            282.906972423916 * x.[5] * x.[34] // a + Aab | catalytic destruction: a + Aab <-> Z + Aab
            -7.63579471687961 * x.[2] * x.[73] // Z + bAa | catalytic destruction: B + bAa <-> Z + bAa
            297.795993958305 * x.[4] * x.[73] // B + bAa | catalytic destruction: B + bAa <-> Z + bAa
            -7.63579471687961 * x.[2] * x.[47] // Z + BaA | catalytic destruction: b + BaA <-> Z + BaA
            297.795993958305 * x.[6] * x.[47] // b + BaA | catalytic destruction: b + BaA <-> Z + BaA
            -297.795993958305 * x.[2] * x.[47] // Z + BaA | catalytic destruction: B + BaA <-> Z + BaA
            7.63579471687961 * x.[4] * x.[47] // B + BaA | catalytic destruction: B + BaA <-> Z + BaA
            -297.795993958305 * x.[2] * x.[73] // Z + bAa | catalytic destruction: b + bAa <-> Z + bAa
            7.63579471687961 * x.[6] * x.[73] // b + bAa | catalytic destruction: b + bAa <-> Z + bAa
            -7.63579471687961 * x.[2] * x.[73] // Z + bAa | catalytic destruction: A + bAa <-> Z + bAa
            297.795993958305 * x.[3] * x.[73] // A + bAa | catalytic destruction: A + bAa <-> Z + bAa
            -7.63579471687961 * x.[2] * x.[47] // Z + BaA | catalytic destruction: a + BaA <-> Z + BaA
            297.795993958305 * x.[5] * x.[47] // a + BaA | catalytic destruction: a + BaA <-> Z + BaA
            -297.795993958305 * x.[2] * x.[47] // Z + BaA | catalytic destruction: A + BaA <-> Z + BaA
            7.63579471687961 * x.[3] * x.[47] // A + BaA | catalytic destruction: A + BaA <-> Z + BaA
            -297.795993958305 * x.[2] * x.[73] // Z + bAa | catalytic destruction: a + bAa <-> Z + bAa
            7.63579471687961 * x.[5] * x.[73] // a + bAa | catalytic destruction: a + bAa <-> Z + bAa
            -241.608177711302 * x.[2] * x.[27] // Z + ABA | catalytic destruction: b + ABA <-> Z + ABA
            241.608177711302 * x.[6] * x.[27] // b + ABA | catalytic destruction: b + ABA <-> Z + ABA
            -241.608177711302 * x.[2] * x.[69] // Z + aba | catalytic destruction: B + aba <-> Z + aba
            241.608177711302 * x.[4] * x.[69] // B + aba | catalytic destruction: B + aba <-> Z + aba
            -6.19508147977697 * x.[2] * x.[69] // Z + aba | catalytic destruction: b + aba <-> Z + aba
            6.19508147977697 * x.[6] * x.[69] // b + aba | catalytic destruction: b + aba <-> Z + aba
            -6.19508147977697 * x.[2] * x.[27] // Z + ABA | catalytic destruction: B + ABA <-> Z + ABA
            6.19508147977697 * x.[4] * x.[27] // B + ABA | catalytic destruction: B + ABA <-> Z + ABA
            -2.51062372870326 * x.[2] * x.[35] // Z + AbA | catalytic destruction: A + AbA <-> Z + AbA
            2.51062372870326 * x.[3] * x.[35] // A + AbA | catalytic destruction: A + AbA <-> Z + AbA
            -2.51062372870326 * x.[2] * x.[61] // Z + aBa | catalytic destruction: a + aBa <-> Z + aBa
            2.51062372870326 * x.[5] * x.[61] // a + aBa | catalytic destruction: a + aBa <-> Z + aBa
            -97.914325419427 * x.[2] * x.[61] // Z + aBa | catalytic destruction: A + aBa <-> Z + aBa
            97.914325419427 * x.[3] * x.[61] // A + aBa | catalytic destruction: A + aBa <-> Z + aBa
            -97.914325419427 * x.[2] * x.[35] // Z + AbA | catalytic destruction: a + AbA <-> Z + AbA
            97.914325419427 * x.[5] * x.[35] // a + AbA | catalytic destruction: a + AbA <-> Z + AbA
            -7.93120107498361 * x.[2] * x.[86] // Z + bbb | catalytic destruction: A + bbb <-> Z + bbb
            309.316841924361 * x.[3] * x.[86] // A + bbb | catalytic destruction: A + bbb <-> Z + bbb
            -7.93120107498361 * x.[2] * x.[44] // Z + BBB | catalytic destruction: a + BBB <-> Z + BBB
            309.316841924361 * x.[5] * x.[44] // a + BBB | catalytic destruction: a + BBB <-> Z + BBB
            -309.316841924361 * x.[2] * x.[44] // Z + BBB | catalytic destruction: A + BBB <-> Z + BBB
            7.93120107498361 * x.[3] * x.[44] // A + BBB | catalytic destruction: A + BBB <-> Z + BBB
            -309.316841924361 * x.[2] * x.[86] // Z + bbb | catalytic destruction: a + bbb <-> Z + bbb
            7.93120107498361 * x.[5] * x.[86] // a + bbb | catalytic destruction: a + bbb <-> Z + bbb
            -1.8415930347959 * x.[2] * x.[39] // Z + BAA | catalytic destruction: b + BAA <-> Z + BAA
            1.8415930347959 * x.[6] * x.[39] // b + BAA | catalytic destruction: b + BAA <-> Z + BAA
            -1.8415930347959 * x.[2] * x.[81] // Z + baa | catalytic destruction: B + baa <-> Z + baa
            1.8415930347959 * x.[4] * x.[81] // B + baa | catalytic destruction: B + baa <-> Z + baa
            -71.82212835704 * x.[2] * x.[81] // Z + baa | catalytic destruction: b + baa <-> Z + baa
            71.82212835704 * x.[6] * x.[81] // b + baa | catalytic destruction: b + baa <-> Z + baa
            -71.82212835704 * x.[2] * x.[39] // Z + BAA | catalytic destruction: B + BAA <-> Z + BAA
            71.82212835704 * x.[4] * x.[39] // B + BAA | catalytic destruction: B + BAA <-> Z + BAA
            -3.64996769844882 * x.[2] * x.[78] // Z + bBb | catalytic destruction: A + bBb <-> Z + bBb
            3.64996769844882 * x.[3] * x.[78] // A + bBb | catalytic destruction: A + bBb <-> Z + bBb
            -3.64996769844882 * x.[2] * x.[52] // Z + BbB | catalytic destruction: a + BbB <-> Z + BbB
            3.64996769844882 * x.[5] * x.[52] // a + BbB | catalytic destruction: a + BbB <-> Z + BbB
            -142.348740239504 * x.[2] * x.[52] // Z + BbB | catalytic destruction: A + BbB <-> Z + BbB
            142.348740239504 * x.[3] * x.[52] // A + BbB | catalytic destruction: A + BbB <-> Z + BbB
            -142.348740239504 * x.[2] * x.[78] // Z + bBb | catalytic destruction: a + bBb <-> Z + bBb
            142.348740239504 * x.[5] * x.[78] // a + bBb | catalytic destruction: a + bBb <-> Z + bBb
            -179.661256131781 * x.[2] * x.[36] // Z + AbB | catalytic destruction: B + AbB <-> Z + AbB
            179.661256131781 * x.[4] * x.[36] // B + AbB | catalytic destruction: B + AbB <-> Z + AbB
            -179.661256131781 * x.[2] * x.[62] // Z + aBb | catalytic destruction: b + aBb <-> Z + aBb
            179.661256131781 * x.[6] * x.[62] // b + aBb | catalytic destruction: b + aBb <-> Z + aBb
            -4.60669887517388 * x.[2] * x.[62] // Z + aBb | catalytic destruction: B + aBb <-> Z + aBb
            4.60669887517388 * x.[4] * x.[62] // B + aBb | catalytic destruction: B + aBb <-> Z + aBb
            -4.60669887517388 * x.[2] * x.[36] // Z + AbB | catalytic destruction: b + AbB <-> Z + AbB
            4.60669887517388 * x.[6] * x.[36] // b + AbB | catalytic destruction: b + AbB <-> Z + AbB
            -0.386872375676463 * x.[2] * x.[53] // Z + Bba | catalytic destruction: a + Bba <-> Z + Bba
            0.386872375676463 * x.[5] * x.[53] // a + Bba | catalytic destruction: a + Bba <-> Z + Bba
            -0.386872375676463 * x.[2] * x.[75] // Z + bBA | catalytic destruction: A + bBA <-> Z + bBA
            0.386872375676463 * x.[3] * x.[75] // A + bBA | catalytic destruction: A + bBA <-> Z + bBA
            -15.0880226513821 * x.[2] * x.[75] // Z + bBA | catalytic destruction: a + bBA <-> Z + bBA
            15.0880226513821 * x.[5] * x.[75] // a + bBA | catalytic destruction: a + bBA <-> Z + bBA
            -15.0880226513821 * x.[2] * x.[53] // Z + Bba | catalytic destruction: A + Bba <-> Z + Bba
            15.0880226513821 * x.[3] * x.[53] // A + Bba | catalytic destruction: A + Bba <-> Z + Bba
            -6.6073542900291 * x.[2] * x.[70] // Z + abb | catalytic destruction: a + abb <-> Z + abb
            6.6073542900291 * x.[5] * x.[70] // a + abb | catalytic destruction: a + abb <-> Z + abb
            -6.6073542900291 * x.[2] * x.[28] // Z + ABB | catalytic destruction: A + ABB <-> Z + ABB
            6.6073542900291 * x.[3] * x.[28] // A + ABB | catalytic destruction: A + ABB <-> Z + ABB
            -257.686817311135 * x.[2] * x.[28] // Z + ABB | catalytic destruction: a + ABB <-> Z + ABB
            257.686817311135 * x.[5] * x.[28] // a + ABB | catalytic destruction: a + ABB <-> Z + ABB
            -257.686817311135 * x.[2] * x.[70] // Z + abb | catalytic destruction: A + abb <-> Z + abb
            257.686817311135 * x.[3] * x.[70] // A + abb | catalytic destruction: A + abb <-> Z + abb
            -1.63754687355549 * x.[2] * x.[63] // Z + aaA | catalytic destruction: b + aaA <-> Z + aaA
            63.8643280686642 * x.[6] * x.[63] // b + aaA | catalytic destruction: b + aaA <-> Z + aaA
            -1.63754687355549 * x.[2] * x.[25] // Z + AAa | catalytic destruction: B + AAa <-> Z + AAa
            63.8643280686642 * x.[4] * x.[25] // B + AAa | catalytic destruction: B + AAa <-> Z + AAa
            -63.8643280686642 * x.[2] * x.[25] // Z + AAa | catalytic destruction: b + AAa <-> Z + AAa
            1.63754687355549 * x.[6] * x.[25] // b + AAa | catalytic destruction: b + AAa <-> Z + AAa
            -63.8643280686642 * x.[2] * x.[63] // Z + aaA | catalytic destruction: B + aaA <-> Z + aaA
            1.63754687355549 * x.[4] * x.[63] // B + aaA | catalytic destruction: B + aaA <-> Z + aaA
            -1.63754687355549 * x.[2] * x.[63] // Z + aaA | catalytic destruction: a + aaA <-> Z + aaA
            63.8643280686642 * x.[5] * x.[63] // a + aaA | catalytic destruction: a + aaA <-> Z + aaA
            -1.63754687355549 * x.[2] * x.[25] // Z + AAa | catalytic destruction: A + AAa <-> Z + AAa
            63.8643280686642 * x.[3] * x.[25] // A + AAa | catalytic destruction: A + AAa <-> Z + AAa
            -63.8643280686642 * x.[2] * x.[25] // Z + AAa | catalytic destruction: a + AAa <-> Z + AAa
            1.63754687355549 * x.[5] * x.[25] // a + AAa | catalytic destruction: a + AAa <-> Z + AAa
            -63.8643280686642 * x.[2] * x.[63] // Z + aaA | catalytic destruction: A + aaA <-> Z + aaA
            1.63754687355549 * x.[3] * x.[63] // A + aaA | catalytic destruction: A + aaA <-> Z + aaA
            -9.39104128484456 * x.[2] * x.[38] // Z + Abb | catalytic destruction: A + Abb <-> Z + Abb
            366.250610108938 * x.[3] * x.[38] // A + Abb | catalytic destruction: A + Abb <-> Z + Abb
            -9.39104128484456 * x.[2] * x.[60] // Z + aBB | catalytic destruction: a + aBB <-> Z + aBB
            366.250610108938 * x.[5] * x.[60] // a + aBB | catalytic destruction: a + aBB <-> Z + aBB
            -366.250610108938 * x.[2] * x.[60] // Z + aBB | catalytic destruction: A + aBB <-> Z + aBB
            9.39104128484456 * x.[3] * x.[60] // A + aBB | catalytic destruction: A + aBB <-> Z + aBB
            -366.250610108938 * x.[2] * x.[38] // Z + Abb | catalytic destruction: a + Abb <-> Z + Abb
            9.39104128484456 * x.[5] * x.[38] // a + Abb | catalytic destruction: a + Abb <-> Z + Abb
            -310.746203594023 * x.[2] * x.[30] // Z + ABb | catalytic destruction: B + ABb <-> Z + ABb
            7.96785137420571 * x.[4] * x.[30] // B + ABb | catalytic destruction: B + ABb <-> Z + ABb
            -310.746203594023 * x.[2] * x.[68] // Z + abB | catalytic destruction: b + abB <-> Z + abB
            7.96785137420571 * x.[6] * x.[68] // b + abB | catalytic destruction: b + abB <-> Z + abB
            -7.96785137420571 * x.[2] * x.[68] // Z + abB | catalytic destruction: B + abB <-> Z + abB
            310.746203594023 * x.[4] * x.[68] // B + abB | catalytic destruction: B + abB <-> Z + abB
            -7.96785137420571 * x.[2] * x.[30] // Z + ABb | catalytic destruction: b + ABb <-> Z + ABb
            310.746203594023 * x.[6] * x.[30] // b + ABb | catalytic destruction: b + ABb <-> Z + ABb
            -0.748433322463468 * x.[2] * x.[50] // Z + Bab | catalytic destruction: a + Bab <-> Z + Bab
            0.748433322463468 * x.[5] * x.[50] // a + Bab | catalytic destruction: a + Bab <-> Z + Bab
            -0.748433322463468 * x.[2] * x.[72] // Z + bAB | catalytic destruction: A + bAB <-> Z + bAB
            0.748433322463468 * x.[3] * x.[72] // A + bAB | catalytic destruction: A + bAB <-> Z + bAB
            -29.1888995760752 * x.[2] * x.[72] // Z + bAB | catalytic destruction: a + bAB <-> Z + bAB
            29.1888995760752 * x.[5] * x.[72] // a + bAB | catalytic destruction: a + bAB <-> Z + bAB
            -29.1888995760752 * x.[2] * x.[50] // Z + Bab | catalytic destruction: A + Bab <-> Z + Bab
            29.1888995760752 * x.[3] * x.[50] // A + Bab | catalytic destruction: A + Bab <-> Z + Bab
            -300.810127753095 * x.[2] * x.[32] // Z + AaB | catalytic destruction: a + AaB <-> Z + AaB
            7.71308019879732 * x.[5] * x.[32] // a + AaB | catalytic destruction: a + AaB <-> Z + AaB
            -300.810127753095 * x.[2] * x.[58] // Z + aAb | catalytic destruction: A + aAb <-> Z + aAb
            7.71308019879732 * x.[3] * x.[58] // A + aAb | catalytic destruction: A + aAb <-> Z + aAb
            -7.71308019879732 * x.[2] * x.[58] // Z + aAb | catalytic destruction: a + aAb <-> Z + aAb
            300.810127753095 * x.[5] * x.[58] // a + aAb | catalytic destruction: a + aAb <-> Z + aAb
            -7.71308019879732 * x.[2] * x.[32] // Z + AaB | catalytic destruction: A + AaB <-> Z + AaB
            300.810127753095 * x.[3] * x.[32] // A + AaB | catalytic destruction: A + AaB <-> Z + AaB
            -8.21385688864 * x.[2] * x.[37] // Z + Aba | catalytic destruction: b + Aba <-> Z + Aba
            320.34041865696 * x.[6] * x.[37] // b + Aba | catalytic destruction: b + Aba <-> Z + Aba
            -8.21385688864 * x.[2] * x.[59] // Z + aBA | catalytic destruction: B + aBA <-> Z + aBA
            320.34041865696 * x.[4] * x.[59] // B + aBA | catalytic destruction: B + aBA <-> Z + aBA
            -320.34041865696 * x.[2] * x.[59] // Z + aBA | catalytic destruction: b + aBA <-> Z + aBA
            8.21385688864 * x.[6] * x.[59] // b + aBA | catalytic destruction: b + aBA <-> Z + aBA
            -320.34041865696 * x.[2] * x.[37] // Z + Aba | catalytic destruction: B + Aba <-> Z + Aba
            8.21385688864 * x.[4] * x.[37] // B + Aba | catalytic destruction: B + Aba <-> Z + Aba
            -140.185438995508 * x.[2] * x.[74] // Z + bAb | catalytic destruction: B + bAb <-> Z + bAb
            3.59449843578226 * x.[4] * x.[74] // B + bAb | catalytic destruction: B + bAb <-> Z + bAb
            -140.185438995508 * x.[2] * x.[48] // Z + BaB | catalytic destruction: b + BaB <-> Z + BaB
            3.59449843578226 * x.[6] * x.[48] // b + BaB | catalytic destruction: b + BaB <-> Z + BaB
            -3.59449843578226 * x.[2] * x.[48] // Z + BaB | catalytic destruction: B + BaB <-> Z + BaB
            140.185438995508 * x.[4] * x.[48] // B + BaB | catalytic destruction: B + BaB <-> Z + BaB
            -3.59449843578226 * x.[2] * x.[74] // Z + bAb | catalytic destruction: b + bAb <-> Z + bAb
            140.185438995508 * x.[6] * x.[74] // b + bAb | catalytic destruction: b + bAb <-> Z + bAb
            -9.30540130093183 * x.[2] * x.[80] // Z + baB | catalytic destruction: b + baB <-> Z + baB
            9.30540130093183 * x.[6] * x.[80] // b + baB | catalytic destruction: b + baB <-> Z + baB
            -9.30540130093183 * x.[2] * x.[42] // Z + BAb | catalytic destruction: B + BAb <-> Z + BAb
            9.30540130093183 * x.[4] * x.[42] // B + BAb | catalytic destruction: B + BAb <-> Z + BAb
            -362.910650736341 * x.[2] * x.[42] // Z + BAb | catalytic destruction: b + BAb <-> Z + BAb
            362.910650736341 * x.[6] * x.[42] // b + BAb | catalytic destruction: b + BAb <-> Z + BAb
            -362.910650736341 * x.[2] * x.[80] // Z + baB | catalytic destruction: B + baB <-> Z + baB
            362.910650736341 * x.[4] * x.[80] // B + baB | catalytic destruction: B + baB <-> Z + baB
            -6.70419275442211 * x.[2] * x.[67] // Z + abA | catalytic destruction: b + abA <-> Z + abA
            6.70419275442211 * x.[6] * x.[67] // b + abA | catalytic destruction: b + abA <-> Z + abA
            -6.70419275442211 * x.[2] * x.[29] // Z + ABa | catalytic destruction: B + ABa <-> Z + ABa
            6.70419275442211 * x.[4] * x.[29] // B + ABa | catalytic destruction: B + ABa <-> Z + ABa
            -261.463517422462 * x.[2] * x.[29] // Z + ABa | catalytic destruction: b + ABa <-> Z + ABa
            261.463517422462 * x.[6] * x.[29] // b + ABa | catalytic destruction: b + ABa <-> Z + ABa
            -261.463517422462 * x.[2] * x.[67] // Z + abA | catalytic destruction: B + abA <-> Z + abA
            261.463517422462 * x.[4] * x.[67] // B + abA | catalytic destruction: B + abA <-> Z + abA
            -5.26751700365569 * x.[2] * x.[24] // Z + AAB | catalytic destruction: a + AAB <-> Z + AAB
            205.433163142572 * x.[5] * x.[24] // a + AAB | catalytic destruction: a + AAB <-> Z + AAB
            -5.26751700365569 * x.[2] * x.[66] // Z + aab | catalytic destruction: A + aab <-> Z + aab
            205.433163142572 * x.[3] * x.[66] // A + aab | catalytic destruction: A + aab <-> Z + aab
            -205.433163142572 * x.[2] * x.[66] // Z + aab | catalytic destruction: a + aab <-> Z + aab
            5.26751700365569 * x.[5] * x.[66] // a + aab | catalytic destruction: a + aab <-> Z + aab
            -205.433163142572 * x.[2] * x.[24] // Z + AAB | catalytic destruction: A + AAB <-> Z + AAB
            5.26751700365569 * x.[3] * x.[24] // A + AAB | catalytic destruction: A + AAB <-> Z + AAB
            -5.26751700365569 * x.[2] * x.[24] // Z + AAB | catalytic destruction: b + AAB <-> Z + AAB
            205.433163142572 * x.[6] * x.[24] // b + AAB | catalytic destruction: b + AAB <-> Z + AAB
            -5.26751700365569 * x.[2] * x.[66] // Z + aab | catalytic destruction: B + aab <-> Z + aab
            205.433163142572 * x.[4] * x.[66] // B + aab | catalytic destruction: B + aab <-> Z + aab
            -205.433163142572 * x.[2] * x.[66] // Z + aab | catalytic destruction: b + aab <-> Z + aab
            5.26751700365569 * x.[6] * x.[66] // b + aab | catalytic destruction: b + aab <-> Z + aab
            -205.433163142572 * x.[2] * x.[24] // Z + AAB | catalytic destruction: B + AAB <-> Z + AAB
            5.26751700365569 * x.[4] * x.[24] // B + AAB | catalytic destruction: B + AAB <-> Z + AAB
            -0.001 * x.[2] // Z | destruction: b <-> Z
            0.001 * x.[6] // b | destruction: b <-> Z
            -0.001 * x.[2] // Z | destruction: B <-> Z
            0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[2] // Z | destruction: a <-> Z
            0.001 * x.[5] // a | destruction: a <-> Z
            -0.001 * x.[2] // Z | destruction: A <-> Z
            0.001 * x.[3] // A | destruction: A <-> Z
            -0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[19] // bA | ligation: b + A <-> bA
            -1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
            1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
            1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
            1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
            1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
            1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
            1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
            1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
            1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
            1.0 * x.[15] // aA | ligation: a + A <-> aA
            -1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
            1.0 * x.[8] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[7] // AA | ligation: A + A <-> AA
            1.0 * x.[7] // AA | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            13.426527118448 * x.[2] * x.[85] // Z + bba | catalytic destruction: A + bba <-> Z + bba
            -523.63455761947 * x.[3] * x.[85] // A + bba | catalytic destruction: A + bba <-> Z + bba
            523.63455761947 * x.[2] * x.[43] // Z + BBA | catalytic destruction: A + BBA <-> Z + BBA
            -13.426527118448 * x.[3] * x.[43] // A + BBA | catalytic destruction: A + BBA <-> Z + BBA
            3.39498740258425 * x.[2] * x.[84] // Z + bbB | catalytic destruction: A + bbB <-> Z + bbB
            -132.404508700785 * x.[3] * x.[84] // A + bbB | catalytic destruction: A + bbB <-> Z + bbB
            132.404508700785 * x.[2] * x.[46] // Z + BBb | catalytic destruction: A + BBb <-> Z + BBb
            -3.39498740258425 * x.[3] * x.[46] // A + BBb | catalytic destruction: A + BBb <-> Z + BBb
            0.58043074863127 * x.[2] * x.[77] // Z + bBa | catalytic destruction: A + bBa <-> Z + bBa
            -22.6367991966195 * x.[3] * x.[77] // A + bBa | catalytic destruction: A + bBa <-> Z + bBa
            22.6367991966195 * x.[2] * x.[51] // Z + BbA | catalytic destruction: A + BbA <-> Z + BbA
            -0.58043074863127 * x.[3] * x.[51] // A + BbA | catalytic destruction: A + BbA <-> Z + BbA
            23.3162479818138 * x.[2] * x.[31] // Z + AaA | catalytic destruction: A + AaA <-> Z + AaA
            -23.3162479818138 * x.[3] * x.[31] // A + AaA | catalytic destruction: A + AaA <-> Z + AaA
            0.597852512354202 * x.[2] * x.[57] // Z + aAa | catalytic destruction: A + aAa <-> Z + aAa
            -0.597852512354202 * x.[3] * x.[57] // A + aAa | catalytic destruction: A + aAa <-> Z + aAa
            282.906972423916 * x.[2] * x.[34] // Z + Aab | catalytic destruction: A + Aab <-> Z + Aab
            -7.25402493394657 * x.[3] * x.[34] // A + Aab | catalytic destruction: A + Aab <-> Z + Aab
            7.25402493394657 * x.[2] * x.[56] // Z + aAB | catalytic destruction: A + aAB <-> Z + aAB
            -282.906972423916 * x.[3] * x.[56] // A + aAB | catalytic destruction: A + aAB <-> Z + aAB
            7.63579471687961 * x.[2] * x.[73] // Z + bAa | catalytic destruction: A + bAa <-> Z + bAa
            -297.795993958305 * x.[3] * x.[73] // A + bAa | catalytic destruction: A + bAa <-> Z + bAa
            297.795993958305 * x.[2] * x.[47] // Z + BaA | catalytic destruction: A + BaA <-> Z + BaA
            -7.63579471687961 * x.[3] * x.[47] // A + BaA | catalytic destruction: A + BaA <-> Z + BaA
            2.51062372870326 * x.[2] * x.[35] // Z + AbA | catalytic destruction: A + AbA <-> Z + AbA
            -2.51062372870326 * x.[3] * x.[35] // A + AbA | catalytic destruction: A + AbA <-> Z + AbA
            97.914325419427 * x.[2] * x.[61] // Z + aBa | catalytic destruction: A + aBa <-> Z + aBa
            -97.914325419427 * x.[3] * x.[61] // A + aBa | catalytic destruction: A + aBa <-> Z + aBa
            7.93120107498361 * x.[2] * x.[86] // Z + bbb | catalytic destruction: A + bbb <-> Z + bbb
            -309.316841924361 * x.[3] * x.[86] // A + bbb | catalytic destruction: A + bbb <-> Z + bbb
            309.316841924361 * x.[2] * x.[44] // Z + BBB | catalytic destruction: A + BBB <-> Z + BBB
            -7.93120107498361 * x.[3] * x.[44] // A + BBB | catalytic destruction: A + BBB <-> Z + BBB
            3.64996769844882 * x.[2] * x.[78] // Z + bBb | catalytic destruction: A + bBb <-> Z + bBb
            -3.64996769844882 * x.[3] * x.[78] // A + bBb | catalytic destruction: A + bBb <-> Z + bBb
            142.348740239504 * x.[2] * x.[52] // Z + BbB | catalytic destruction: A + BbB <-> Z + BbB
            -142.348740239504 * x.[3] * x.[52] // A + BbB | catalytic destruction: A + BbB <-> Z + BbB
            0.386872375676463 * x.[2] * x.[75] // Z + bBA | catalytic destruction: A + bBA <-> Z + bBA
            -0.386872375676463 * x.[3] * x.[75] // A + bBA | catalytic destruction: A + bBA <-> Z + bBA
            15.0880226513821 * x.[2] * x.[53] // Z + Bba | catalytic destruction: A + Bba <-> Z + Bba
            -15.0880226513821 * x.[3] * x.[53] // A + Bba | catalytic destruction: A + Bba <-> Z + Bba
            6.6073542900291 * x.[2] * x.[28] // Z + ABB | catalytic destruction: A + ABB <-> Z + ABB
            -6.6073542900291 * x.[3] * x.[28] // A + ABB | catalytic destruction: A + ABB <-> Z + ABB
            257.686817311135 * x.[2] * x.[70] // Z + abb | catalytic destruction: A + abb <-> Z + abb
            -257.686817311135 * x.[3] * x.[70] // A + abb | catalytic destruction: A + abb <-> Z + abb
            1.63754687355549 * x.[2] * x.[25] // Z + AAa | catalytic destruction: A + AAa <-> Z + AAa
            -63.8643280686642 * x.[3] * x.[25] // A + AAa | catalytic destruction: A + AAa <-> Z + AAa
            63.8643280686642 * x.[2] * x.[63] // Z + aaA | catalytic destruction: A + aaA <-> Z + aaA
            -1.63754687355549 * x.[3] * x.[63] // A + aaA | catalytic destruction: A + aaA <-> Z + aaA
            9.39104128484456 * x.[2] * x.[38] // Z + Abb | catalytic destruction: A + Abb <-> Z + Abb
            -366.250610108938 * x.[3] * x.[38] // A + Abb | catalytic destruction: A + Abb <-> Z + Abb
            366.250610108938 * x.[2] * x.[60] // Z + aBB | catalytic destruction: A + aBB <-> Z + aBB
            -9.39104128484456 * x.[3] * x.[60] // A + aBB | catalytic destruction: A + aBB <-> Z + aBB
            0.748433322463468 * x.[2] * x.[72] // Z + bAB | catalytic destruction: A + bAB <-> Z + bAB
            -0.748433322463468 * x.[3] * x.[72] // A + bAB | catalytic destruction: A + bAB <-> Z + bAB
            29.1888995760752 * x.[2] * x.[50] // Z + Bab | catalytic destruction: A + Bab <-> Z + Bab
            -29.1888995760752 * x.[3] * x.[50] // A + Bab | catalytic destruction: A + Bab <-> Z + Bab
            300.810127753095 * x.[2] * x.[58] // Z + aAb | catalytic destruction: A + aAb <-> Z + aAb
            -7.71308019879732 * x.[3] * x.[58] // A + aAb | catalytic destruction: A + aAb <-> Z + aAb
            7.71308019879732 * x.[2] * x.[32] // Z + AaB | catalytic destruction: A + AaB <-> Z + AaB
            -300.810127753095 * x.[3] * x.[32] // A + AaB | catalytic destruction: A + AaB <-> Z + AaB
            5.26751700365569 * x.[2] * x.[66] // Z + aab | catalytic destruction: A + aab <-> Z + aab
            -205.433163142572 * x.[3] * x.[66] // A + aab | catalytic destruction: A + aab <-> Z + aab
            205.433163142572 * x.[2] * x.[24] // Z + AAB | catalytic destruction: A + AAB <-> Z + AAB
            -5.26751700365569 * x.[3] * x.[24] // A + AAB | catalytic destruction: A + AAB <-> Z + AAB
            0.001 * x.[2] // Z | destruction: A <-> Z
            -0.001 * x.[3] // A | destruction: A <-> Z
            -0.001 * x.[3] // A | synthesis: Y <-> A
            0.001 * x.[1] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 4 - B
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
            1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[20] // bB | ligation: b + B <-> bB
            -1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[12] // BB | ligation: B + B <-> BB
            1.0 * x.[12] // BB | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[16] // aB | ligation: a + B <-> aB
            -1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[8] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            124.52147491494 * x.[2] * x.[79] // Z + baA | catalytic destruction: B + baA <-> Z + baA
            -3.19285833115232 * x.[4] * x.[79] // B + baA | catalytic destruction: B + baA <-> Z + baA
            3.19285833115232 * x.[2] * x.[41] // Z + BAa | catalytic destruction: B + BAa <-> Z + BAa
            -124.52147491494 * x.[4] * x.[41] // B + BAa | catalytic destruction: B + BAa <-> Z + BAa
            6.29040747236337 * x.[2] * x.[54] // Z + Bbb | catalytic destruction: B + Bbb <-> Z + Bbb
            -245.325891422171 * x.[4] * x.[54] // B + Bbb | catalytic destruction: B + Bbb <-> Z + Bbb
            245.325891422171 * x.[2] * x.[76] // Z + bBB | catalytic destruction: B + bBB <-> Z + bBB
            -6.29040747236337 * x.[4] * x.[76] // B + bBB | catalytic destruction: B + bBB <-> Z + bBB
            23.3162479818138 * x.[2] * x.[31] // Z + AaA | catalytic destruction: B + AaA <-> Z + AaA
            -23.3162479818138 * x.[4] * x.[31] // B + AaA | catalytic destruction: B + AaA <-> Z + AaA
            0.597852512354202 * x.[2] * x.[57] // Z + aAa | catalytic destruction: B + aAa <-> Z + aAa
            -0.597852512354202 * x.[4] * x.[57] // B + aAa | catalytic destruction: B + aAa <-> Z + aAa
            3.62732552426672 * x.[2] * x.[45] // Z + BBa | catalytic destruction: B + BBa <-> Z + BBa
            -3.62732552426672 * x.[4] * x.[45] // B + BBa | catalytic destruction: B + BBa <-> Z + BBa
            141.465695446402 * x.[2] * x.[83] // Z + bbA | catalytic destruction: B + bbA <-> Z + bbA
            -141.465695446402 * x.[4] * x.[83] // B + bbA | catalytic destruction: B + bbA <-> Z + bbA
            7.63579471687961 * x.[2] * x.[73] // Z + bAa | catalytic destruction: B + bAa <-> Z + bAa
            -297.795993958305 * x.[4] * x.[73] // B + bAa | catalytic destruction: B + bAa <-> Z + bAa
            297.795993958305 * x.[2] * x.[47] // Z + BaA | catalytic destruction: B + BaA <-> Z + BaA
            -7.63579471687961 * x.[4] * x.[47] // B + BaA | catalytic destruction: B + BaA <-> Z + BaA
            241.608177711302 * x.[2] * x.[69] // Z + aba | catalytic destruction: B + aba <-> Z + aba
            -241.608177711302 * x.[4] * x.[69] // B + aba | catalytic destruction: B + aba <-> Z + aba
            6.19508147977697 * x.[2] * x.[27] // Z + ABA | catalytic destruction: B + ABA <-> Z + ABA
            -6.19508147977697 * x.[4] * x.[27] // B + ABA | catalytic destruction: B + ABA <-> Z + ABA
            1.8415930347959 * x.[2] * x.[81] // Z + baa | catalytic destruction: B + baa <-> Z + baa
            -1.8415930347959 * x.[4] * x.[81] // B + baa | catalytic destruction: B + baa <-> Z + baa
            71.82212835704 * x.[2] * x.[39] // Z + BAA | catalytic destruction: B + BAA <-> Z + BAA
            -71.82212835704 * x.[4] * x.[39] // B + BAA | catalytic destruction: B + BAA <-> Z + BAA
            179.661256131781 * x.[2] * x.[36] // Z + AbB | catalytic destruction: B + AbB <-> Z + AbB
            -179.661256131781 * x.[4] * x.[36] // B + AbB | catalytic destruction: B + AbB <-> Z + AbB
            4.60669887517388 * x.[2] * x.[62] // Z + aBb | catalytic destruction: B + aBb <-> Z + aBb
            -4.60669887517388 * x.[4] * x.[62] // B + aBb | catalytic destruction: B + aBb <-> Z + aBb
            1.63754687355549 * x.[2] * x.[25] // Z + AAa | catalytic destruction: B + AAa <-> Z + AAa
            -63.8643280686642 * x.[4] * x.[25] // B + AAa | catalytic destruction: B + AAa <-> Z + AAa
            63.8643280686642 * x.[2] * x.[63] // Z + aaA | catalytic destruction: B + aaA <-> Z + aaA
            -1.63754687355549 * x.[4] * x.[63] // B + aaA | catalytic destruction: B + aaA <-> Z + aaA
            310.746203594023 * x.[2] * x.[30] // Z + ABb | catalytic destruction: B + ABb <-> Z + ABb
            -7.96785137420571 * x.[4] * x.[30] // B + ABb | catalytic destruction: B + ABb <-> Z + ABb
            7.96785137420571 * x.[2] * x.[68] // Z + abB | catalytic destruction: B + abB <-> Z + abB
            -310.746203594023 * x.[4] * x.[68] // B + abB | catalytic destruction: B + abB <-> Z + abB
            8.21385688864 * x.[2] * x.[59] // Z + aBA | catalytic destruction: B + aBA <-> Z + aBA
            -320.34041865696 * x.[4] * x.[59] // B + aBA | catalytic destruction: B + aBA <-> Z + aBA
            320.34041865696 * x.[2] * x.[37] // Z + Aba | catalytic destruction: B + Aba <-> Z + Aba
            -8.21385688864 * x.[4] * x.[37] // B + Aba | catalytic destruction: B + Aba <-> Z + Aba
            140.185438995508 * x.[2] * x.[74] // Z + bAb | catalytic destruction: B + bAb <-> Z + bAb
            -3.59449843578226 * x.[4] * x.[74] // B + bAb | catalytic destruction: B + bAb <-> Z + bAb
            3.59449843578226 * x.[2] * x.[48] // Z + BaB | catalytic destruction: B + BaB <-> Z + BaB
            -140.185438995508 * x.[4] * x.[48] // B + BaB | catalytic destruction: B + BaB <-> Z + BaB
            9.30540130093183 * x.[2] * x.[42] // Z + BAb | catalytic destruction: B + BAb <-> Z + BAb
            -9.30540130093183 * x.[4] * x.[42] // B + BAb | catalytic destruction: B + BAb <-> Z + BAb
            362.910650736341 * x.[2] * x.[80] // Z + baB | catalytic destruction: B + baB <-> Z + baB
            -362.910650736341 * x.[4] * x.[80] // B + baB | catalytic destruction: B + baB <-> Z + baB
            6.70419275442211 * x.[2] * x.[29] // Z + ABa | catalytic destruction: B + ABa <-> Z + ABa
            -6.70419275442211 * x.[4] * x.[29] // B + ABa | catalytic destruction: B + ABa <-> Z + ABa
            261.463517422462 * x.[2] * x.[67] // Z + abA | catalytic destruction: B + abA <-> Z + abA
            -261.463517422462 * x.[4] * x.[67] // B + abA | catalytic destruction: B + abA <-> Z + abA
            5.26751700365569 * x.[2] * x.[66] // Z + aab | catalytic destruction: B + aab <-> Z + aab
            -205.433163142572 * x.[4] * x.[66] // B + aab | catalytic destruction: B + aab <-> Z + aab
            205.433163142572 * x.[2] * x.[24] // Z + AAB | catalytic destruction: B + AAB <-> Z + AAB
            -5.26751700365569 * x.[4] * x.[24] // B + AAB | catalytic destruction: B + AAB <-> Z + AAB
            0.001 * x.[2] // Z | destruction: B <-> Z
            -0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[4] // B | synthesis: Y <-> B
            0.001 * x.[1] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 5 - a
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[68] // abB | ligation: a + bB <-> abB
            -1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
            1.0 * x.[67] // abA | ligation: a + bA <-> abA
            -1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
            1.0 * x.[70] // abb | ligation: a + bb <-> abb
            -1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
            1.0 * x.[69] // aba | ligation: a + ba <-> aba
            -1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
            1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
            1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
            1.0 * x.[66] // aab | ligation: a + ab <-> aab
            -1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
            1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
            1.0 * x.[16] // aB | ligation: a + B <-> aB
            -1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[15] // aA | ligation: a + A <-> aA
            -1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
            1.0 * x.[18] // ab | ligation: a + b <-> ab
            -1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
            1.0 * x.[17] // aa | ligation: a + a <-> aa
            1.0 * x.[17] // aa | ligation: a + a <-> aa
            -1.0 * x.[5] * x.[5] // a + a | ligation: a + a <-> aa
            -1.0 * x.[5] * x.[5] // a + a | ligation: a + a <-> aa
            13.426527118448 * x.[2] * x.[43] // Z + BBA | catalytic destruction: a + BBA <-> Z + BBA
            -523.63455761947 * x.[5] * x.[43] // a + BBA | catalytic destruction: a + BBA <-> Z + BBA
            523.63455761947 * x.[2] * x.[85] // Z + bba | catalytic destruction: a + bba <-> Z + bba
            -13.426527118448 * x.[5] * x.[85] // a + bba | catalytic destruction: a + bba <-> Z + bba
            3.39498740258425 * x.[2] * x.[46] // Z + BBb | catalytic destruction: a + BBb <-> Z + BBb
            -132.404508700785 * x.[5] * x.[46] // a + BBb | catalytic destruction: a + BBb <-> Z + BBb
            132.404508700785 * x.[2] * x.[84] // Z + bbB | catalytic destruction: a + bbB <-> Z + bbB
            -3.39498740258425 * x.[5] * x.[84] // a + bbB | catalytic destruction: a + bbB <-> Z + bbB
            0.58043074863127 * x.[2] * x.[51] // Z + BbA | catalytic destruction: a + BbA <-> Z + BbA
            -22.6367991966195 * x.[5] * x.[51] // a + BbA | catalytic destruction: a + BbA <-> Z + BbA
            22.6367991966195 * x.[2] * x.[77] // Z + bBa | catalytic destruction: a + bBa <-> Z + bBa
            -0.58043074863127 * x.[5] * x.[77] // a + bBa | catalytic destruction: a + bBa <-> Z + bBa
            23.3162479818138 * x.[2] * x.[57] // Z + aAa | catalytic destruction: a + aAa <-> Z + aAa
            -23.3162479818138 * x.[5] * x.[57] // a + aAa | catalytic destruction: a + aAa <-> Z + aAa
            0.597852512354202 * x.[2] * x.[31] // Z + AaA | catalytic destruction: a + AaA <-> Z + AaA
            -0.597852512354202 * x.[5] * x.[31] // a + AaA | catalytic destruction: a + AaA <-> Z + AaA
            282.906972423916 * x.[2] * x.[56] // Z + aAB | catalytic destruction: a + aAB <-> Z + aAB
            -7.25402493394657 * x.[5] * x.[56] // a + aAB | catalytic destruction: a + aAB <-> Z + aAB
            7.25402493394657 * x.[2] * x.[34] // Z + Aab | catalytic destruction: a + Aab <-> Z + Aab
            -282.906972423916 * x.[5] * x.[34] // a + Aab | catalytic destruction: a + Aab <-> Z + Aab
            7.63579471687961 * x.[2] * x.[47] // Z + BaA | catalytic destruction: a + BaA <-> Z + BaA
            -297.795993958305 * x.[5] * x.[47] // a + BaA | catalytic destruction: a + BaA <-> Z + BaA
            297.795993958305 * x.[2] * x.[73] // Z + bAa | catalytic destruction: a + bAa <-> Z + bAa
            -7.63579471687961 * x.[5] * x.[73] // a + bAa | catalytic destruction: a + bAa <-> Z + bAa
            2.51062372870326 * x.[2] * x.[61] // Z + aBa | catalytic destruction: a + aBa <-> Z + aBa
            -2.51062372870326 * x.[5] * x.[61] // a + aBa | catalytic destruction: a + aBa <-> Z + aBa
            97.914325419427 * x.[2] * x.[35] // Z + AbA | catalytic destruction: a + AbA <-> Z + AbA
            -97.914325419427 * x.[5] * x.[35] // a + AbA | catalytic destruction: a + AbA <-> Z + AbA
            7.93120107498361 * x.[2] * x.[44] // Z + BBB | catalytic destruction: a + BBB <-> Z + BBB
            -309.316841924361 * x.[5] * x.[44] // a + BBB | catalytic destruction: a + BBB <-> Z + BBB
            309.316841924361 * x.[2] * x.[86] // Z + bbb | catalytic destruction: a + bbb <-> Z + bbb
            -7.93120107498361 * x.[5] * x.[86] // a + bbb | catalytic destruction: a + bbb <-> Z + bbb
            3.64996769844882 * x.[2] * x.[52] // Z + BbB | catalytic destruction: a + BbB <-> Z + BbB
            -3.64996769844882 * x.[5] * x.[52] // a + BbB | catalytic destruction: a + BbB <-> Z + BbB
            142.348740239504 * x.[2] * x.[78] // Z + bBb | catalytic destruction: a + bBb <-> Z + bBb
            -142.348740239504 * x.[5] * x.[78] // a + bBb | catalytic destruction: a + bBb <-> Z + bBb
            0.386872375676463 * x.[2] * x.[53] // Z + Bba | catalytic destruction: a + Bba <-> Z + Bba
            -0.386872375676463 * x.[5] * x.[53] // a + Bba | catalytic destruction: a + Bba <-> Z + Bba
            15.0880226513821 * x.[2] * x.[75] // Z + bBA | catalytic destruction: a + bBA <-> Z + bBA
            -15.0880226513821 * x.[5] * x.[75] // a + bBA | catalytic destruction: a + bBA <-> Z + bBA
            6.6073542900291 * x.[2] * x.[70] // Z + abb | catalytic destruction: a + abb <-> Z + abb
            -6.6073542900291 * x.[5] * x.[70] // a + abb | catalytic destruction: a + abb <-> Z + abb
            257.686817311135 * x.[2] * x.[28] // Z + ABB | catalytic destruction: a + ABB <-> Z + ABB
            -257.686817311135 * x.[5] * x.[28] // a + ABB | catalytic destruction: a + ABB <-> Z + ABB
            1.63754687355549 * x.[2] * x.[63] // Z + aaA | catalytic destruction: a + aaA <-> Z + aaA
            -63.8643280686642 * x.[5] * x.[63] // a + aaA | catalytic destruction: a + aaA <-> Z + aaA
            63.8643280686642 * x.[2] * x.[25] // Z + AAa | catalytic destruction: a + AAa <-> Z + AAa
            -1.63754687355549 * x.[5] * x.[25] // a + AAa | catalytic destruction: a + AAa <-> Z + AAa
            9.39104128484456 * x.[2] * x.[60] // Z + aBB | catalytic destruction: a + aBB <-> Z + aBB
            -366.250610108938 * x.[5] * x.[60] // a + aBB | catalytic destruction: a + aBB <-> Z + aBB
            366.250610108938 * x.[2] * x.[38] // Z + Abb | catalytic destruction: a + Abb <-> Z + Abb
            -9.39104128484456 * x.[5] * x.[38] // a + Abb | catalytic destruction: a + Abb <-> Z + Abb
            0.748433322463468 * x.[2] * x.[50] // Z + Bab | catalytic destruction: a + Bab <-> Z + Bab
            -0.748433322463468 * x.[5] * x.[50] // a + Bab | catalytic destruction: a + Bab <-> Z + Bab
            29.1888995760752 * x.[2] * x.[72] // Z + bAB | catalytic destruction: a + bAB <-> Z + bAB
            -29.1888995760752 * x.[5] * x.[72] // a + bAB | catalytic destruction: a + bAB <-> Z + bAB
            300.810127753095 * x.[2] * x.[32] // Z + AaB | catalytic destruction: a + AaB <-> Z + AaB
            -7.71308019879732 * x.[5] * x.[32] // a + AaB | catalytic destruction: a + AaB <-> Z + AaB
            7.71308019879732 * x.[2] * x.[58] // Z + aAb | catalytic destruction: a + aAb <-> Z + aAb
            -300.810127753095 * x.[5] * x.[58] // a + aAb | catalytic destruction: a + aAb <-> Z + aAb
            5.26751700365569 * x.[2] * x.[24] // Z + AAB | catalytic destruction: a + AAB <-> Z + AAB
            -205.433163142572 * x.[5] * x.[24] // a + AAB | catalytic destruction: a + AAB <-> Z + AAB
            205.433163142572 * x.[2] * x.[66] // Z + aab | catalytic destruction: a + aab <-> Z + aab
            -5.26751700365569 * x.[5] * x.[66] // a + aab | catalytic destruction: a + aab <-> Z + aab
            0.001 * x.[2] // Z | destruction: a <-> Z
            -0.001 * x.[5] // a | destruction: a <-> Z
            -0.001 * x.[5] // a | synthesis: Y <-> a
            0.001 * x.[1] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 6 - b
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[86] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[6] * x.[22] // b + bb | ligation: b + bb <-> bbb
            1.0 * x.[85] // bba | ligation: b + ba <-> bba
            -1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[80] // baB | ligation: b + aB <-> baB
            -1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[79] // baA | ligation: b + aA <-> baA
            -1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[82] // bab | ligation: b + ab <-> bab
            -1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[81] // baa | ligation: b + aa <-> baa
            -1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[20] // bB | ligation: b + B <-> bB
            -1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[19] // bA | ligation: b + A <-> bA
            -1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[22] // bb | ligation: b + b <-> bb
            1.0 * x.[22] // bb | ligation: b + b <-> bb
            -1.0 * x.[6] * x.[6] // b + b | ligation: b + b <-> bb
            -1.0 * x.[6] * x.[6] // b + b | ligation: b + b <-> bb
            1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
            1.0 * x.[18] // ab | ligation: a + b <-> ab
            -1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
            124.52147491494 * x.[2] * x.[41] // Z + BAa | catalytic destruction: b + BAa <-> Z + BAa
            -3.19285833115232 * x.[6] * x.[41] // b + BAa | catalytic destruction: b + BAa <-> Z + BAa
            3.19285833115232 * x.[2] * x.[79] // Z + baA | catalytic destruction: b + baA <-> Z + baA
            -124.52147491494 * x.[6] * x.[79] // b + baA | catalytic destruction: b + baA <-> Z + baA
            6.29040747236337 * x.[2] * x.[76] // Z + bBB | catalytic destruction: b + bBB <-> Z + bBB
            -245.325891422171 * x.[6] * x.[76] // b + bBB | catalytic destruction: b + bBB <-> Z + bBB
            245.325891422171 * x.[2] * x.[54] // Z + Bbb | catalytic destruction: b + Bbb <-> Z + Bbb
            -6.29040747236337 * x.[6] * x.[54] // b + Bbb | catalytic destruction: b + Bbb <-> Z + Bbb
            23.3162479818138 * x.[2] * x.[57] // Z + aAa | catalytic destruction: b + aAa <-> Z + aAa
            -23.3162479818138 * x.[6] * x.[57] // b + aAa | catalytic destruction: b + aAa <-> Z + aAa
            0.597852512354202 * x.[2] * x.[31] // Z + AaA | catalytic destruction: b + AaA <-> Z + AaA
            -0.597852512354202 * x.[6] * x.[31] // b + AaA | catalytic destruction: b + AaA <-> Z + AaA
            3.62732552426672 * x.[2] * x.[83] // Z + bbA | catalytic destruction: b + bbA <-> Z + bbA
            -3.62732552426672 * x.[6] * x.[83] // b + bbA | catalytic destruction: b + bbA <-> Z + bbA
            141.465695446402 * x.[2] * x.[45] // Z + BBa | catalytic destruction: b + BBa <-> Z + BBa
            -141.465695446402 * x.[6] * x.[45] // b + BBa | catalytic destruction: b + BBa <-> Z + BBa
            7.63579471687961 * x.[2] * x.[47] // Z + BaA | catalytic destruction: b + BaA <-> Z + BaA
            -297.795993958305 * x.[6] * x.[47] // b + BaA | catalytic destruction: b + BaA <-> Z + BaA
            297.795993958305 * x.[2] * x.[73] // Z + bAa | catalytic destruction: b + bAa <-> Z + bAa
            -7.63579471687961 * x.[6] * x.[73] // b + bAa | catalytic destruction: b + bAa <-> Z + bAa
            241.608177711302 * x.[2] * x.[27] // Z + ABA | catalytic destruction: b + ABA <-> Z + ABA
            -241.608177711302 * x.[6] * x.[27] // b + ABA | catalytic destruction: b + ABA <-> Z + ABA
            6.19508147977697 * x.[2] * x.[69] // Z + aba | catalytic destruction: b + aba <-> Z + aba
            -6.19508147977697 * x.[6] * x.[69] // b + aba | catalytic destruction: b + aba <-> Z + aba
            1.8415930347959 * x.[2] * x.[39] // Z + BAA | catalytic destruction: b + BAA <-> Z + BAA
            -1.8415930347959 * x.[6] * x.[39] // b + BAA | catalytic destruction: b + BAA <-> Z + BAA
            71.82212835704 * x.[2] * x.[81] // Z + baa | catalytic destruction: b + baa <-> Z + baa
            -71.82212835704 * x.[6] * x.[81] // b + baa | catalytic destruction: b + baa <-> Z + baa
            179.661256131781 * x.[2] * x.[62] // Z + aBb | catalytic destruction: b + aBb <-> Z + aBb
            -179.661256131781 * x.[6] * x.[62] // b + aBb | catalytic destruction: b + aBb <-> Z + aBb
            4.60669887517388 * x.[2] * x.[36] // Z + AbB | catalytic destruction: b + AbB <-> Z + AbB
            -4.60669887517388 * x.[6] * x.[36] // b + AbB | catalytic destruction: b + AbB <-> Z + AbB
            1.63754687355549 * x.[2] * x.[63] // Z + aaA | catalytic destruction: b + aaA <-> Z + aaA
            -63.8643280686642 * x.[6] * x.[63] // b + aaA | catalytic destruction: b + aaA <-> Z + aaA
            63.8643280686642 * x.[2] * x.[25] // Z + AAa | catalytic destruction: b + AAa <-> Z + AAa
            -1.63754687355549 * x.[6] * x.[25] // b + AAa | catalytic destruction: b + AAa <-> Z + AAa
            310.746203594023 * x.[2] * x.[68] // Z + abB | catalytic destruction: b + abB <-> Z + abB
            -7.96785137420571 * x.[6] * x.[68] // b + abB | catalytic destruction: b + abB <-> Z + abB
            7.96785137420571 * x.[2] * x.[30] // Z + ABb | catalytic destruction: b + ABb <-> Z + ABb
            -310.746203594023 * x.[6] * x.[30] // b + ABb | catalytic destruction: b + ABb <-> Z + ABb
            8.21385688864 * x.[2] * x.[37] // Z + Aba | catalytic destruction: b + Aba <-> Z + Aba
            -320.34041865696 * x.[6] * x.[37] // b + Aba | catalytic destruction: b + Aba <-> Z + Aba
            320.34041865696 * x.[2] * x.[59] // Z + aBA | catalytic destruction: b + aBA <-> Z + aBA
            -8.21385688864 * x.[6] * x.[59] // b + aBA | catalytic destruction: b + aBA <-> Z + aBA
            140.185438995508 * x.[2] * x.[48] // Z + BaB | catalytic destruction: b + BaB <-> Z + BaB
            -3.59449843578226 * x.[6] * x.[48] // b + BaB | catalytic destruction: b + BaB <-> Z + BaB
            3.59449843578226 * x.[2] * x.[74] // Z + bAb | catalytic destruction: b + bAb <-> Z + bAb
            -140.185438995508 * x.[6] * x.[74] // b + bAb | catalytic destruction: b + bAb <-> Z + bAb
            9.30540130093183 * x.[2] * x.[80] // Z + baB | catalytic destruction: b + baB <-> Z + baB
            -9.30540130093183 * x.[6] * x.[80] // b + baB | catalytic destruction: b + baB <-> Z + baB
            362.910650736341 * x.[2] * x.[42] // Z + BAb | catalytic destruction: b + BAb <-> Z + BAb
            -362.910650736341 * x.[6] * x.[42] // b + BAb | catalytic destruction: b + BAb <-> Z + BAb
            6.70419275442211 * x.[2] * x.[67] // Z + abA | catalytic destruction: b + abA <-> Z + abA
            -6.70419275442211 * x.[6] * x.[67] // b + abA | catalytic destruction: b + abA <-> Z + abA
            261.463517422462 * x.[2] * x.[29] // Z + ABa | catalytic destruction: b + ABa <-> Z + ABa
            -261.463517422462 * x.[6] * x.[29] // b + ABa | catalytic destruction: b + ABa <-> Z + ABa
            5.26751700365569 * x.[2] * x.[24] // Z + AAB | catalytic destruction: b + AAB <-> Z + AAB
            -205.433163142572 * x.[6] * x.[24] // b + AAB | catalytic destruction: b + AAB <-> Z + AAB
            205.433163142572 * x.[2] * x.[66] // Z + aab | catalytic destruction: b + aab <-> Z + aab
            -5.26751700365569 * x.[6] * x.[66] // b + aab | catalytic destruction: b + aab <-> Z + aab
            0.001 * x.[2] // Z | destruction: b <-> Z
            -0.001 * x.[6] // b | destruction: b <-> Z
            -0.001 * x.[6] // b | synthesis: Y <-> b
            0.001 * x.[1] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 7 - AA
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
            -1.0 * x.[7] // AA | ligation: A + A <-> AA
            1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 8 - AB
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
            -1.0 * x.[8] // AB | ligation: A + B <-> AB
            1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 9 - Aa
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
            -1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
        |]
        |> Array.sum


    // 10 - Ab
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
            -1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
        |]
        |> Array.sum


    // 11 - BA
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 12 - BB
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
            -1.0 * x.[12] // BB | ligation: B + B <-> BB
            1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 13 - Ba
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
            -1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 14 - Bb
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
            -1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 15 - aA
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[79] // baA | ligation: b + aA <-> baA
            -1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
            -1.0 * x.[15] // aA | ligation: a + A <-> aA
            1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
        |]
        |> Array.sum


    // 16 - aB
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[80] // baB | ligation: b + aB <-> baB
            -1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
            -1.0 * x.[16] // aB | ligation: a + B <-> aB
            1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
        |]
        |> Array.sum


    // 17 - aa
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[81] // baa | ligation: b + aa <-> baa
            -1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
            -1.0 * x.[17] // aa | ligation: a + a <-> aa
            1.0 * x.[5] * x.[5] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 18 - ab
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[82] // bab | ligation: b + ab <-> bab
            -1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[66] // aab | ligation: a + ab <-> aab
            -1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
            -1.0 * x.[18] // ab | ligation: a + b <-> ab
            1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 19 - bA
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
            -1.0 * x.[19] // bA | ligation: b + A <-> bA
            1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[67] // abA | ligation: a + bA <-> abA
            -1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 20 - bB
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
            -1.0 * x.[20] // bB | ligation: b + B <-> bB
            1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[68] // abB | ligation: a + bB <-> abB
            -1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 21 - ba
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[85] // bba | ligation: b + ba <-> bba
            -1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[69] // aba | ligation: a + ba <-> aba
            -1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 22 - bb
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[86] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[6] * x.[22] // b + bb | ligation: b + bb <-> bbb
            -1.0 * x.[22] // bb | ligation: b + b <-> bb
            1.0 * x.[6] * x.[6] // b + b | ligation: b + b <-> bb
            1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[70] // abb | ligation: a + bb <-> abb
            -1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 23 - AAA
    let d23 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 24 - AAB
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 25 - AAa
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 26 - AAb
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 27 - ABA
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 28 - ABB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 29 - ABa
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 30 - ABb
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 31 - AaA
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 32 - AaB
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 33 - Aaa
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 34 - Aab
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 35 - AbA
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 36 - AbB
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 37 - Aba
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 38 - Abb
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 39 - BAA
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 40 - BAB
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 41 - BAa
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 42 - BAb
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 43 - BBA
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 44 - BBB
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 45 - BBa
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 46 - BBb
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 47 - BaA
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 48 - BaB
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 49 - Baa
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 50 - Bab
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 51 - BbA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 52 - BbB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 53 - Bba
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 54 - Bbb
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 55 - aAA
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 56 - aAB
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 57 - aAa
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 58 - aAb
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 59 - aBA
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 60 - aBB
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 61 - aBa
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 62 - aBb
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 63 - aaA
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 64 - aaB
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 65 - aaa
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 66 - aab
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[66] // aab | ligation: a + ab <-> aab
            1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 67 - abA
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[67] // abA | ligation: a + bA <-> abA
            1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 68 - abB
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[68] // abB | ligation: a + bB <-> abB
            1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 69 - aba
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[69] // aba | ligation: a + ba <-> aba
            1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 70 - abb
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[70] // abb | ligation: a + bb <-> abb
            1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 71 - bAA
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 72 - bAB
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 73 - bAa
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 74 - bAb
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 75 - bBA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 76 - bBB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 77 - bBa
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 78 - bBb
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 79 - baA
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[79] // baA | ligation: b + aA <-> baA
            1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 80 - baB
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[80] // baB | ligation: b + aB <-> baB
            1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 81 - baa
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[81] // baa | ligation: b + aa <-> baa
            1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 82 - bab
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[82] // bab | ligation: b + ab <-> bab
            1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 83 - bbA
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 84 - bbB
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 85 - bba
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[85] // bba | ligation: b + ba <-> bba
            1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 86 - bbb
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[86] // bbb | ligation: b + bb <-> bbb
            1.0 * x.[6] * x.[22] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum



    let update (xRaw : array<double>) : array<double> = 
        // printfn "update::Starting..."
        let x = xRaw |> Array.map (fun e -> max e 0.0)
        let xSum = (x |> Array.sum) - (x.[1] + x.[2] + x.[0])


        let xSumN = 
            [|
                1.0 * x.[3] // A
                1.0 * x.[4] // B
                1.0 * x.[5] // a
                1.0 * x.[6] // b
                2.0 * x.[7] // AA
                2.0 * x.[8] // AB
                2.0 * x.[9] // Aa
                2.0 * x.[10] // Ab
                2.0 * x.[11] // BA
                2.0 * x.[12] // BB
                2.0 * x.[13] // Ba
                2.0 * x.[14] // Bb
                2.0 * x.[15] // aA
                2.0 * x.[16] // aB
                2.0 * x.[17] // aa
                2.0 * x.[18] // ab
                2.0 * x.[19] // bA
                2.0 * x.[20] // bB
                2.0 * x.[21] // ba
                2.0 * x.[22] // bb
                3.0 * x.[23] // AAA
                3.0 * x.[24] // AAB
                3.0 * x.[25] // AAa
                3.0 * x.[26] // AAb
                3.0 * x.[27] // ABA
                3.0 * x.[28] // ABB
                3.0 * x.[29] // ABa
                3.0 * x.[30] // ABb
                3.0 * x.[31] // AaA
                3.0 * x.[32] // AaB
                3.0 * x.[33] // Aaa
                3.0 * x.[34] // Aab
                3.0 * x.[35] // AbA
                3.0 * x.[36] // AbB
                3.0 * x.[37] // Aba
                3.0 * x.[38] // Abb
                3.0 * x.[39] // BAA
                3.0 * x.[40] // BAB
                3.0 * x.[41] // BAa
                3.0 * x.[42] // BAb
                3.0 * x.[43] // BBA
                3.0 * x.[44] // BBB
                3.0 * x.[45] // BBa
                3.0 * x.[46] // BBb
                3.0 * x.[47] // BaA
                3.0 * x.[48] // BaB
                3.0 * x.[49] // Baa
                3.0 * x.[50] // Bab
                3.0 * x.[51] // BbA
                3.0 * x.[52] // BbB
                3.0 * x.[53] // Bba
                3.0 * x.[54] // Bbb
                3.0 * x.[55] // aAA
                3.0 * x.[56] // aAB
                3.0 * x.[57] // aAa
                3.0 * x.[58] // aAb
                3.0 * x.[59] // aBA
                3.0 * x.[60] // aBB
                3.0 * x.[61] // aBa
                3.0 * x.[62] // aBb
                3.0 * x.[63] // aaA
                3.0 * x.[64] // aaB
                3.0 * x.[65] // aaa
                3.0 * x.[66] // aab
                3.0 * x.[67] // abA
                3.0 * x.[68] // abB
                3.0 * x.[69] // aba
                3.0 * x.[70] // abb
                3.0 * x.[71] // bAA
                3.0 * x.[72] // bAB
                3.0 * x.[73] // bAa
                3.0 * x.[74] // bAb
                3.0 * x.[75] // bBA
                3.0 * x.[76] // bBB
                3.0 * x.[77] // bBa
                3.0 * x.[78] // bBb
                3.0 * x.[79] // baA
                3.0 * x.[80] // baB
                3.0 * x.[81] // baa
                3.0 * x.[82] // bab
                3.0 * x.[83] // bbA
                3.0 * x.[84] // bbB
                3.0 * x.[85] // bba
                3.0 * x.[86] // bbb
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[3] * x.[3] // A
                1.0 * x.[4] * x.[4] // B
                1.0 * x.[5] * x.[5] // a
                1.0 * x.[6] * x.[6] // b
                2.0 * x.[7] * x.[7] // AA
                2.0 * x.[8] * x.[8] // AB
                2.0 * x.[9] * x.[9] // Aa
                2.0 * x.[10] * x.[10] // Ab
                2.0 * x.[11] * x.[11] // BA
                2.0 * x.[12] * x.[12] // BB
                2.0 * x.[13] * x.[13] // Ba
                2.0 * x.[14] * x.[14] // Bb
                2.0 * x.[15] * x.[15] // aA
                2.0 * x.[16] * x.[16] // aB
                2.0 * x.[17] * x.[17] // aa
                2.0 * x.[18] * x.[18] // ab
                2.0 * x.[19] * x.[19] // bA
                2.0 * x.[20] * x.[20] // bB
                2.0 * x.[21] * x.[21] // ba
                2.0 * x.[22] * x.[22] // bb
                3.0 * x.[23] * x.[23] // AAA
                3.0 * x.[24] * x.[24] // AAB
                3.0 * x.[25] * x.[25] // AAa
                3.0 * x.[26] * x.[26] // AAb
                3.0 * x.[27] * x.[27] // ABA
                3.0 * x.[28] * x.[28] // ABB
                3.0 * x.[29] * x.[29] // ABa
                3.0 * x.[30] * x.[30] // ABb
                3.0 * x.[31] * x.[31] // AaA
                3.0 * x.[32] * x.[32] // AaB
                3.0 * x.[33] * x.[33] // Aaa
                3.0 * x.[34] * x.[34] // Aab
                3.0 * x.[35] * x.[35] // AbA
                3.0 * x.[36] * x.[36] // AbB
                3.0 * x.[37] * x.[37] // Aba
                3.0 * x.[38] * x.[38] // Abb
                3.0 * x.[39] * x.[39] // BAA
                3.0 * x.[40] * x.[40] // BAB
                3.0 * x.[41] * x.[41] // BAa
                3.0 * x.[42] * x.[42] // BAb
                3.0 * x.[43] * x.[43] // BBA
                3.0 * x.[44] * x.[44] // BBB
                3.0 * x.[45] * x.[45] // BBa
                3.0 * x.[46] * x.[46] // BBb
                3.0 * x.[47] * x.[47] // BaA
                3.0 * x.[48] * x.[48] // BaB
                3.0 * x.[49] * x.[49] // Baa
                3.0 * x.[50] * x.[50] // Bab
                3.0 * x.[51] * x.[51] // BbA
                3.0 * x.[52] * x.[52] // BbB
                3.0 * x.[53] * x.[53] // Bba
                3.0 * x.[54] * x.[54] // Bbb
                3.0 * x.[55] * x.[55] // aAA
                3.0 * x.[56] * x.[56] // aAB
                3.0 * x.[57] * x.[57] // aAa
                3.0 * x.[58] * x.[58] // aAb
                3.0 * x.[59] * x.[59] // aBA
                3.0 * x.[60] * x.[60] // aBB
                3.0 * x.[61] * x.[61] // aBa
                3.0 * x.[62] * x.[62] // aBb
                3.0 * x.[63] * x.[63] // aaA
                3.0 * x.[64] * x.[64] // aaB
                3.0 * x.[65] * x.[65] // aaa
                3.0 * x.[66] * x.[66] // aab
                3.0 * x.[67] * x.[67] // abA
                3.0 * x.[68] * x.[68] // abB
                3.0 * x.[69] * x.[69] // aba
                3.0 * x.[70] * x.[70] // abb
                3.0 * x.[71] * x.[71] // bAA
                3.0 * x.[72] * x.[72] // bAB
                3.0 * x.[73] * x.[73] // bAa
                3.0 * x.[74] * x.[74] // bAb
                3.0 * x.[75] * x.[75] // bBA
                3.0 * x.[76] * x.[76] // bBB
                3.0 * x.[77] * x.[77] // bBa
                3.0 * x.[78] * x.[78] // bBb
                3.0 * x.[79] * x.[79] // baA
                3.0 * x.[80] * x.[80] // baB
                3.0 * x.[81] * x.[81] // baa
                3.0 * x.[82] * x.[82] // bab
                3.0 * x.[83] * x.[83] // bbA
                3.0 * x.[84] * x.[84] // bbB
                3.0 * x.[85] * x.[85] // bba
                3.0 * x.[86] * x.[86] // bbb
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
            d17 x xSum xSumN xSumSquaredN
            d18 x xSum xSumN xSumSquaredN
            d19 x xSum xSumN xSumSquaredN
            d20 x xSum xSumN xSumSquaredN
            d21 x xSum xSumN xSumSquaredN
            d22 x xSum xSumN xSumSquaredN
            d23 x xSum xSumN xSumSquaredN
            d24 x xSum xSumN xSumSquaredN
            d25 x xSum xSumN xSumSquaredN
            d26 x xSum xSumN xSumSquaredN
            d27 x xSum xSumN xSumSquaredN
            d28 x xSum xSumN xSumSquaredN
            d29 x xSum xSumN xSumSquaredN
            d30 x xSum xSumN xSumSquaredN
            d31 x xSum xSumN xSumSquaredN
            d32 x xSum xSumN xSumSquaredN
            d33 x xSum xSumN xSumSquaredN
            d34 x xSum xSumN xSumSquaredN
            d35 x xSum xSumN xSumSquaredN
            d36 x xSum xSumN xSumSquaredN
            d37 x xSum xSumN xSumSquaredN
            d38 x xSum xSumN xSumSquaredN
            d39 x xSum xSumN xSumSquaredN
            d40 x xSum xSumN xSumSquaredN
            d41 x xSum xSumN xSumSquaredN
            d42 x xSum xSumN xSumSquaredN
            d43 x xSum xSumN xSumSquaredN
            d44 x xSum xSumN xSumSquaredN
            d45 x xSum xSumN xSumSquaredN
            d46 x xSum xSumN xSumSquaredN
            d47 x xSum xSumN xSumSquaredN
            d48 x xSum xSumN xSumSquaredN
            d49 x xSum xSumN xSumSquaredN
            d50 x xSum xSumN xSumSquaredN
            d51 x xSum xSumN xSumSquaredN
            d52 x xSum xSumN xSumSquaredN
            d53 x xSum xSumN xSumSquaredN
            d54 x xSum xSumN xSumSquaredN
            d55 x xSum xSumN xSumSquaredN
            d56 x xSum xSumN xSumSquaredN
            d57 x xSum xSumN xSumSquaredN
            d58 x xSum xSumN xSumSquaredN
            d59 x xSum xSumN xSumSquaredN
            d60 x xSum xSumN xSumSquaredN
            d61 x xSum xSumN xSumSquaredN
            d62 x xSum xSumN xSumSquaredN
            d63 x xSum xSumN xSumSquaredN
            d64 x xSum xSumN xSumSquaredN
            d65 x xSum xSumN xSumSquaredN
            d66 x xSum xSumN xSumSquaredN
            d67 x xSum xSumN xSumSquaredN
            d68 x xSum xSumN xSumSquaredN
            d69 x xSum xSumN xSumSquaredN
            d70 x xSum xSumN xSumSquaredN
            d71 x xSum xSumN xSumSquaredN
            d72 x xSum xSumN xSumSquaredN
            d73 x xSum xSumN xSumSquaredN
            d74 x xSum xSumN xSumSquaredN
            d75 x xSum xSumN xSumSquaredN
            d76 x xSum xSumN xSumSquaredN
            d77 x xSum xSumN xSumSquaredN
            d78 x xSum xSumN xSumSquaredN
            d79 x xSum xSumN xSumSquaredN
            d80 x xSum xSumN xSumSquaredN
            d81 x xSum xSumN xSumSquaredN
            d82 x xSum xSumN xSumSquaredN
            d83 x xSum xSumN xSumSquaredN
            d84 x xSum xSumN xSumSquaredN
            d85 x xSum xSumN xSumSquaredN
            d86 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData =
        {
            regularParams =
                {
                    modelDataParams =
                        {
                            modelInfo =
                                {
                                    fileStructureVersionNumber = "2.0.0.0"
                                    versionNumber = "2.0.0.0"
                                    seedValue = seedValue
                                    modelDataId = ModelDataId 6L
                                    numberOfSubstances = 87
                                    numberOfAminoAcids = TwoAminoAcids
                                    maxPeptideLength = ThreeMax
                                    defaultSetIndex = 0
                                }

                            allParams =
                                [|
                                    {
                                        modelParam = 
                                            {
                                                wasteRecyclingRate = 0.1
                                            }
                                            |> WasteRecyclingRateParam
                                        usage = PrimaryParam
                                    }

                                    {
                                        modelParam = 
                                            {
                                                synthesisDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                forwardScale = Some 0.001
                                                backwardScale = Some 0.001
                                            }
                                            |> SynthRndParam
                                            |> SynthesisRateParam
                                        usage = PrimaryParam
                                    }

                                    {
                                        modelParam = 
                                            {
                                                destructionDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                forwardScale = Some 0.001
                                                backwardScale = Some 0.001
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
                                                        backwardScale = Some 0.001
                                                    }
                                                    |> DestrRndParam

                                                catDestrRndEeParams = 
                                                    {
                                                        rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0001; scale = Some 100000.0; shift = None } } |> Distribution |> RateMultDistr
                                                        eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                        eeBackwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
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
                                                                backwardScale = Some 0.001
                                                            }
                                                            |> DestrRndParam

                                                        catDestrRndEeParams = 
                                                            {
                                                                rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0001; scale = Some 100000.0; shift = None } } |> Distribution |> RateMultDistr
                                                                eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                                eeBackwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                            }
                                                    }

                                                catDestrSimParam = 
                                                    {
                                                        simBaseDistribution = { distributionType = Uniform; distributionParams = { threshold = Some 0.2; scale = None; shift = Some 1.0 } } |> Distribution
                                                        getRateMultiplierDistr = DeltaRateMultDistrGetter
                                                        getForwardEeDistr = DeltaEeDistributionGetter
                                                        getBackwardEeDistr = DeltaEeDistributionGetter
                                                    }

                                            }
                                            |> CatDestrSimParam
                                            |> CatalyticDestructionRateParam
                                        usage = PrimaryParam
                                    }

                                    {
                                        modelParam = 
                                            {
                                                ligationDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                forwardScale = Some 1.0
                                                backwardScale = Some 1.0
                                            }
                                            |> LigRndParam
                                            |> LigationRateParam
                                        usage = PrimaryParam
                                    }

                                |]
                        }

                    allSubst = allSubst
                    allInd = allInd

                    allRawReactions =
                        [
                            (FoodCreationName, 1)
                            (WasteRemovalName, 1)
                            (WasteRecyclingName, 1)
                            (SynthesisName, 4)
                            (DestructionName, 4)
                            (CatalyticSynthesisName, 256)
                            (CatalyticDestructionName, 256)
                            (LigationName, 39)
                            (CatalyticLigationName, 2496)
                            (SedimentationDirectName, 7056)
                            (SedimentationAllName, 4)
                            (RacemizationName, 4)
                            (CatalyticRacemizationName, 256)
                        ]

                    allReactions =
                        [
                            (WasteRecyclingName, 1)
                            (SynthesisName, 4)
                            (DestructionName, 4)
                            (CatalyticDestructionName, 124)
                            (LigationName, 78)
                        ]
                }

            funcParams =
                {
                    getTotals = getTotals
                    getTotalSubst = getTotalSubst
                    getDerivative = update
                }
        }


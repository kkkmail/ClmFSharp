namespace Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 1187743572
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 85

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
            x.[2] // B
            x.[3] // a
            x.[4] // b
            2.0 * x.[5] // AA
            2.0 * x.[6] // AB
            2.0 * x.[7] // Aa
            2.0 * x.[8] // Ab
            2.0 * x.[9] // BA
            2.0 * x.[10] // BB
            2.0 * x.[11] // Ba
            2.0 * x.[12] // Bb
            2.0 * x.[13] // aA
            2.0 * x.[14] // aB
            2.0 * x.[15] // aa
            2.0 * x.[16] // ab
            2.0 * x.[17] // bA
            2.0 * x.[18] // bB
            2.0 * x.[19] // ba
            2.0 * x.[20] // bb
            3.0 * x.[21] // AAA
            3.0 * x.[22] // AAB
            3.0 * x.[23] // AAa
            3.0 * x.[24] // AAb
            3.0 * x.[25] // ABA
            3.0 * x.[26] // ABB
            3.0 * x.[27] // ABa
            3.0 * x.[28] // ABb
            3.0 * x.[29] // AaA
            3.0 * x.[30] // AaB
            3.0 * x.[31] // Aaa
            3.0 * x.[32] // Aab
            3.0 * x.[33] // AbA
            3.0 * x.[34] // AbB
            3.0 * x.[35] // Aba
            3.0 * x.[36] // Abb
            3.0 * x.[37] // BAA
            3.0 * x.[38] // BAB
            3.0 * x.[39] // BAa
            3.0 * x.[40] // BAb
            3.0 * x.[41] // BBA
            3.0 * x.[42] // BBB
            3.0 * x.[43] // BBa
            3.0 * x.[44] // BBb
            3.0 * x.[45] // BaA
            3.0 * x.[46] // BaB
            3.0 * x.[47] // Baa
            3.0 * x.[48] // Bab
            3.0 * x.[49] // BbA
            3.0 * x.[50] // BbB
            3.0 * x.[51] // Bba
            3.0 * x.[52] // Bbb
            3.0 * x.[53] // aAA
            3.0 * x.[54] // aAB
            3.0 * x.[55] // aAa
            3.0 * x.[56] // aAb
            3.0 * x.[57] // aBA
            3.0 * x.[58] // aBB
            3.0 * x.[59] // aBa
            3.0 * x.[60] // aBb
            3.0 * x.[61] // aaA
            3.0 * x.[62] // aaB
            3.0 * x.[63] // aaa
            3.0 * x.[64] // aab
            3.0 * x.[65] // abA
            3.0 * x.[66] // abB
            3.0 * x.[67] // aba
            3.0 * x.[68] // abb
            3.0 * x.[69] // bAA
            3.0 * x.[70] // bAB
            3.0 * x.[71] // bAa
            3.0 * x.[72] // bAb
            3.0 * x.[73] // bBA
            3.0 * x.[74] // bBB
            3.0 * x.[75] // bBa
            3.0 * x.[76] // bBb
            3.0 * x.[77] // baA
            3.0 * x.[78] // baB
            3.0 * x.[79] // baa
            3.0 * x.[80] // bab
            3.0 * x.[81] // bbA
            3.0 * x.[82] // bbB
            3.0 * x.[83] // bba
            3.0 * x.[84] // bbb
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[5] // AA
                    x.[6] // AB
                    x.[7] // Aa
                    x.[8] // Ab
                    x.[9] // BA
                    x.[13] // aA
                    x.[17] // bA
                    3.0 * x.[21] // AAA
                    2.0 * x.[22] // AAB
                    2.0 * x.[23] // AAa
                    2.0 * x.[24] // AAb
                    2.0 * x.[25] // ABA
                    x.[26] // ABB
                    x.[27] // ABa
                    x.[28] // ABb
                    2.0 * x.[29] // AaA
                    x.[30] // AaB
                    x.[31] // Aaa
                    x.[32] // Aab
                    2.0 * x.[33] // AbA
                    x.[34] // AbB
                    x.[35] // Aba
                    x.[36] // Abb
                    2.0 * x.[37] // BAA
                    x.[38] // BAB
                    x.[39] // BAa
                    x.[40] // BAb
                    x.[41] // BBA
                    x.[45] // BaA
                    x.[49] // BbA
                    2.0 * x.[53] // aAA
                    x.[54] // aAB
                    x.[55] // aAa
                    x.[56] // aAb
                    x.[57] // aBA
                    x.[61] // aaA
                    x.[65] // abA
                    2.0 * x.[69] // bAA
                    x.[70] // bAB
                    x.[71] // bAa
                    x.[72] // bAb
                    x.[73] // bBA
                    x.[77] // baA
                    x.[81] // bbA
                |]
                |> Array.sum
                ,
                [|
                    x.[3] // a
                    x.[7] // Aa
                    x.[11] // Ba
                    x.[13] // aA
                    x.[14] // aB
                    2.0 * x.[15] // aa
                    x.[16] // ab
                    x.[19] // ba
                    x.[23] // AAa
                    x.[27] // ABa
                    x.[29] // AaA
                    x.[30] // AaB
                    2.0 * x.[31] // Aaa
                    x.[32] // Aab
                    x.[35] // Aba
                    x.[39] // BAa
                    x.[43] // BBa
                    x.[45] // BaA
                    x.[46] // BaB
                    2.0 * x.[47] // Baa
                    x.[48] // Bab
                    x.[51] // Bba
                    x.[53] // aAA
                    x.[54] // aAB
                    2.0 * x.[55] // aAa
                    x.[56] // aAb
                    x.[57] // aBA
                    x.[58] // aBB
                    2.0 * x.[59] // aBa
                    x.[60] // aBb
                    2.0 * x.[61] // aaA
                    2.0 * x.[62] // aaB
                    3.0 * x.[63] // aaa
                    2.0 * x.[64] // aab
                    x.[65] // abA
                    x.[66] // abB
                    2.0 * x.[67] // aba
                    x.[68] // abb
                    x.[71] // bAa
                    x.[75] // bBa
                    x.[77] // baA
                    x.[78] // baB
                    2.0 * x.[79] // baa
                    x.[80] // bab
                    x.[83] // bba
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[2] // B
                    x.[6] // AB
                    x.[9] // BA
                    2.0 * x.[10] // BB
                    x.[11] // Ba
                    x.[12] // Bb
                    x.[14] // aB
                    x.[18] // bB
                    x.[22] // AAB
                    x.[25] // ABA
                    2.0 * x.[26] // ABB
                    x.[27] // ABa
                    x.[28] // ABb
                    x.[30] // AaB
                    x.[34] // AbB
                    x.[37] // BAA
                    2.0 * x.[38] // BAB
                    x.[39] // BAa
                    x.[40] // BAb
                    2.0 * x.[41] // BBA
                    3.0 * x.[42] // BBB
                    2.0 * x.[43] // BBa
                    2.0 * x.[44] // BBb
                    x.[45] // BaA
                    2.0 * x.[46] // BaB
                    x.[47] // Baa
                    x.[48] // Bab
                    x.[49] // BbA
                    2.0 * x.[50] // BbB
                    x.[51] // Bba
                    x.[52] // Bbb
                    x.[54] // aAB
                    x.[57] // aBA
                    2.0 * x.[58] // aBB
                    x.[59] // aBa
                    x.[60] // aBb
                    x.[62] // aaB
                    x.[66] // abB
                    x.[70] // bAB
                    x.[73] // bBA
                    2.0 * x.[74] // bBB
                    x.[75] // bBa
                    x.[76] // bBb
                    x.[78] // baB
                    x.[82] // bbB
                |]
                |> Array.sum
                ,
                [|
                    x.[4] // b
                    x.[8] // Ab
                    x.[12] // Bb
                    x.[16] // ab
                    x.[17] // bA
                    x.[18] // bB
                    x.[19] // ba
                    2.0 * x.[20] // bb
                    x.[24] // AAb
                    x.[28] // ABb
                    x.[32] // Aab
                    x.[33] // AbA
                    x.[34] // AbB
                    x.[35] // Aba
                    2.0 * x.[36] // Abb
                    x.[40] // BAb
                    x.[44] // BBb
                    x.[48] // Bab
                    x.[49] // BbA
                    x.[50] // BbB
                    x.[51] // Bba
                    2.0 * x.[52] // Bbb
                    x.[56] // aAb
                    x.[60] // aBb
                    x.[64] // aab
                    x.[65] // abA
                    x.[66] // abB
                    x.[67] // aba
                    2.0 * x.[68] // abb
                    x.[69] // bAA
                    x.[70] // bAB
                    x.[71] // bAa
                    2.0 * x.[72] // bAb
                    x.[73] // bBA
                    x.[74] // bBB
                    x.[75] // bBa
                    2.0 * x.[76] // bBb
                    x.[77] // baA
                    x.[78] // baB
                    x.[79] // baa
                    2.0 * x.[80] // bab
                    2.0 * x.[81] // bbA
                    2.0 * x.[82] // bbB
                    2.0 * x.[83] // bba
                    3.0 * x.[84] // bbb
                |]
                |> Array.sum
            )
        |]



    // 0 - Y
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            0.0001 * x.[4] // b | synthesis: Y <-> b
            -0.001 * x.[0] // Y | synthesis: Y <-> b
            0.0001 * x.[2] // B | synthesis: Y <-> B
            -0.001 * x.[0] // Y | synthesis: Y <-> B
            0.0001 * x.[3] // a | synthesis: Y <-> a
            -0.001 * x.[0] // Y | synthesis: Y <-> a
            0.0001 * x.[1] // A | synthesis: Y <-> A
            -0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 1 - A
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            0.001 * x.[3] // a | racemization: a -> A
            -0.001 * x.[1] // A | racemization: A -> a
            -0.0001 * x.[1] // A | synthesis: Y <-> A
            0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 2 - B
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            0.210183646462902 * x.[4] * x.[42] // b + BBB | catalytic racemization: b + BBB -> B + BBB
            -0.210183646462902 * x.[2] * x.[84] // B + bbb | catalytic racemization: B + bbb -> b + bbb
            0.293117523965138 * x.[4] * x.[31] // b + Aaa | catalytic racemization: b + Aaa -> B + Aaa
            -0.293117523965138 * x.[2] * x.[53] // B + aAA | catalytic racemization: B + aAA -> b + aAA
            0.207843643371621 * x.[4] * x.[84] // b + bbb | catalytic racemization: b + bbb -> B + bbb
            -0.207843643371621 * x.[2] * x.[42] // B + BBB | catalytic racemization: B + BBB -> b + BBB
            0.285976142064811 * x.[4] * x.[53] // b + aAA | catalytic racemization: b + aAA -> B + aAA
            -0.285976142064811 * x.[2] * x.[31] // B + Aaa | catalytic racemization: B + Aaa -> b + Aaa
            0.001 * x.[4] // b | racemization: b -> B
            -0.001 * x.[2] // B | racemization: B -> b
            -0.0001 * x.[2] // B | synthesis: Y <-> B
            0.001 * x.[0] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 3 - a
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -0.001 * x.[3] // a | racemization: a -> A
            0.001 * x.[1] // A | racemization: A -> a
            -0.0001 * x.[3] // a | synthesis: Y <-> a
            0.001 * x.[0] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 4 - b
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -0.210183646462902 * x.[4] * x.[42] // b + BBB | catalytic racemization: b + BBB -> B + BBB
            0.210183646462902 * x.[2] * x.[84] // B + bbb | catalytic racemization: B + bbb -> b + bbb
            -0.293117523965138 * x.[4] * x.[31] // b + Aaa | catalytic racemization: b + Aaa -> B + Aaa
            0.293117523965138 * x.[2] * x.[53] // B + aAA | catalytic racemization: B + aAA -> b + aAA
            -0.207843643371621 * x.[4] * x.[84] // b + bbb | catalytic racemization: b + bbb -> B + bbb
            0.207843643371621 * x.[2] * x.[42] // B + BBB | catalytic racemization: B + BBB -> b + BBB
            -0.285976142064811 * x.[4] * x.[53] // b + aAA | catalytic racemization: b + aAA -> B + aAA
            0.285976142064811 * x.[2] * x.[31] // B + Aaa | catalytic racemization: B + Aaa -> b + Aaa
            -0.001 * x.[4] // b | racemization: b -> B
            0.001 * x.[2] // B | racemization: B -> b
            -0.0001 * x.[4] // b | synthesis: Y <-> b
            0.001 * x.[0] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 5 - AA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 6 - AB
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 7 - Aa
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 8 - Ab
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 9 - BA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 10 - BB
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 11 - Ba
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 12 - Bb
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 13 - aA
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 14 - aB
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 15 - aa
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 16 - ab
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 17 - bA
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 18 - bB
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 19 - ba
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 20 - bb
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 21 - AAA
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 22 - AAB
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 23 - AAa
    let d23 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 24 - AAb
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 25 - ABA
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 26 - ABB
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 27 - ABa
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 28 - ABb
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 29 - AaA
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 30 - AaB
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 31 - Aaa
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 32 - Aab
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 33 - AbA
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 34 - AbB
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 35 - Aba
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 36 - Abb
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 37 - BAA
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 38 - BAB
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 39 - BAa
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 40 - BAb
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 41 - BBA
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 42 - BBB
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 43 - BBa
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 44 - BBb
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 45 - BaA
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 46 - BaB
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 47 - Baa
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 48 - Bab
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 49 - BbA
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 50 - BbB
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 51 - Bba
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 52 - Bbb
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 53 - aAA
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 54 - aAB
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 55 - aAa
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 56 - aAb
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 57 - aBA
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 58 - aBB
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 59 - aBa
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 60 - aBb
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 61 - aaA
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 62 - aaB
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 63 - aaa
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 64 - aab
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 65 - abA
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 66 - abB
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 67 - aba
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 68 - abb
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 69 - bAA
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 70 - bAB
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 71 - bAa
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 72 - bAb
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 73 - bBA
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 74 - bBB
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 75 - bBa
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 76 - bBb
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 77 - baA
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 78 - baB
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 79 - baa
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 80 - bab
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 81 - bbA
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 82 - bbB
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 83 - bba
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum


    // 84 - bbb
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

        let xSum = (x |> Array.sum) - x.[0]


        let xSumN = 
            [|
                1.0 * x.[1] // A
                1.0 * x.[2] // B
                1.0 * x.[3] // a
                1.0 * x.[4] // b
                2.0 * x.[5] // AA
                2.0 * x.[6] // AB
                2.0 * x.[7] // Aa
                2.0 * x.[8] // Ab
                2.0 * x.[9] // BA
                2.0 * x.[10] // BB
                2.0 * x.[11] // Ba
                2.0 * x.[12] // Bb
                2.0 * x.[13] // aA
                2.0 * x.[14] // aB
                2.0 * x.[15] // aa
                2.0 * x.[16] // ab
                2.0 * x.[17] // bA
                2.0 * x.[18] // bB
                2.0 * x.[19] // ba
                2.0 * x.[20] // bb
                3.0 * x.[21] // AAA
                3.0 * x.[22] // AAB
                3.0 * x.[23] // AAa
                3.0 * x.[24] // AAb
                3.0 * x.[25] // ABA
                3.0 * x.[26] // ABB
                3.0 * x.[27] // ABa
                3.0 * x.[28] // ABb
                3.0 * x.[29] // AaA
                3.0 * x.[30] // AaB
                3.0 * x.[31] // Aaa
                3.0 * x.[32] // Aab
                3.0 * x.[33] // AbA
                3.0 * x.[34] // AbB
                3.0 * x.[35] // Aba
                3.0 * x.[36] // Abb
                3.0 * x.[37] // BAA
                3.0 * x.[38] // BAB
                3.0 * x.[39] // BAa
                3.0 * x.[40] // BAb
                3.0 * x.[41] // BBA
                3.0 * x.[42] // BBB
                3.0 * x.[43] // BBa
                3.0 * x.[44] // BBb
                3.0 * x.[45] // BaA
                3.0 * x.[46] // BaB
                3.0 * x.[47] // Baa
                3.0 * x.[48] // Bab
                3.0 * x.[49] // BbA
                3.0 * x.[50] // BbB
                3.0 * x.[51] // Bba
                3.0 * x.[52] // Bbb
                3.0 * x.[53] // aAA
                3.0 * x.[54] // aAB
                3.0 * x.[55] // aAa
                3.0 * x.[56] // aAb
                3.0 * x.[57] // aBA
                3.0 * x.[58] // aBB
                3.0 * x.[59] // aBa
                3.0 * x.[60] // aBb
                3.0 * x.[61] // aaA
                3.0 * x.[62] // aaB
                3.0 * x.[63] // aaa
                3.0 * x.[64] // aab
                3.0 * x.[65] // abA
                3.0 * x.[66] // abB
                3.0 * x.[67] // aba
                3.0 * x.[68] // abb
                3.0 * x.[69] // bAA
                3.0 * x.[70] // bAB
                3.0 * x.[71] // bAa
                3.0 * x.[72] // bAb
                3.0 * x.[73] // bBA
                3.0 * x.[74] // bBB
                3.0 * x.[75] // bBa
                3.0 * x.[76] // bBb
                3.0 * x.[77] // baA
                3.0 * x.[78] // baB
                3.0 * x.[79] // baa
                3.0 * x.[80] // bab
                3.0 * x.[81] // bbA
                3.0 * x.[82] // bbB
                3.0 * x.[83] // bba
                3.0 * x.[84] // bbb
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[1] * x.[1] // A
                1.0 * x.[2] * x.[2] // B
                1.0 * x.[3] * x.[3] // a
                1.0 * x.[4] * x.[4] // b
                2.0 * x.[5] * x.[5] // AA
                2.0 * x.[6] * x.[6] // AB
                2.0 * x.[7] * x.[7] // Aa
                2.0 * x.[8] * x.[8] // Ab
                2.0 * x.[9] * x.[9] // BA
                2.0 * x.[10] * x.[10] // BB
                2.0 * x.[11] * x.[11] // Ba
                2.0 * x.[12] * x.[12] // Bb
                2.0 * x.[13] * x.[13] // aA
                2.0 * x.[14] * x.[14] // aB
                2.0 * x.[15] * x.[15] // aa
                2.0 * x.[16] * x.[16] // ab
                2.0 * x.[17] * x.[17] // bA
                2.0 * x.[18] * x.[18] // bB
                2.0 * x.[19] * x.[19] // ba
                2.0 * x.[20] * x.[20] // bb
                3.0 * x.[21] * x.[21] // AAA
                3.0 * x.[22] * x.[22] // AAB
                3.0 * x.[23] * x.[23] // AAa
                3.0 * x.[24] * x.[24] // AAb
                3.0 * x.[25] * x.[25] // ABA
                3.0 * x.[26] * x.[26] // ABB
                3.0 * x.[27] * x.[27] // ABa
                3.0 * x.[28] * x.[28] // ABb
                3.0 * x.[29] * x.[29] // AaA
                3.0 * x.[30] * x.[30] // AaB
                3.0 * x.[31] * x.[31] // Aaa
                3.0 * x.[32] * x.[32] // Aab
                3.0 * x.[33] * x.[33] // AbA
                3.0 * x.[34] * x.[34] // AbB
                3.0 * x.[35] * x.[35] // Aba
                3.0 * x.[36] * x.[36] // Abb
                3.0 * x.[37] * x.[37] // BAA
                3.0 * x.[38] * x.[38] // BAB
                3.0 * x.[39] * x.[39] // BAa
                3.0 * x.[40] * x.[40] // BAb
                3.0 * x.[41] * x.[41] // BBA
                3.0 * x.[42] * x.[42] // BBB
                3.0 * x.[43] * x.[43] // BBa
                3.0 * x.[44] * x.[44] // BBb
                3.0 * x.[45] * x.[45] // BaA
                3.0 * x.[46] * x.[46] // BaB
                3.0 * x.[47] * x.[47] // Baa
                3.0 * x.[48] * x.[48] // Bab
                3.0 * x.[49] * x.[49] // BbA
                3.0 * x.[50] * x.[50] // BbB
                3.0 * x.[51] * x.[51] // Bba
                3.0 * x.[52] * x.[52] // Bbb
                3.0 * x.[53] * x.[53] // aAA
                3.0 * x.[54] * x.[54] // aAB
                3.0 * x.[55] * x.[55] // aAa
                3.0 * x.[56] * x.[56] // aAb
                3.0 * x.[57] * x.[57] // aBA
                3.0 * x.[58] * x.[58] // aBB
                3.0 * x.[59] * x.[59] // aBa
                3.0 * x.[60] * x.[60] // aBb
                3.0 * x.[61] * x.[61] // aaA
                3.0 * x.[62] * x.[62] // aaB
                3.0 * x.[63] * x.[63] // aaa
                3.0 * x.[64] * x.[64] // aab
                3.0 * x.[65] * x.[65] // abA
                3.0 * x.[66] * x.[66] // abB
                3.0 * x.[67] * x.[67] // aba
                3.0 * x.[68] * x.[68] // abb
                3.0 * x.[69] * x.[69] // bAA
                3.0 * x.[70] * x.[70] // bAB
                3.0 * x.[71] * x.[71] // bAa
                3.0 * x.[72] * x.[72] // bAb
                3.0 * x.[73] * x.[73] // bBA
                3.0 * x.[74] * x.[74] // bBB
                3.0 * x.[75] * x.[75] // bBa
                3.0 * x.[76] * x.[76] // bBb
                3.0 * x.[77] * x.[77] // baA
                3.0 * x.[78] * x.[78] // baB
                3.0 * x.[79] * x.[79] // baa
                3.0 * x.[80] * x.[80] // bab
                3.0 * x.[81] * x.[81] // bbA
                3.0 * x.[82] * x.[82] // bbB
                3.0 * x.[83] * x.[83] // bba
                3.0 * x.[84] * x.[84] // bbb
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
        |]


    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = "1.1.1.0"
                            versionNumber = "1.1.2.0"
                            seedValue = seedValue
                            modelName = "20181213_001"
                            numberOfSubstances = 85
                            numberOfAminoAcids = TwoAminoAcids
                            maxPeptideLength = ThreeMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution(1083333091, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.0001
                            }
                            |> SynthRndParam
                            |> SynthesisRateParam

                            {
                                racemizationDistribution = DeltaDistribution(125105949, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                            }
                            |> RacemRndParam
                            |> RacemizationRateParam

                            {
                                simRacemDistribution = UniformDistribution(1381698909, { threshold = Some 0.2 }) |> Uniform
                                aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            }
                            |> CatRacemSimParam
                            |> CatalyticRacemizationRateParam

                        ]
                }

            getTotals = getTotals
            getTotalSubst = getTotalSubst
            allSubst = allSubst
            allInd = allInd

            allRawReactions = 
                [
                    (SynthesisName, 4)
                    (CatalyticSynthesisName, 320)
                    (LigationName, 39)
                    (CatalyticLigationName, 3120)
                    (SedimentationDirectName, 2331)
                    (SedimentationAllName, 4)
                    (RacemizationName, 4)
                    (CatalyticRacemizationName, 320)
                ]

            allReactions = 
                [
                    (SynthesisName, 4)
                    (RacemizationName, 4)
                    (CatalyticRacemizationName, 8)
                ]
        }


namespace Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 1109423128
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
    let kW = 0.0173971274875205 / 86.0


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
            0.445054985045952 * x.[5] * x.[54] // a + Bbb | catalytic synthesis: Y + Bbb <-> a + Bbb
            -0.391385000088169 * x.[1] * x.[54] // Y + Bbb | catalytic synthesis: Y + Bbb <-> a + Bbb
            0.445054985045952 * x.[3] * x.[76] // A + bBB | catalytic synthesis: Y + bBB <-> A + bBB
            -0.391385000088169 * x.[1] * x.[76] // Y + bBB | catalytic synthesis: Y + bBB <-> A + bBB
            0.134829646778709 * x.[5] * x.[76] // a + bBB | catalytic synthesis: Y + bBB <-> a + bBB
            -0.188499631736492 * x.[1] * x.[76] // Y + bBB | catalytic synthesis: Y + bBB <-> a + bBB
            0.134829646778709 * x.[3] * x.[54] // A + Bbb | catalytic synthesis: Y + Bbb <-> A + Bbb
            -0.188499631736492 * x.[1] * x.[54] // Y + Bbb | catalytic synthesis: Y + Bbb <-> A + Bbb
            0.001 * x.[6] // b | synthesis: Y <-> b
            -0.001 * x.[1] // Y | synthesis: Y <-> b
            0.001 * x.[4] // B | synthesis: Y <-> B
            -0.001 * x.[1] // Y | synthesis: Y <-> B
            0.001 * x.[5] // a | synthesis: Y <-> a
            -0.001 * x.[1] // Y | synthesis: Y <-> a
            0.001 * x.[3] // A | synthesis: Y <-> A
            -0.001 * x.[1] // Y | synthesis: Y <-> A
            0.1 * x.[2] // Z | recycling: Z -> Y
            0.01 * 1.0 // 0 X | food: 0 X -> Y
        |]
        |> Array.sum


    // 2 - Z
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            kW * (2.0 * xSum * xSumN - xSumSquaredN)
            -1.11881259639926 * x.[2] * x.[28] // Z + ABB | catalytic destruction: b + ABB <-> Z + ABB
            0.492781927695115 * x.[6] * x.[28] // b + ABB | catalytic destruction: b + ABB <-> Z + ABB
            -1.11881259639926 * x.[2] * x.[70] // Z + abb | catalytic destruction: B + abb <-> Z + abb
            0.492781927695115 * x.[4] * x.[70] // B + abb | catalytic destruction: B + abb <-> Z + abb
            -0.591857853097805 * x.[2] * x.[70] // Z + abb | catalytic destruction: b + abb <-> Z + abb
            1.21788852180195 * x.[6] * x.[70] // b + abb | catalytic destruction: b + abb <-> Z + abb
            -0.591857853097805 * x.[2] * x.[28] // Z + ABB | catalytic destruction: B + ABB <-> Z + ABB
            1.21788852180195 * x.[4] * x.[28] // B + ABB | catalytic destruction: B + ABB <-> Z + ABB
            -0.001 * x.[2] // Z | destruction: b <-> Z
            0.001 * x.[6] // b | destruction: b <-> Z
            -0.001 * x.[2] // Z | destruction: B <-> Z
            0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[2] // Z | destruction: a <-> Z
            0.001 * x.[5] // a | destruction: a <-> Z
            -0.001 * x.[2] // Z | destruction: A <-> Z
            0.001 * x.[3] // A | destruction: A <-> Z
            -0.1 * x.[2] // Z | recycling: Z -> Y
            -10.0 * x.[2] // Z | waste: Z -> 
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[3]) * x.[3]
            0.238394029938642 * x.[5] * x.[38] // a + Abb | catalytic racemization: a + Abb -> A + Abb
            -0.238394029938642 * x.[3] * x.[60] // A + aBB | catalytic racemization: A + aBB -> a + aBB
            0.249990246537281 * x.[5] * x.[60] // a + aBB | catalytic racemization: a + aBB -> A + aBB
            -0.249990246537281 * x.[3] * x.[38] // A + Abb | catalytic racemization: A + Abb -> a + Abb
            0.001 * x.[5] // a | racemization: a -> A
            -0.001 * x.[3] // A | racemization: A -> a
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
            -0.445054985045952 * x.[3] * x.[76] // A + bBB | catalytic synthesis: Y + bBB <-> A + bBB
            0.391385000088169 * x.[1] * x.[76] // Y + bBB | catalytic synthesis: Y + bBB <-> A + bBB
            -0.134829646778709 * x.[3] * x.[54] // A + Bbb | catalytic synthesis: Y + Bbb <-> A + Bbb
            0.188499631736492 * x.[1] * x.[54] // Y + Bbb | catalytic synthesis: Y + Bbb <-> A + Bbb
            0.001 * x.[2] // Z | destruction: A <-> Z
            -0.001 * x.[3] // A | destruction: A <-> Z
            -0.001 * x.[3] // A | synthesis: Y <-> A
            0.001 * x.[1] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 4 - B
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[4]) * x.[4]
            0.001 * x.[6] // b | racemization: b -> B
            -0.001 * x.[4] // B | racemization: B -> b
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
            1.11881259639926 * x.[2] * x.[70] // Z + abb | catalytic destruction: B + abb <-> Z + abb
            -0.492781927695115 * x.[4] * x.[70] // B + abb | catalytic destruction: B + abb <-> Z + abb
            0.591857853097805 * x.[2] * x.[28] // Z + ABB | catalytic destruction: B + ABB <-> Z + ABB
            -1.21788852180195 * x.[4] * x.[28] // B + ABB | catalytic destruction: B + ABB <-> Z + ABB
            0.001 * x.[2] // Z | destruction: B <-> Z
            -0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[4] // B | synthesis: Y <-> B
            0.001 * x.[1] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 5 - a
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[5]) * x.[5]
            -0.238394029938642 * x.[5] * x.[38] // a + Abb | catalytic racemization: a + Abb -> A + Abb
            0.238394029938642 * x.[3] * x.[60] // A + aBB | catalytic racemization: A + aBB -> a + aBB
            -0.249990246537281 * x.[5] * x.[60] // a + aBB | catalytic racemization: a + aBB -> A + aBB
            0.249990246537281 * x.[3] * x.[38] // A + Abb | catalytic racemization: A + Abb -> a + Abb
            -0.001 * x.[5] // a | racemization: a -> A
            0.001 * x.[3] // A | racemization: A -> a
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
            -0.445054985045952 * x.[5] * x.[54] // a + Bbb | catalytic synthesis: Y + Bbb <-> a + Bbb
            0.391385000088169 * x.[1] * x.[54] // Y + Bbb | catalytic synthesis: Y + Bbb <-> a + Bbb
            -0.134829646778709 * x.[5] * x.[76] // a + bBB | catalytic synthesis: Y + bBB <-> a + bBB
            0.188499631736492 * x.[1] * x.[76] // Y + bBB | catalytic synthesis: Y + bBB <-> a + bBB
            0.001 * x.[2] // Z | destruction: a <-> Z
            -0.001 * x.[5] // a | destruction: a <-> Z
            -0.001 * x.[5] // a | synthesis: Y <-> a
            0.001 * x.[1] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 6 - b
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[6]) * x.[6]
            -0.001 * x.[6] // b | racemization: b -> B
            0.001 * x.[4] // B | racemization: B -> b
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
            1.11881259639926 * x.[2] * x.[28] // Z + ABB | catalytic destruction: b + ABB <-> Z + ABB
            -0.492781927695115 * x.[6] * x.[28] // b + ABB | catalytic destruction: b + ABB <-> Z + ABB
            0.591857853097805 * x.[2] * x.[70] // Z + abb | catalytic destruction: b + abb <-> Z + abb
            -1.21788852180195 * x.[6] * x.[70] // b + abb | catalytic destruction: b + abb <-> Z + abb
            0.001 * x.[2] // Z | destruction: b <-> Z
            -0.001 * x.[6] // b | destruction: b <-> Z
            -0.001 * x.[6] // b | synthesis: Y <-> b
            0.001 * x.[1] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 7 - AA
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[7]) * x.[7]
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
            -kW * (2.0 * xSum - x.[8]) * x.[8]
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
            -kW * (2.0 * xSum - x.[9]) * x.[9]
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
            -kW * (2.0 * xSum - x.[10]) * x.[10]
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
            -kW * (2.0 * xSum - x.[11]) * x.[11]
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
            -kW * (2.0 * xSum - x.[12]) * x.[12]
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
            -kW * (2.0 * xSum - x.[13]) * x.[13]
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
            -kW * (2.0 * xSum - x.[14]) * x.[14]
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
            -kW * (2.0 * xSum - x.[15]) * x.[15]
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
            -kW * (2.0 * xSum - x.[16]) * x.[16]
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
            -kW * (2.0 * xSum - x.[17]) * x.[17]
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
            -kW * (2.0 * xSum - x.[18]) * x.[18]
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
            -kW * (2.0 * xSum - x.[19]) * x.[19]
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
            -kW * (2.0 * xSum - x.[20]) * x.[20]
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
            -kW * (2.0 * xSum - x.[21]) * x.[21]
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
            -kW * (2.0 * xSum - x.[22]) * x.[22]
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
            -kW * (2.0 * xSum - x.[23]) * x.[23]
            -1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 24 - AAB
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[24]) * x.[24]
            -1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 25 - AAa
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[25]) * x.[25]
            -1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 26 - AAb
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[26]) * x.[26]
            -1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 27 - ABA
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[27]) * x.[27]
            -1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 28 - ABB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[28]) * x.[28]
            -1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 29 - ABa
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[29]) * x.[29]
            -1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 30 - ABb
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[30]) * x.[30]
            -1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 31 - AaA
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[31]) * x.[31]
            -1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 32 - AaB
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[32]) * x.[32]
            -1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 33 - Aaa
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[33]) * x.[33]
            -1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 34 - Aab
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[34]) * x.[34]
            -1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 35 - AbA
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[35]) * x.[35]
            -1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 36 - AbB
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[36]) * x.[36]
            -1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 37 - Aba
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[37]) * x.[37]
            -1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 38 - Abb
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[38]) * x.[38]
            -1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 39 - BAA
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[39]) * x.[39]
            -1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 40 - BAB
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[40]) * x.[40]
            -1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 41 - BAa
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[41]) * x.[41]
            -1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 42 - BAb
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[42]) * x.[42]
            -1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 43 - BBA
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[43]) * x.[43]
            -1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 44 - BBB
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[44]) * x.[44]
            -1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 45 - BBa
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[45]) * x.[45]
            -1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 46 - BBb
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[46]) * x.[46]
            -1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 47 - BaA
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[47]) * x.[47]
            -1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 48 - BaB
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[48]) * x.[48]
            -1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 49 - Baa
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[49]) * x.[49]
            -1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 50 - Bab
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[50]) * x.[50]
            -1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 51 - BbA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[51]) * x.[51]
            -1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 52 - BbB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[52]) * x.[52]
            -1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 53 - Bba
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[53]) * x.[53]
            -1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 54 - Bbb
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[54]) * x.[54]
            -1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 55 - aAA
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[55]) * x.[55]
            -1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 56 - aAB
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[56]) * x.[56]
            -1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 57 - aAa
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[57]) * x.[57]
            -1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 58 - aAb
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[58]) * x.[58]
            -1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 59 - aBA
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[59]) * x.[59]
            -1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 60 - aBB
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[60]) * x.[60]
            -1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 61 - aBa
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[61]) * x.[61]
            -1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 62 - aBb
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[62]) * x.[62]
            -1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 63 - aaA
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[63]) * x.[63]
            -1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 64 - aaB
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[64]) * x.[64]
            -1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 65 - aaa
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[65]) * x.[65]
            -1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 66 - aab
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[66]) * x.[66]
            -1.0 * x.[66] // aab | ligation: a + ab <-> aab
            1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 67 - abA
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[67]) * x.[67]
            -1.0 * x.[67] // abA | ligation: a + bA <-> abA
            1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 68 - abB
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[68]) * x.[68]
            -1.0 * x.[68] // abB | ligation: a + bB <-> abB
            1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 69 - aba
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[69]) * x.[69]
            -1.0 * x.[69] // aba | ligation: a + ba <-> aba
            1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 70 - abb
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[70]) * x.[70]
            -1.0 * x.[70] // abb | ligation: a + bb <-> abb
            1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 71 - bAA
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[71]) * x.[71]
            -1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 72 - bAB
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[72]) * x.[72]
            -1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 73 - bAa
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[73]) * x.[73]
            -1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 74 - bAb
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[74]) * x.[74]
            -1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 75 - bBA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[75]) * x.[75]
            -1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 76 - bBB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[76]) * x.[76]
            -1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 77 - bBa
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[77]) * x.[77]
            -1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 78 - bBb
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[78]) * x.[78]
            -1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 79 - baA
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[79]) * x.[79]
            -1.0 * x.[79] // baA | ligation: b + aA <-> baA
            1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 80 - baB
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[80]) * x.[80]
            -1.0 * x.[80] // baB | ligation: b + aB <-> baB
            1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 81 - baa
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[81]) * x.[81]
            -1.0 * x.[81] // baa | ligation: b + aa <-> baa
            1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 82 - bab
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[82]) * x.[82]
            -1.0 * x.[82] // bab | ligation: b + ab <-> bab
            1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 83 - bbA
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[83]) * x.[83]
            -1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 84 - bbB
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[84]) * x.[84]
            -1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 85 - bba
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[85]) * x.[85]
            -1.0 * x.[85] // bba | ligation: b + ba <-> bba
            1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 86 - bbb
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[86]) * x.[86]
            -1.0 * x.[86] // bbb | ligation: b + bb <-> bbb
            1.0 * x.[6] * x.[22] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

        let xSum = (x |> Array.sum) - x.[0]


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
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = "1.2.1.0"
                            versionNumber = "1.2.1.0"
                            seedValue = seedValue
                            modelName = "20181223_005"
                            numberOfSubstances = 87
                            numberOfAminoAcids = TwoAminoAcids
                            maxPeptideLength = ThreeMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution(2019351656, { threshold = None; scale = None; shift = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.001
                            }
                            |> SynthRndParam
                            |> SynthesisRateParam

                            {
                                catSynthDistribution = TriangularDistribution(1284402257, { threshold = Some 0.02; scale = None; shift = None }) |> Triangular
                                eeParams = 
                                {
                                    eeForwardDistribution = SymmetricTriangularDistribution(553519987, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    eeBackwardDistribution = SymmetricTriangularDistribution(1855451446, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    multiplier = 1000.0
                                }
                            }
                            |> CatSynthRndParam
                            |> CatalyticSynthesisRateParam

                            {
                                destructionDistribution = DeltaDistribution(1720151443, { threshold = None; scale = None; shift = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.001
                            }
                            |> DestrRndParam
                            |> DestructionRateParam

                            {
                                catDestrDistribution = TriangularDistribution(1893903767, { threshold = Some 0.02; scale = None; shift = None }) |> Triangular
                                eeParams = 
                                {
                                    eeForwardDistribution = SymmetricTriangularDistribution(1582178414, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    eeBackwardDistribution = SymmetricTriangularDistribution(1016589291, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    multiplier = 1000.0
                                }
                            }
                            |> CatDestrRndParam
                            |> CatalyticDestructionRateParam

                            {
                                ligationDistribution = DeltaDistribution(201502492, { threshold = None; scale = None; shift = None }) |> Delta
                                forwardScale = Some 1.0
                                backwardScale = Some 1.0
                            }
                            |> LigRndParam
                            |> LigationRateParam

                            {
                                racemizationDistribution = DeltaDistribution(737608590, { threshold = None; scale = None; shift = None }) |> Delta
                                forwardScale = Some 0.001
                            }
                            |> RacemRndParam
                            |> RacemizationRateParam

                            {
                                catRacemDistribution = TriangularDistribution(806147669, { threshold = Some 0.02; scale = None; shift = None }) |> Triangular
                                eeParams = 
                                {
                                    eeForwardDistribution = SymmetricTriangularDistribution(273839671, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    eeBackwardDistribution = SymmetricTriangularDistribution(2040729543, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    multiplier = 1000.0
                                }
                            }
                            |> CatRacemRndParam
                            |> CatalyticRacemizationRateParam

                            {
                                foodCreationRate = 0.01
                            }
                            |> FoodCreationRateParam

                            {
                                wasteRemovalRate = 10.0
                            }
                            |> WasteRemovalRateParam

                            {
                                wasteRecyclingRate = 0.1
                            }
                            |> WasteRecyclingRateParam

                            {
                                aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                                simSynthDistribution = UniformDistribution(767779821, { threshold = Some 0.2; scale = None; shift = None }) |> Uniform
                                getForwardEeDistr = DefaultEeDistributionGetter
                                getBackwardEeDistr = DefaultEeDistributionGetter
                                getMultiplierDistr = DefaultSimDistributionGetter
                            }
                            |> CatSynthSimParam
                            |> CatalyticSynthesisRateParam

                            {
                                simDestrDistribution = UniformDistribution(1859601915, { threshold = Some 0.2; scale = None; shift = None }) |> Uniform
                                aminoAcids = AminoAcid.getAminoAcids NumberOfAminoAcids.TwoAminoAcids
                            }
                            |> CatDestrSimParam
                            |> CatalyticDestructionRateParam

                            {
                                catLigationDistribution = TriangularDistribution(672846875, { threshold = Some 5E-05; scale = None; shift = None }) |> Triangular
                                eeParams = 
                                {
                                    eeForwardDistribution = SymmetricTriangularDistribution(1487016821, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    eeBackwardDistribution = SymmetricTriangularDistribution(1395184989, { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe |> Some
                                    multiplier = 2000.0
                                }
                            }
                            |> CatLigRndParam
                            |> CatalyticLigationRateParam

                            {
                                sedimentationDirectDistribution = TriangularDistribution(1641628916, { threshold = Some 2E-05; scale = None; shift = None }) |> Triangular
                                forwardScale = Some 10000.0
                            }
                            |> SedDirRndParam
                            |> SedimentationDirectRateParam

                            {
                                sedimentationAllDistribution = TriangularDistribution(639149230, { threshold = None; scale = None; shift = None }) |> Triangular
                                forwardScale = Some 0.1
                            }
                            |> SedAllRndParam
                            |> SedimentationAllRateParam

                            {
                                simRacemDistribution = UniformDistribution(1661647157, { threshold = Some 0.2; scale = None; shift = None }) |> Uniform
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
                    (FoodCreationName, 1)
                    (WasteRemovalName, 1)
                    (WasteRecyclingName, 1)
                    (SynthesisName, 4)
                    (DestructionName, 4)
                    (CatalyticSynthesisName, 320)
                    (CatalyticDestructionName, 320)
                    (LigationName, 39)
                    (CatalyticLigationName, 3120)
                    (SedimentationDirectName, 2331)
                    (SedimentationAllName, 4)
                    (RacemizationName, 4)
                    (CatalyticRacemizationName, 320)
                ]

            allReactions = 
                [
                    (FoodCreationName, 1)
                    (WasteRemovalName, 1)
                    (WasteRecyclingName, 1)
                    (SynthesisName, 4)
                    (DestructionName, 4)
                    (CatalyticSynthesisName, 4)
                    (CatalyticDestructionName, 4)
                    (LigationName, 78)
                    (RacemizationName, 4)
                    (CatalyticRacemizationName, 4)
                    (SedimentationAllName, 4)
                ]
        }


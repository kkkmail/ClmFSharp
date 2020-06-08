namespace Clm.Model

open System
open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmSys.ContGenPrimitives

module ModelData = 
    let seedValue = 1118393691
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
            x.[0] // abundant
            x.[1] // food
            x.[2] // waste
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
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[47] * x.[58] // BaA + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            -4404.73200206935 * x.[13] * x.[3] * x.[58] // Ba + A + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            112.941846206906 * x.[47] * x.[32] // BaA + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            -112.941846206906 * x.[13] * x.[3] * x.[32] // Ba + A + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            4404.73200206935 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            -4404.73200206935 * x.[9] * x.[3] * x.[58] // Aa + A + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            112.941846206906 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            -112.941846206906 * x.[9] * x.[3] * x.[32] // Aa + A + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            4404.73200206935 * x.[79] * x.[58] // baA + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            -4404.73200206935 * x.[21] * x.[3] * x.[58] // ba + A + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            112.941846206906 * x.[79] * x.[32] // baA + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            -112.941846206906 * x.[21] * x.[3] * x.[32] // ba + A + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            4404.73200206935 * x.[63] * x.[58] // aaA + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            -4404.73200206935 * x.[17] * x.[3] * x.[58] // aa + A + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            112.941846206906 * x.[63] * x.[32] // aaA + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            -112.941846206906 * x.[17] * x.[3] * x.[32] // aa + A + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            4404.73200206935 * x.[34] * x.[32] // Aab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            -4404.73200206935 * x.[3] * x.[18] * x.[32] // A + ab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            112.941846206906 * x.[34] * x.[58] // Aab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            -112.941846206906 * x.[3] * x.[18] * x.[58] // A + ab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            4404.73200206935 * x.[33] * x.[32] // Aaa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            -4404.73200206935 * x.[3] * x.[17] * x.[32] // A + aa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            112.941846206906 * x.[33] * x.[58] // Aaa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            -112.941846206906 * x.[3] * x.[17] * x.[58] // A + aa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            4404.73200206935 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -4404.73200206935 * x.[3] * x.[16] * x.[32] // A + aB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            112.941846206906 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            -112.941846206906 * x.[3] * x.[16] * x.[58] // A + aB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            4404.73200206935 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            -4404.73200206935 * x.[3] * x.[15] * x.[32] // A + aA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            112.941846206906 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            -112.941846206906 * x.[3] * x.[15] * x.[58] // A + aA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            4404.73200206935 * x.[15] * x.[58] // aA + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            -4404.73200206935 * x.[5] * x.[3] * x.[58] // a + A + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            4404.73200206935 * x.[9] * x.[32] // Aa + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            -4404.73200206935 * x.[3] * x.[5] * x.[32] // A + a + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            112.941846206906 * x.[15] * x.[32] // aA + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            -112.941846206906 * x.[5] * x.[3] * x.[32] // a + A + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            112.941846206906 * x.[9] * x.[58] // Aa + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            -112.941846206906 * x.[3] * x.[5] * x.[58] // A + a + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            4404.73200206935 * x.[37] * x.[32] // Aba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            -4404.73200206935 * x.[3] * x.[21] * x.[32] // A + ba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            112.941846206906 * x.[37] * x.[58] // Aba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            -112.941846206906 * x.[3] * x.[21] * x.[58] // A + ba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            4404.73200206935 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            -4404.73200206935 * x.[3] * x.[20] * x.[32] // A + bB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            112.941846206906 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            -112.941846206906 * x.[3] * x.[20] * x.[58] // A + bB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            4404.73200206935 * x.[35] * x.[32] // AbA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            -4404.73200206935 * x.[3] * x.[19] * x.[32] // A + bA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            112.941846206906 * x.[35] * x.[58] // AbA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            -112.941846206906 * x.[3] * x.[19] * x.[58] // A + bA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            4404.73200206935 * x.[10] * x.[32] // Ab + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            -4404.73200206935 * x.[3] * x.[6] * x.[32] // A + b + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            112.941846206906 * x.[10] * x.[58] // Ab + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            -112.941846206906 * x.[3] * x.[6] * x.[58] // A + b + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            4404.73200206935 * x.[38] * x.[32] // Abb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            -4404.73200206935 * x.[3] * x.[22] * x.[32] // A + bb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            112.941846206907 * x.[38] * x.[58] // Abb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            -112.941846206907 * x.[3] * x.[22] * x.[58] // A + bb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            1.0 * x.[75] // bBA | ligation: bB + A <-> bBA
            -1.0 * x.[20] * x.[3] // bB + A | ligation: bB + A <-> bBA
            1.0 * x.[51] // BbA | ligation: Bb + A <-> BbA
            -1.0 * x.[14] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            1.0 * x.[71] // bAA | ligation: bA + A <-> bAA
            -1.0 * x.[19] * x.[3] // bA + A | ligation: bA + A <-> bAA
            1.0 * x.[47] // BaA | ligation: Ba + A <-> BaA
            -1.0 * x.[13] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            1.0 * x.[83] // bbA | ligation: bb + A <-> bbA
            -1.0 * x.[22] * x.[3] // bb + A | ligation: bb + A <-> bbA
            1.0 * x.[43] // BBA | ligation: BB + A <-> BBA
            -1.0 * x.[12] * x.[3] // BB + A | ligation: BB + A <-> BBA
            1.0 * x.[79] // baA | ligation: ba + A <-> baA
            -1.0 * x.[21] * x.[3] // ba + A | ligation: ba + A <-> baA
            1.0 * x.[39] // BAA | ligation: BA + A <-> BAA
            -1.0 * x.[11] * x.[3] // BA + A | ligation: BA + A <-> BAA
            1.0 * x.[19] // bA | ligation: b + A <-> bA
            -1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[11] // BA | ligation: B + A <-> BA
            -1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[59] // aBA | ligation: aB + A <-> aBA
            -1.0 * x.[16] * x.[3] // aB + A | ligation: aB + A <-> aBA
            1.0 * x.[35] // AbA | ligation: Ab + A <-> AbA
            -1.0 * x.[10] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            1.0 * x.[55] // aAA | ligation: aA + A <-> aAA
            -1.0 * x.[15] * x.[3] // aA + A | ligation: aA + A <-> aAA
            1.0 * x.[31] // AaA | ligation: Aa + A <-> AaA
            -1.0 * x.[9] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            1.0 * x.[67] // abA | ligation: ab + A <-> abA
            -1.0 * x.[18] * x.[3] // ab + A | ligation: ab + A <-> abA
            1.0 * x.[27] // ABA | ligation: AB + A <-> ABA
            -1.0 * x.[8] * x.[3] // AB + A | ligation: AB + A <-> ABA
            1.0 * x.[63] // aaA | ligation: aa + A <-> aaA
            -1.0 * x.[17] * x.[3] // aa + A | ligation: aa + A <-> aaA
            1.0 * x.[23] // AAA | ligation: AA + A <-> AAA
            -1.0 * x.[7] * x.[3] // AA + A | ligation: AA + A <-> AAA
            1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
            1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[15] // aA | ligation: a + A <-> aA
            -1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
            1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
            1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
            1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
            1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
            1.0 * x.[8] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
            1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
            1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
            1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
            1.0 * x.[7] // AA | ligation: A + A <-> AA
            1.0 * x.[7] // AA | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 4 - B
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[48] * x.[58] // BaB + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            -4404.73200206935 * x.[13] * x.[4] * x.[58] // Ba + B + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            112.941846206906 * x.[48] * x.[32] // BaB + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            -112.941846206906 * x.[13] * x.[4] * x.[32] // Ba + B + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            4404.73200206935 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            -4404.73200206935 * x.[9] * x.[4] * x.[58] // Aa + B + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            112.941846206906 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -112.941846206906 * x.[9] * x.[4] * x.[32] // Aa + B + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            4404.73200206935 * x.[80] * x.[58] // baB + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            -4404.73200206935 * x.[21] * x.[4] * x.[58] // ba + B + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            112.941846206906 * x.[80] * x.[32] // baB + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            -112.941846206906 * x.[21] * x.[4] * x.[32] // ba + B + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            4404.73200206935 * x.[64] * x.[58] // aaB + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            -4404.73200206935 * x.[17] * x.[4] * x.[58] // aa + B + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            112.941846206906 * x.[64] * x.[32] // aaB + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            -112.941846206906 * x.[17] * x.[4] * x.[32] // aa + B + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            4404.73200206935 * x.[16] * x.[58] // aB + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            -4404.73200206935 * x.[5] * x.[4] * x.[58] // a + B + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            112.941846206906 * x.[16] * x.[32] // aB + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            -112.941846206906 * x.[5] * x.[4] * x.[32] // a + B + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            4404.73200206935 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            -4404.73200206935 * x.[14] * x.[4] * x.[58] // Bb + B + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            112.941846206906 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            -112.941846206906 * x.[14] * x.[4] * x.[32] // Bb + B + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            4404.73200206935 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            -4404.73200206935 * x.[10] * x.[4] * x.[58] // Ab + B + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            112.941846206906 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            -112.941846206906 * x.[10] * x.[4] * x.[32] // Ab + B + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            4404.73200206935 * x.[84] * x.[58] // bbB + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            -4404.73200206935 * x.[22] * x.[4] * x.[58] // bb + B + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            112.941846206906 * x.[84] * x.[32] // bbB + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            -112.941846206906 * x.[22] * x.[4] * x.[32] // bb + B + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            4404.73200206935 * x.[68] * x.[58] // abB + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            -4404.73200206935 * x.[18] * x.[4] * x.[58] // ab + B + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            112.941846206906 * x.[68] * x.[32] // abB + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            -112.941846206906 * x.[18] * x.[4] * x.[32] // ab + B + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            4404.73200206935 * x.[54] * x.[32] // Bbb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            -4404.73200206935 * x.[4] * x.[22] * x.[32] // B + bb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            112.941846206906 * x.[54] * x.[58] // Bbb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            -112.941846206906 * x.[4] * x.[22] * x.[58] // B + bb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            4404.73200206935 * x.[53] * x.[32] // Bba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            -4404.73200206935 * x.[4] * x.[21] * x.[32] // B + ba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            112.941846206906 * x.[53] * x.[58] // Bba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            -112.941846206906 * x.[4] * x.[21] * x.[58] // B + ba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            4404.73200206935 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            -4404.73200206935 * x.[4] * x.[20] * x.[32] // B + bB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            112.941846206906 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            -112.941846206906 * x.[4] * x.[20] * x.[58] // B + bB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            4404.73200206935 * x.[51] * x.[32] // BbA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            -4404.73200206935 * x.[4] * x.[19] * x.[32] // B + bA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            112.941846206906 * x.[51] * x.[58] // BbA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            -112.941846206906 * x.[4] * x.[19] * x.[58] // B + bA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            4404.73200206935 * x.[20] * x.[58] // bB + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            -4404.73200206935 * x.[6] * x.[4] * x.[58] // b + B + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            4404.73200206935 * x.[14] * x.[32] // Bb + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            -4404.73200206935 * x.[4] * x.[6] * x.[32] // B + b + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            112.941846206906 * x.[20] * x.[32] // bB + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            -112.941846206906 * x.[6] * x.[4] * x.[32] // b + B + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            112.941846206906 * x.[14] * x.[58] // Bb + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            -112.941846206906 * x.[4] * x.[6] * x.[58] // B + b + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            1.0 * x.[76] // bBB | ligation: bB + B <-> bBB
            -1.0 * x.[20] * x.[4] // bB + B | ligation: bB + B <-> bBB
            1.0 * x.[52] // BbB | ligation: Bb + B <-> BbB
            -1.0 * x.[14] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            1.0 * x.[72] // bAB | ligation: bA + B <-> bAB
            -1.0 * x.[19] * x.[4] // bA + B | ligation: bA + B <-> bAB
            1.0 * x.[48] // BaB | ligation: Ba + B <-> BaB
            -1.0 * x.[13] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            1.0 * x.[84] // bbB | ligation: bb + B <-> bbB
            -1.0 * x.[22] * x.[4] // bb + B | ligation: bb + B <-> bbB
            1.0 * x.[44] // BBB | ligation: BB + B <-> BBB
            -1.0 * x.[12] * x.[4] // BB + B | ligation: BB + B <-> BBB
            1.0 * x.[80] // baB | ligation: ba + B <-> baB
            -1.0 * x.[21] * x.[4] // ba + B | ligation: ba + B <-> baB
            1.0 * x.[40] // BAB | ligation: BA + B <-> BAB
            -1.0 * x.[11] * x.[4] // BA + B | ligation: BA + B <-> BAB
            1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[20] // bB | ligation: b + B <-> bB
            -1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
            1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[12] // BB | ligation: B + B <-> BB
            1.0 * x.[12] // BB | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[11] // BA | ligation: B + A <-> BA
            -1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[60] // aBB | ligation: aB + B <-> aBB
            -1.0 * x.[16] * x.[4] // aB + B | ligation: aB + B <-> aBB
            1.0 * x.[36] // AbB | ligation: Ab + B <-> AbB
            -1.0 * x.[10] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            1.0 * x.[56] // aAB | ligation: aA + B <-> aAB
            -1.0 * x.[15] * x.[4] // aA + B | ligation: aA + B <-> aAB
            1.0 * x.[32] // AaB | ligation: Aa + B <-> AaB
            -1.0 * x.[9] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            1.0 * x.[68] // abB | ligation: ab + B <-> abB
            -1.0 * x.[18] * x.[4] // ab + B | ligation: ab + B <-> abB
            1.0 * x.[28] // ABB | ligation: AB + B <-> ABB
            -1.0 * x.[8] * x.[4] // AB + B | ligation: AB + B <-> ABB
            1.0 * x.[64] // aaB | ligation: aa + B <-> aaB
            -1.0 * x.[17] * x.[4] // aa + B | ligation: aa + B <-> aaB
            1.0 * x.[24] // AAB | ligation: AA + B <-> AAB
            -1.0 * x.[7] * x.[4] // AA + B | ligation: AA + B <-> AAB
            1.0 * x.[16] // aB | ligation: a + B <-> aB
            -1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[8] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 5 - a
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[73] * x.[32] // bAa + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            -4404.73200206935 * x.[19] * x.[5] * x.[32] // bA + a + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            112.941846206906 * x.[73] * x.[58] // bAa + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            -112.941846206906 * x.[19] * x.[5] * x.[58] // bA + a + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            4404.73200206935 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            -4404.73200206935 * x.[15] * x.[5] * x.[32] // aA + a + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            112.941846206906 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            -112.941846206906 * x.[15] * x.[5] * x.[58] // aA + a + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            4404.73200206935 * x.[41] * x.[32] // BAa + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            -4404.73200206935 * x.[11] * x.[5] * x.[32] // BA + a + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            112.941846206906 * x.[41] * x.[58] // BAa + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            -112.941846206906 * x.[11] * x.[5] * x.[58] // BA + a + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            4404.73200206935 * x.[25] * x.[32] // AAa + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            -4404.73200206935 * x.[7] * x.[5] * x.[32] // AA + a + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            112.941846206906 * x.[25] * x.[58] // AAa + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            -112.941846206906 * x.[7] * x.[5] * x.[58] // AA + a + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            4404.73200206935 * x.[56] * x.[58] // aAB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            -4404.73200206935 * x.[5] * x.[8] * x.[58] // a + AB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            112.941846206906 * x.[56] * x.[32] // aAB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            -112.941846206906 * x.[5] * x.[8] * x.[32] // a + AB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            4404.73200206935 * x.[55] * x.[58] // aAA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            -4404.73200206935 * x.[5] * x.[7] * x.[58] // a + AA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            112.941846206906 * x.[55] * x.[32] // aAA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            -112.941846206906 * x.[5] * x.[7] * x.[32] // a + AA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            4404.73200206935 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -4404.73200206935 * x.[5] * x.[10] * x.[58] // a + Ab + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            112.941846206906 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            -112.941846206906 * x.[5] * x.[10] * x.[32] // a + Ab + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            4404.73200206935 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            -4404.73200206935 * x.[5] * x.[9] * x.[58] // a + Aa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            112.941846206906 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            -112.941846206906 * x.[5] * x.[9] * x.[32] // a + Aa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            4404.73200206935 * x.[15] * x.[58] // aA + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            -4404.73200206935 * x.[5] * x.[3] * x.[58] // a + A + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            4404.73200206935 * x.[9] * x.[32] // Aa + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            -4404.73200206935 * x.[3] * x.[5] * x.[32] // A + a + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            112.941846206906 * x.[15] * x.[32] // aA + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            -112.941846206906 * x.[5] * x.[3] * x.[32] // a + A + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            112.941846206906 * x.[9] * x.[58] // Aa + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            -112.941846206906 * x.[3] * x.[5] * x.[58] // A + a + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            4404.73200206935 * x.[59] * x.[58] // aBA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            -4404.73200206935 * x.[5] * x.[11] * x.[58] // a + BA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            112.941846206906 * x.[59] * x.[32] // aBA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            -112.941846206906 * x.[5] * x.[11] * x.[32] // a + BA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            4404.73200206935 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            -4404.73200206935 * x.[5] * x.[14] * x.[58] // a + Bb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            112.941846206906 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            -112.941846206906 * x.[5] * x.[14] * x.[32] // a + Bb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            4404.73200206935 * x.[61] * x.[58] // aBa + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            -4404.73200206935 * x.[5] * x.[13] * x.[58] // a + Ba + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            112.941846206906 * x.[61] * x.[32] // aBa + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            -112.941846206906 * x.[5] * x.[13] * x.[32] // a + Ba + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            4404.73200206935 * x.[16] * x.[58] // aB + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            -4404.73200206935 * x.[5] * x.[4] * x.[58] // a + B + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            112.941846206906 * x.[16] * x.[32] // aB + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            -112.941846206906 * x.[5] * x.[4] * x.[32] // a + B + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            4404.73200206935 * x.[60] * x.[58] // aBB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            -4404.73200206935 * x.[5] * x.[12] * x.[58] // a + BB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            112.941846206907 * x.[60] * x.[32] // aBB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            -112.941846206907 * x.[5] * x.[12] * x.[32] // a + BB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            1.0 * x.[53] // Bba | ligation: Bb + a <-> Bba
            -1.0 * x.[14] * x.[5] // Bb + a | ligation: Bb + a <-> Bba
            1.0 * x.[77] // bBa | ligation: bB + a <-> bBa
            -1.0 * x.[20] * x.[5] // bB + a | ligation: bB + a <-> bBa
            1.0 * x.[49] // Baa | ligation: Ba + a <-> Baa
            -1.0 * x.[13] * x.[5] // Ba + a | ligation: Ba + a <-> Baa
            1.0 * x.[73] // bAa | ligation: bA + a <-> bAa
            -1.0 * x.[19] * x.[5] // bA + a | ligation: bA + a <-> bAa
            1.0 * x.[45] // BBa | ligation: BB + a <-> BBa
            -1.0 * x.[12] * x.[5] // BB + a | ligation: BB + a <-> BBa
            1.0 * x.[85] // bba | ligation: bb + a <-> bba
            -1.0 * x.[22] * x.[5] // bb + a | ligation: bb + a <-> bba
            1.0 * x.[41] // BAa | ligation: BA + a <-> BAa
            -1.0 * x.[11] * x.[5] // BA + a | ligation: BA + a <-> BAa
            1.0 * x.[81] // baa | ligation: ba + a <-> baa
            -1.0 * x.[21] * x.[5] // ba + a | ligation: ba + a <-> baa
            1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[21] // ba | ligation: b + a <-> ba
            -1.0 * x.[6] * x.[5] // b + a | ligation: b + a <-> ba
            1.0 * x.[37] // Aba | ligation: Ab + a <-> Aba
            -1.0 * x.[10] * x.[5] // Ab + a | ligation: Ab + a <-> Aba
            1.0 * x.[61] // aBa | ligation: aB + a <-> aBa
            -1.0 * x.[16] * x.[5] // aB + a | ligation: aB + a <-> aBa
            1.0 * x.[33] // Aaa | ligation: Aa + a <-> Aaa
            -1.0 * x.[9] * x.[5] // Aa + a | ligation: Aa + a <-> Aaa
            1.0 * x.[57] // aAa | ligation: aA + a <-> aAa
            -1.0 * x.[15] * x.[5] // aA + a | ligation: aA + a <-> aAa
            1.0 * x.[29] // ABa | ligation: AB + a <-> ABa
            -1.0 * x.[8] * x.[5] // AB + a | ligation: AB + a <-> ABa
            1.0 * x.[69] // aba | ligation: ab + a <-> aba
            -1.0 * x.[18] * x.[5] // ab + a | ligation: ab + a <-> aba
            1.0 * x.[25] // AAa | ligation: AA + a <-> AAa
            -1.0 * x.[7] * x.[5] // AA + a | ligation: AA + a <-> AAa
            1.0 * x.[65] // aaa | ligation: aa + a <-> aaa
            -1.0 * x.[17] * x.[5] // aa + a | ligation: aa + a <-> aaa
            1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[16] // aB | ligation: a + B <-> aB
            -1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[15] // aA | ligation: a + A <-> aA
            -1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
            1.0 * x.[68] // abB | ligation: a + bB <-> abB
            -1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
            1.0 * x.[67] // abA | ligation: a + bA <-> abA
            -1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
            1.0 * x.[70] // abb | ligation: a + bb <-> abb
            -1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
            1.0 * x.[69] // aba | ligation: a + ba <-> aba
            -1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
            1.0 * x.[18] // ab | ligation: a + b <-> ab
            -1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
            1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
            1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
            1.0 * x.[66] // aab | ligation: a + ab <-> aab
            -1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
            1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
            1.0 * x.[17] // aa | ligation: a + a <-> aa
            1.0 * x.[17] // aa | ligation: a + a <-> aa
            -1.0 * x.[5] * x.[5] // a + a | ligation: a + a <-> aa
            -1.0 * x.[5] * x.[5] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 6 - b
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[74] * x.[32] // bAb + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            -4404.73200206935 * x.[19] * x.[6] * x.[32] // bA + b + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            112.941846206906 * x.[74] * x.[58] // bAb + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            -112.941846206906 * x.[19] * x.[6] * x.[58] // bA + b + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            4404.73200206935 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            -4404.73200206935 * x.[15] * x.[6] * x.[32] // aA + b + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            112.941846206906 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -112.941846206906 * x.[15] * x.[6] * x.[58] // aA + b + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            4404.73200206935 * x.[42] * x.[32] // BAb + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            -4404.73200206935 * x.[11] * x.[6] * x.[32] // BA + b + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            112.941846206906 * x.[42] * x.[58] // BAb + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            -112.941846206906 * x.[11] * x.[6] * x.[58] // BA + b + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            4404.73200206935 * x.[26] * x.[32] // AAb + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            -4404.73200206935 * x.[7] * x.[6] * x.[32] // AA + b + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            112.941846206906 * x.[26] * x.[58] // AAb + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            -112.941846206906 * x.[7] * x.[6] * x.[58] // AA + b + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            4404.73200206935 * x.[10] * x.[32] // Ab + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            -4404.73200206935 * x.[3] * x.[6] * x.[32] // A + b + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            112.941846206906 * x.[10] * x.[58] // Ab + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            -112.941846206906 * x.[3] * x.[6] * x.[58] // A + b + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            4404.73200206935 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            -4404.73200206935 * x.[20] * x.[6] * x.[32] // bB + b + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            112.941846206906 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            -112.941846206906 * x.[20] * x.[6] * x.[58] // bB + b + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            4404.73200206935 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            -4404.73200206935 * x.[16] * x.[6] * x.[32] // aB + b + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            112.941846206906 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            -112.941846206906 * x.[16] * x.[6] * x.[58] // aB + b + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            4404.73200206935 * x.[46] * x.[32] // BBb + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            -4404.73200206935 * x.[12] * x.[6] * x.[32] // BB + b + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            112.941846206906 * x.[46] * x.[58] // BBb + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            -112.941846206906 * x.[12] * x.[6] * x.[58] // BB + b + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            4404.73200206935 * x.[30] * x.[32] // ABb + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            -4404.73200206935 * x.[8] * x.[6] * x.[32] // AB + b + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            112.941846206906 * x.[30] * x.[58] // ABb + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            -112.941846206906 * x.[8] * x.[6] * x.[58] // AB + b + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            4404.73200206935 * x.[76] * x.[58] // bBB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            -4404.73200206935 * x.[6] * x.[12] * x.[58] // b + BB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            112.941846206906 * x.[76] * x.[32] // bBB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            -112.941846206906 * x.[6] * x.[12] * x.[32] // b + BB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            4404.73200206935 * x.[75] * x.[58] // bBA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            -4404.73200206935 * x.[6] * x.[11] * x.[58] // b + BA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            112.941846206906 * x.[75] * x.[32] // bBA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            -112.941846206906 * x.[6] * x.[11] * x.[32] // b + BA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            4404.73200206935 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            -4404.73200206935 * x.[6] * x.[14] * x.[58] // b + Bb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            112.941846206906 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            -112.941846206906 * x.[6] * x.[14] * x.[32] // b + Bb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            4404.73200206935 * x.[77] * x.[58] // bBa + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            -4404.73200206935 * x.[6] * x.[13] * x.[58] // b + Ba + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            112.941846206906 * x.[77] * x.[32] // bBa + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            -112.941846206906 * x.[6] * x.[13] * x.[32] // b + Ba + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            4404.73200206935 * x.[20] * x.[58] // bB + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            -4404.73200206935 * x.[6] * x.[4] * x.[58] // b + B + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            4404.73200206935 * x.[14] * x.[32] // Bb + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            -4404.73200206935 * x.[4] * x.[6] * x.[32] // B + b + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            112.941846206906 * x.[20] * x.[32] // bB + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            -112.941846206906 * x.[6] * x.[4] * x.[32] // b + B + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            112.941846206906 * x.[14] * x.[58] // Bb + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            -112.941846206906 * x.[4] * x.[6] * x.[58] // B + b + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            1.0 * x.[54] // Bbb | ligation: Bb + b <-> Bbb
            -1.0 * x.[14] * x.[6] // Bb + b | ligation: Bb + b <-> Bbb
            1.0 * x.[78] // bBb | ligation: bB + b <-> bBb
            -1.0 * x.[20] * x.[6] // bB + b | ligation: bB + b <-> bBb
            1.0 * x.[50] // Bab | ligation: Ba + b <-> Bab
            -1.0 * x.[13] * x.[6] // Ba + b | ligation: Ba + b <-> Bab
            1.0 * x.[74] // bAb | ligation: bA + b <-> bAb
            -1.0 * x.[19] * x.[6] // bA + b | ligation: bA + b <-> bAb
            1.0 * x.[46] // BBb | ligation: BB + b <-> BBb
            -1.0 * x.[12] * x.[6] // BB + b | ligation: BB + b <-> BBb
            1.0 * x.[86] // bbb | ligation: bb + b <-> bbb
            -1.0 * x.[22] * x.[6] // bb + b | ligation: bb + b <-> bbb
            1.0 * x.[42] // BAb | ligation: BA + b <-> BAb
            -1.0 * x.[11] * x.[6] // BA + b | ligation: BA + b <-> BAb
            1.0 * x.[82] // bab | ligation: ba + b <-> bab
            -1.0 * x.[21] * x.[6] // ba + b | ligation: ba + b <-> bab
            1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[20] // bB | ligation: b + B <-> bB
            -1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[19] // bA | ligation: b + A <-> bA
            -1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[86] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[6] * x.[22] // b + bb | ligation: b + bb <-> bbb
            1.0 * x.[85] // bba | ligation: b + ba <-> bba
            -1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[22] // bb | ligation: b + b <-> bb
            1.0 * x.[22] // bb | ligation: b + b <-> bb
            -1.0 * x.[6] * x.[6] // b + b | ligation: b + b <-> bb
            -1.0 * x.[6] * x.[6] // b + b | ligation: b + b <-> bb
            1.0 * x.[80] // baB | ligation: b + aB <-> baB
            -1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[79] // baA | ligation: b + aA <-> baA
            -1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[82] // bab | ligation: b + ab <-> bab
            -1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[81] // baa | ligation: b + aa <-> baa
            -1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[21] // ba | ligation: b + a <-> ba
            -1.0 * x.[6] * x.[5] // b + a | ligation: b + a <-> ba
            1.0 * x.[38] // Abb | ligation: Ab + b <-> Abb
            -1.0 * x.[10] * x.[6] // Ab + b | ligation: Ab + b <-> Abb
            1.0 * x.[62] // aBb | ligation: aB + b <-> aBb
            -1.0 * x.[16] * x.[6] // aB + b | ligation: aB + b <-> aBb
            1.0 * x.[34] // Aab | ligation: Aa + b <-> Aab
            -1.0 * x.[9] * x.[6] // Aa + b | ligation: Aa + b <-> Aab
            1.0 * x.[58] // aAb | ligation: aA + b <-> aAb
            -1.0 * x.[15] * x.[6] // aA + b | ligation: aA + b <-> aAb
            1.0 * x.[30] // ABb | ligation: AB + b <-> ABb
            -1.0 * x.[8] * x.[6] // AB + b | ligation: AB + b <-> ABb
            1.0 * x.[70] // abb | ligation: ab + b <-> abb
            -1.0 * x.[18] * x.[6] // ab + b | ligation: ab + b <-> abb
            1.0 * x.[26] // AAb | ligation: AA + b <-> AAb
            -1.0 * x.[7] * x.[6] // AA + b | ligation: AA + b <-> AAb
            1.0 * x.[66] // aab | ligation: aa + b <-> aab
            -1.0 * x.[17] * x.[6] // aa + b | ligation: aa + b <-> aab
            1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
            1.0 * x.[18] // ab | ligation: a + b <-> ab
            -1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 7 - AA
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[25] * x.[32] // AAa + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            -4404.73200206935 * x.[7] * x.[5] * x.[32] // AA + a + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            112.941846206906 * x.[25] * x.[58] // AAa + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            -112.941846206906 * x.[7] * x.[5] * x.[58] // AA + a + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            4404.73200206935 * x.[55] * x.[58] // aAA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            -4404.73200206935 * x.[5] * x.[7] * x.[58] // a + AA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            112.941846206906 * x.[55] * x.[32] // aAA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            -112.941846206906 * x.[5] * x.[7] * x.[32] // a + AA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            4404.73200206935 * x.[26] * x.[32] // AAb + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            -4404.73200206935 * x.[7] * x.[6] * x.[32] // AA + b + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            112.941846206906 * x.[26] * x.[58] // AAb + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            -112.941846206906 * x.[7] * x.[6] * x.[58] // AA + b + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[26] // AAb | ligation: AA + b <-> AAb
            -1.0 * x.[7] * x.[6] // AA + b | ligation: AA + b <-> AAb
            1.0 * x.[25] // AAa | ligation: AA + a <-> AAa
            -1.0 * x.[7] * x.[5] // AA + a | ligation: AA + a <-> AAa
            1.0 * x.[24] // AAB | ligation: AA + B <-> AAB
            -1.0 * x.[7] * x.[4] // AA + B | ligation: AA + B <-> AAB
            1.0 * x.[23] // AAA | ligation: AA + A <-> AAA
            -1.0 * x.[7] * x.[3] // AA + A | ligation: AA + A <-> AAA
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
            4404.73200206935 * x.[56] * x.[58] // aAB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            -4404.73200206935 * x.[5] * x.[8] * x.[58] // a + AB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            112.941846206906 * x.[56] * x.[32] // aAB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            -112.941846206906 * x.[5] * x.[8] * x.[32] // a + AB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            4404.73200206935 * x.[30] * x.[32] // ABb + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            -4404.73200206935 * x.[8] * x.[6] * x.[32] // AB + b + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            112.941846206906 * x.[30] * x.[58] // ABb + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            -112.941846206906 * x.[8] * x.[6] * x.[58] // AB + b + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[30] // ABb | ligation: AB + b <-> ABb
            -1.0 * x.[8] * x.[6] // AB + b | ligation: AB + b <-> ABb
            1.0 * x.[29] // ABa | ligation: AB + a <-> ABa
            -1.0 * x.[8] * x.[5] // AB + a | ligation: AB + a <-> ABa
            1.0 * x.[28] // ABB | ligation: AB + B <-> ABB
            -1.0 * x.[8] * x.[4] // AB + B | ligation: AB + B <-> ABB
            1.0 * x.[27] // ABA | ligation: AB + A <-> ABA
            -1.0 * x.[8] * x.[3] // AB + A | ligation: AB + A <-> ABA
            1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
            -1.0 * x.[8] // AB | ligation: A + B <-> AB
            1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 9 - Aa
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            -4404.73200206935 * x.[9] * x.[3] * x.[58] // Aa + A + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            112.941846206906 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            -112.941846206906 * x.[9] * x.[3] * x.[32] // Aa + A + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            4404.73200206935 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            -4404.73200206935 * x.[5] * x.[9] * x.[58] // a + Aa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            112.941846206906 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            -112.941846206906 * x.[5] * x.[9] * x.[32] // a + Aa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            -4404.73200206935 * x.[9] * x.[32] // Aa + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            4404.73200206935 * x.[3] * x.[5] * x.[32] // A + a + AaB | catalytic ligation: A + a + AaB <-> Aa + AaB
            -112.941846206906 * x.[9] * x.[58] // Aa + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            112.941846206906 * x.[3] * x.[5] * x.[58] // A + a + aAb | catalytic ligation: A + a + aAb <-> Aa + aAb
            4404.73200206935 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            -4404.73200206935 * x.[9] * x.[4] * x.[58] // Aa + B + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            112.941846206906 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -112.941846206906 * x.[9] * x.[4] * x.[32] // Aa + B + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[34] // Aab | ligation: Aa + b <-> Aab
            -1.0 * x.[9] * x.[6] // Aa + b | ligation: Aa + b <-> Aab
            1.0 * x.[33] // Aaa | ligation: Aa + a <-> Aaa
            -1.0 * x.[9] * x.[5] // Aa + a | ligation: Aa + a <-> Aaa
            1.0 * x.[32] // AaB | ligation: Aa + B <-> AaB
            -1.0 * x.[9] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            1.0 * x.[31] // AaA | ligation: Aa + A <-> AaA
            -1.0 * x.[9] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
            -1.0 * x.[9] // Aa | ligation: A + a <-> Aa
            1.0 * x.[3] * x.[5] // A + a | ligation: A + a <-> Aa
            1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 10 - Ab
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -4404.73200206935 * x.[5] * x.[10] * x.[58] // a + Ab + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            112.941846206906 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            -112.941846206906 * x.[5] * x.[10] * x.[32] // a + Ab + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            -4404.73200206935 * x.[10] * x.[32] // Ab + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            4404.73200206935 * x.[3] * x.[6] * x.[32] // A + b + AaB | catalytic ligation: A + b + AaB <-> Ab + AaB
            -112.941846206906 * x.[10] * x.[58] // Ab + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            112.941846206906 * x.[3] * x.[6] * x.[58] // A + b + aAb | catalytic ligation: A + b + aAb <-> Ab + aAb
            4404.73200206935 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            -4404.73200206935 * x.[10] * x.[4] * x.[58] // Ab + B + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            112.941846206906 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            -112.941846206906 * x.[10] * x.[4] * x.[32] // Ab + B + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[38] // Abb | ligation: Ab + b <-> Abb
            -1.0 * x.[10] * x.[6] // Ab + b | ligation: Ab + b <-> Abb
            1.0 * x.[37] // Aba | ligation: Ab + a <-> Aba
            -1.0 * x.[10] * x.[5] // Ab + a | ligation: Ab + a <-> Aba
            1.0 * x.[36] // AbB | ligation: Ab + B <-> AbB
            -1.0 * x.[10] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            1.0 * x.[35] // AbA | ligation: Ab + A <-> AbA
            -1.0 * x.[10] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            -1.0 * x.[10] // Ab | ligation: A + b <-> Ab
            1.0 * x.[3] * x.[6] // A + b | ligation: A + b <-> Ab
            1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 11 - BA
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[41] * x.[32] // BAa + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            -4404.73200206935 * x.[11] * x.[5] * x.[32] // BA + a + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            112.941846206906 * x.[41] * x.[58] // BAa + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            -112.941846206906 * x.[11] * x.[5] * x.[58] // BA + a + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            4404.73200206935 * x.[42] * x.[32] // BAb + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            -4404.73200206935 * x.[11] * x.[6] * x.[32] // BA + b + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            112.941846206906 * x.[42] * x.[58] // BAb + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            -112.941846206906 * x.[11] * x.[6] * x.[58] // BA + b + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            4404.73200206935 * x.[59] * x.[58] // aBA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            -4404.73200206935 * x.[5] * x.[11] * x.[58] // a + BA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            112.941846206906 * x.[59] * x.[32] // aBA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            -112.941846206906 * x.[5] * x.[11] * x.[32] // a + BA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            4404.73200206935 * x.[75] * x.[58] // bBA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            -4404.73200206935 * x.[6] * x.[11] * x.[58] // b + BA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            112.941846206906 * x.[75] * x.[32] // bBA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            -112.941846206906 * x.[6] * x.[11] * x.[32] // b + BA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            1.0 * x.[42] // BAb | ligation: BA + b <-> BAb
            -1.0 * x.[11] * x.[6] // BA + b | ligation: BA + b <-> BAb
            1.0 * x.[41] // BAa | ligation: BA + a <-> BAa
            -1.0 * x.[11] * x.[5] // BA + a | ligation: BA + a <-> BAa
            1.0 * x.[40] // BAB | ligation: BA + B <-> BAB
            -1.0 * x.[11] * x.[4] // BA + B | ligation: BA + B <-> BAB
            1.0 * x.[39] // BAA | ligation: BA + A <-> BAA
            -1.0 * x.[11] * x.[3] // BA + A | ligation: BA + A <-> BAA
            1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
            -1.0 * x.[11] // BA | ligation: B + A <-> BA
            1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 12 - BB
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[46] * x.[32] // BBb + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            -4404.73200206935 * x.[12] * x.[6] * x.[32] // BB + b + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            112.941846206906 * x.[46] * x.[58] // BBb + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            -112.941846206906 * x.[12] * x.[6] * x.[58] // BB + b + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            4404.73200206935 * x.[76] * x.[58] // bBB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            -4404.73200206935 * x.[6] * x.[12] * x.[58] // b + BB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            112.941846206906 * x.[76] * x.[32] // bBB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            -112.941846206906 * x.[6] * x.[12] * x.[32] // b + BB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            4404.73200206935 * x.[60] * x.[58] // aBB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            -4404.73200206935 * x.[5] * x.[12] * x.[58] // a + BB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            112.941846206907 * x.[60] * x.[32] // aBB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            -112.941846206907 * x.[5] * x.[12] * x.[32] // a + BB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            1.0 * x.[46] // BBb | ligation: BB + b <-> BBb
            -1.0 * x.[12] * x.[6] // BB + b | ligation: BB + b <-> BBb
            1.0 * x.[45] // BBa | ligation: BB + a <-> BBa
            -1.0 * x.[12] * x.[5] // BB + a | ligation: BB + a <-> BBa
            1.0 * x.[44] // BBB | ligation: BB + B <-> BBB
            -1.0 * x.[12] * x.[4] // BB + B | ligation: BB + B <-> BBB
            1.0 * x.[43] // BBA | ligation: BB + A <-> BBA
            -1.0 * x.[12] * x.[3] // BB + A | ligation: BB + A <-> BBA
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
            4404.73200206935 * x.[47] * x.[58] // BaA + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            -4404.73200206935 * x.[13] * x.[3] * x.[58] // Ba + A + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            112.941846206906 * x.[47] * x.[32] // BaA + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            -112.941846206906 * x.[13] * x.[3] * x.[32] // Ba + A + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            4404.73200206935 * x.[48] * x.[58] // BaB + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            -4404.73200206935 * x.[13] * x.[4] * x.[58] // Ba + B + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            112.941846206906 * x.[48] * x.[32] // BaB + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            -112.941846206906 * x.[13] * x.[4] * x.[32] // Ba + B + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            4404.73200206935 * x.[61] * x.[58] // aBa + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            -4404.73200206935 * x.[5] * x.[13] * x.[58] // a + Ba + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            112.941846206906 * x.[61] * x.[32] // aBa + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            -112.941846206906 * x.[5] * x.[13] * x.[32] // a + Ba + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            4404.73200206935 * x.[77] * x.[58] // bBa + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            -4404.73200206935 * x.[6] * x.[13] * x.[58] // b + Ba + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            112.941846206906 * x.[77] * x.[32] // bBa + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            -112.941846206906 * x.[6] * x.[13] * x.[32] // b + Ba + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            1.0 * x.[50] // Bab | ligation: Ba + b <-> Bab
            -1.0 * x.[13] * x.[6] // Ba + b | ligation: Ba + b <-> Bab
            1.0 * x.[49] // Baa | ligation: Ba + a <-> Baa
            -1.0 * x.[13] * x.[5] // Ba + a | ligation: Ba + a <-> Baa
            1.0 * x.[48] // BaB | ligation: Ba + B <-> BaB
            -1.0 * x.[13] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            1.0 * x.[47] // BaA | ligation: Ba + A <-> BaA
            -1.0 * x.[13] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
            -1.0 * x.[13] // Ba | ligation: B + a <-> Ba
            1.0 * x.[4] * x.[5] // B + a | ligation: B + a <-> Ba
            1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 14 - Bb
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            -4404.73200206935 * x.[5] * x.[14] * x.[58] // a + Bb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            112.941846206906 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            -112.941846206906 * x.[5] * x.[14] * x.[32] // a + Bb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            4404.73200206935 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            -4404.73200206935 * x.[14] * x.[4] * x.[58] // Bb + B + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            112.941846206906 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            -112.941846206906 * x.[14] * x.[4] * x.[32] // Bb + B + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            4404.73200206935 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            -4404.73200206935 * x.[6] * x.[14] * x.[58] // b + Bb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            112.941846206906 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            -112.941846206906 * x.[6] * x.[14] * x.[32] // b + Bb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            -4404.73200206935 * x.[14] * x.[32] // Bb + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            4404.73200206935 * x.[4] * x.[6] * x.[32] // B + b + AaB | catalytic ligation: B + b + AaB <-> Bb + AaB
            -112.941846206906 * x.[14] * x.[58] // Bb + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            112.941846206906 * x.[4] * x.[6] * x.[58] // B + b + aAb | catalytic ligation: B + b + aAb <-> Bb + aAb
            1.0 * x.[54] // Bbb | ligation: Bb + b <-> Bbb
            -1.0 * x.[14] * x.[6] // Bb + b | ligation: Bb + b <-> Bbb
            1.0 * x.[53] // Bba | ligation: Bb + a <-> Bba
            -1.0 * x.[14] * x.[5] // Bb + a | ligation: Bb + a <-> Bba
            1.0 * x.[52] // BbB | ligation: Bb + B <-> BbB
            -1.0 * x.[14] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            1.0 * x.[51] // BbA | ligation: Bb + A <-> BbA
            -1.0 * x.[14] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
            -1.0 * x.[14] // Bb | ligation: B + b <-> Bb
            1.0 * x.[4] * x.[6] // B + b | ligation: B + b <-> Bb
            1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 15 - aA
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            -4404.73200206935 * x.[15] * x.[5] * x.[32] // aA + a + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            112.941846206906 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            -112.941846206906 * x.[15] * x.[5] * x.[58] // aA + a + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            4404.73200206935 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            -4404.73200206935 * x.[3] * x.[15] * x.[32] // A + aA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            112.941846206906 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            -112.941846206906 * x.[3] * x.[15] * x.[58] // A + aA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            -4404.73200206935 * x.[15] * x.[58] // aA + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            4404.73200206935 * x.[5] * x.[3] * x.[58] // a + A + aAb | catalytic ligation: a + A + aAb <-> aA + aAb
            -112.941846206906 * x.[15] * x.[32] // aA + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            112.941846206906 * x.[5] * x.[3] * x.[32] // a + A + AaB | catalytic ligation: a + A + AaB <-> aA + AaB
            4404.73200206935 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            -4404.73200206935 * x.[15] * x.[6] * x.[32] // aA + b + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            112.941846206906 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -112.941846206906 * x.[15] * x.[6] * x.[58] // aA + b + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[79] // baA | ligation: b + aA <-> baA
            -1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[56] // aAB | ligation: aA + B <-> aAB
            -1.0 * x.[15] * x.[4] // aA + B | ligation: aA + B <-> aAB
            1.0 * x.[55] // aAA | ligation: aA + A <-> aAA
            -1.0 * x.[15] * x.[3] // aA + A | ligation: aA + A <-> aAA
            1.0 * x.[58] // aAb | ligation: aA + b <-> aAb
            -1.0 * x.[15] * x.[6] // aA + b | ligation: aA + b <-> aAb
            1.0 * x.[57] // aAa | ligation: aA + a <-> aAa
            -1.0 * x.[15] * x.[5] // aA + a | ligation: aA + a <-> aAa
            1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
            -1.0 * x.[15] // aA | ligation: a + A <-> aA
            1.0 * x.[5] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 16 - aB
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -4404.73200206935 * x.[3] * x.[16] * x.[32] // A + aB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            112.941846206906 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            -112.941846206906 * x.[3] * x.[16] * x.[58] // A + aB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            -4404.73200206935 * x.[16] * x.[58] // aB + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            4404.73200206935 * x.[5] * x.[4] * x.[58] // a + B + aAb | catalytic ligation: a + B + aAb <-> aB + aAb
            -112.941846206906 * x.[16] * x.[32] // aB + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            112.941846206906 * x.[5] * x.[4] * x.[32] // a + B + AaB | catalytic ligation: a + B + AaB <-> aB + AaB
            4404.73200206935 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            -4404.73200206935 * x.[16] * x.[6] * x.[32] // aB + b + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            112.941846206906 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            -112.941846206906 * x.[16] * x.[6] * x.[58] // aB + b + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[80] // baB | ligation: b + aB <-> baB
            -1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[60] // aBB | ligation: aB + B <-> aBB
            -1.0 * x.[16] * x.[4] // aB + B | ligation: aB + B <-> aBB
            1.0 * x.[59] // aBA | ligation: aB + A <-> aBA
            -1.0 * x.[16] * x.[3] // aB + A | ligation: aB + A <-> aBA
            1.0 * x.[62] // aBb | ligation: aB + b <-> aBb
            -1.0 * x.[16] * x.[6] // aB + b | ligation: aB + b <-> aBb
            1.0 * x.[61] // aBa | ligation: aB + a <-> aBa
            -1.0 * x.[16] * x.[5] // aB + a | ligation: aB + a <-> aBa
            -1.0 * x.[16] // aB | ligation: a + B <-> aB
            1.0 * x.[5] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 17 - aa
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[63] * x.[58] // aaA + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            -4404.73200206935 * x.[17] * x.[3] * x.[58] // aa + A + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            112.941846206906 * x.[63] * x.[32] // aaA + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            -112.941846206906 * x.[17] * x.[3] * x.[32] // aa + A + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            4404.73200206935 * x.[33] * x.[32] // Aaa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            -4404.73200206935 * x.[3] * x.[17] * x.[32] // A + aa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            112.941846206906 * x.[33] * x.[58] // Aaa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            -112.941846206906 * x.[3] * x.[17] * x.[58] // A + aa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            4404.73200206935 * x.[64] * x.[58] // aaB + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            -4404.73200206935 * x.[17] * x.[4] * x.[58] // aa + B + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            112.941846206906 * x.[64] * x.[32] // aaB + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            -112.941846206906 * x.[17] * x.[4] * x.[32] // aa + B + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[81] // baa | ligation: b + aa <-> baa
            -1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[64] // aaB | ligation: aa + B <-> aaB
            -1.0 * x.[17] * x.[4] // aa + B | ligation: aa + B <-> aaB
            1.0 * x.[63] // aaA | ligation: aa + A <-> aaA
            -1.0 * x.[17] * x.[3] // aa + A | ligation: aa + A <-> aaA
            1.0 * x.[66] // aab | ligation: aa + b <-> aab
            -1.0 * x.[17] * x.[6] // aa + b | ligation: aa + b <-> aab
            1.0 * x.[65] // aaa | ligation: aa + a <-> aaa
            -1.0 * x.[17] * x.[5] // aa + a | ligation: aa + a <-> aaa
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
            4404.73200206935 * x.[34] * x.[32] // Aab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            -4404.73200206935 * x.[3] * x.[18] * x.[32] // A + ab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            112.941846206906 * x.[34] * x.[58] // Aab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            -112.941846206906 * x.[3] * x.[18] * x.[58] // A + ab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            4404.73200206935 * x.[68] * x.[58] // abB + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            -4404.73200206935 * x.[18] * x.[4] * x.[58] // ab + B + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            112.941846206906 * x.[68] * x.[32] // abB + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            -112.941846206906 * x.[18] * x.[4] * x.[32] // ab + B + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[82] // bab | ligation: b + ab <-> bab
            -1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[68] // abB | ligation: ab + B <-> abB
            -1.0 * x.[18] * x.[4] // ab + B | ligation: ab + B <-> abB
            1.0 * x.[67] // abA | ligation: ab + A <-> abA
            -1.0 * x.[18] * x.[3] // ab + A | ligation: ab + A <-> abA
            1.0 * x.[70] // abb | ligation: ab + b <-> abb
            -1.0 * x.[18] * x.[6] // ab + b | ligation: ab + b <-> abb
            1.0 * x.[69] // aba | ligation: ab + a <-> aba
            -1.0 * x.[18] * x.[5] // ab + a | ligation: ab + a <-> aba
            1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
            -1.0 * x.[18] // ab | ligation: a + b <-> ab
            1.0 * x.[5] * x.[6] // a + b | ligation: a + b <-> ab
            1.0 * x.[66] // aab | ligation: a + ab <-> aab
            -1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 19 - bA
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[73] * x.[32] // bAa + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            -4404.73200206935 * x.[19] * x.[5] * x.[32] // bA + a + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            112.941846206906 * x.[73] * x.[58] // bAa + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            -112.941846206906 * x.[19] * x.[5] * x.[58] // bA + a + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            4404.73200206935 * x.[74] * x.[32] // bAb + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            -4404.73200206935 * x.[19] * x.[6] * x.[32] // bA + b + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            112.941846206906 * x.[74] * x.[58] // bAb + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            -112.941846206906 * x.[19] * x.[6] * x.[58] // bA + b + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            4404.73200206935 * x.[35] * x.[32] // AbA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            -4404.73200206935 * x.[3] * x.[19] * x.[32] // A + bA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            112.941846206906 * x.[35] * x.[58] // AbA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            -112.941846206906 * x.[3] * x.[19] * x.[58] // A + bA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            4404.73200206935 * x.[51] * x.[32] // BbA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            -4404.73200206935 * x.[4] * x.[19] * x.[32] // B + bA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            112.941846206906 * x.[51] * x.[58] // BbA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            -112.941846206906 * x.[4] * x.[19] * x.[58] // B + bA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            1.0 * x.[72] // bAB | ligation: bA + B <-> bAB
            -1.0 * x.[19] * x.[4] // bA + B | ligation: bA + B <-> bAB
            1.0 * x.[71] // bAA | ligation: bA + A <-> bAA
            -1.0 * x.[19] * x.[3] // bA + A | ligation: bA + A <-> bAA
            1.0 * x.[74] // bAb | ligation: bA + b <-> bAb
            -1.0 * x.[19] * x.[6] // bA + b | ligation: bA + b <-> bAb
            1.0 * x.[73] // bAa | ligation: bA + a <-> bAa
            -1.0 * x.[19] * x.[5] // bA + a | ligation: bA + a <-> bAa
            1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
            -1.0 * x.[19] // bA | ligation: b + A <-> bA
            1.0 * x.[6] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[67] // abA | ligation: a + bA <-> abA
            -1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 20 - bB
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            -4404.73200206935 * x.[3] * x.[20] * x.[32] // A + bB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            112.941846206906 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            -112.941846206906 * x.[3] * x.[20] * x.[58] // A + bB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            4404.73200206935 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            -4404.73200206935 * x.[20] * x.[6] * x.[32] // bB + b + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            112.941846206906 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            -112.941846206906 * x.[20] * x.[6] * x.[58] // bB + b + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            4404.73200206935 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            -4404.73200206935 * x.[4] * x.[20] * x.[32] // B + bB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            112.941846206906 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            -112.941846206906 * x.[4] * x.[20] * x.[58] // B + bB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            -4404.73200206935 * x.[20] * x.[58] // bB + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            4404.73200206935 * x.[6] * x.[4] * x.[58] // b + B + aAb | catalytic ligation: b + B + aAb <-> bB + aAb
            -112.941846206906 * x.[20] * x.[32] // bB + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            112.941846206906 * x.[6] * x.[4] * x.[32] // b + B + AaB | catalytic ligation: b + B + AaB <-> bB + AaB
            1.0 * x.[76] // bBB | ligation: bB + B <-> bBB
            -1.0 * x.[20] * x.[4] // bB + B | ligation: bB + B <-> bBB
            1.0 * x.[75] // bBA | ligation: bB + A <-> bBA
            -1.0 * x.[20] * x.[3] // bB + A | ligation: bB + A <-> bBA
            1.0 * x.[78] // bBb | ligation: bB + b <-> bBb
            -1.0 * x.[20] * x.[6] // bB + b | ligation: bB + b <-> bBb
            1.0 * x.[77] // bBa | ligation: bB + a <-> bBa
            -1.0 * x.[20] * x.[5] // bB + a | ligation: bB + a <-> bBa
            1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
            -1.0 * x.[20] // bB | ligation: b + B <-> bB
            1.0 * x.[6] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[68] // abB | ligation: a + bB <-> abB
            -1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 21 - ba
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[79] * x.[58] // baA + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            -4404.73200206935 * x.[21] * x.[3] * x.[58] // ba + A + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            112.941846206906 * x.[79] * x.[32] // baA + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            -112.941846206906 * x.[21] * x.[3] * x.[32] // ba + A + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            4404.73200206935 * x.[80] * x.[58] // baB + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            -4404.73200206935 * x.[21] * x.[4] * x.[58] // ba + B + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            112.941846206906 * x.[80] * x.[32] // baB + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            -112.941846206906 * x.[21] * x.[4] * x.[32] // ba + B + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            4404.73200206935 * x.[37] * x.[32] // Aba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            -4404.73200206935 * x.[3] * x.[21] * x.[32] // A + ba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            112.941846206906 * x.[37] * x.[58] // Aba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            -112.941846206906 * x.[3] * x.[21] * x.[58] // A + ba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            4404.73200206935 * x.[53] * x.[32] // Bba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            -4404.73200206935 * x.[4] * x.[21] * x.[32] // B + ba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            112.941846206906 * x.[53] * x.[58] // Bba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            -112.941846206906 * x.[4] * x.[21] * x.[58] // B + ba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            1.0 * x.[80] // baB | ligation: ba + B <-> baB
            -1.0 * x.[21] * x.[4] // ba + B | ligation: ba + B <-> baB
            1.0 * x.[79] // baA | ligation: ba + A <-> baA
            -1.0 * x.[21] * x.[3] // ba + A | ligation: ba + A <-> baA
            1.0 * x.[82] // bab | ligation: ba + b <-> bab
            -1.0 * x.[21] * x.[6] // ba + b | ligation: ba + b <-> bab
            1.0 * x.[81] // baa | ligation: ba + a <-> baa
            -1.0 * x.[21] * x.[5] // ba + a | ligation: ba + a <-> baa
            1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[85] // bba | ligation: b + ba <-> bba
            -1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
            -1.0 * x.[21] // ba | ligation: b + a <-> ba
            1.0 * x.[6] * x.[5] // b + a | ligation: b + a <-> ba
            1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[69] // aba | ligation: a + ba <-> aba
            -1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 22 - bb
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[84] * x.[58] // bbB + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            -4404.73200206935 * x.[22] * x.[4] * x.[58] // bb + B + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            112.941846206906 * x.[84] * x.[32] // bbB + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            -112.941846206906 * x.[22] * x.[4] * x.[32] // bb + B + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            4404.73200206935 * x.[54] * x.[32] // Bbb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            -4404.73200206935 * x.[4] * x.[22] * x.[32] // B + bb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            112.941846206906 * x.[54] * x.[58] // Bbb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            -112.941846206906 * x.[4] * x.[22] * x.[58] // B + bb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            4404.73200206935 * x.[38] * x.[32] // Abb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            -4404.73200206935 * x.[3] * x.[22] * x.[32] // A + bb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            112.941846206907 * x.[38] * x.[58] // Abb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            -112.941846206907 * x.[3] * x.[22] * x.[58] // A + bb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            1.0 * x.[84] // bbB | ligation: bb + B <-> bbB
            -1.0 * x.[22] * x.[4] // bb + B | ligation: bb + B <-> bbB
            1.0 * x.[83] // bbA | ligation: bb + A <-> bbA
            -1.0 * x.[22] * x.[3] // bb + A | ligation: bb + A <-> bbA
            1.0 * x.[86] // bbb | ligation: bb + b <-> bbb
            -1.0 * x.[22] * x.[6] // bb + b | ligation: bb + b <-> bbb
            1.0 * x.[85] // bba | ligation: bb + a <-> bba
            -1.0 * x.[22] * x.[5] // bb + a | ligation: bb + a <-> bba
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
            -1.0 * x.[23] // AAA | ligation: AA + A <-> AAA
            1.0 * x.[7] * x.[3] // AA + A | ligation: AA + A <-> AAA
            -1.0 * x.[23] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[7] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 24 - AAB
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[24] // AAB | ligation: AA + B <-> AAB
            1.0 * x.[7] * x.[4] // AA + B | ligation: AA + B <-> AAB
            -1.0 * x.[24] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[8] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 25 - AAa
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[25] * x.[32] // AAa + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            4404.73200206935 * x.[7] * x.[5] * x.[32] // AA + a + AaB | catalytic ligation: AA + a + AaB <-> AAa + AaB
            -112.941846206906 * x.[25] * x.[58] // AAa + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            112.941846206906 * x.[7] * x.[5] * x.[58] // AA + a + aAb | catalytic ligation: AA + a + aAb <-> AAa + aAb
            -1.0 * x.[25] // AAa | ligation: AA + a <-> AAa
            1.0 * x.[7] * x.[5] // AA + a | ligation: AA + a <-> AAa
            -1.0 * x.[25] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[9] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 26 - AAb
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[26] * x.[32] // AAb + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            4404.73200206935 * x.[7] * x.[6] * x.[32] // AA + b + AaB | catalytic ligation: AA + b + AaB <-> AAb + AaB
            -112.941846206906 * x.[26] * x.[58] // AAb + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            112.941846206906 * x.[7] * x.[6] * x.[58] // AA + b + aAb | catalytic ligation: AA + b + aAb <-> AAb + aAb
            -1.0 * x.[26] // AAb | ligation: AA + b <-> AAb
            1.0 * x.[7] * x.[6] // AA + b | ligation: AA + b <-> AAb
            -1.0 * x.[26] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[10] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 27 - ABA
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[27] // ABA | ligation: AB + A <-> ABA
            1.0 * x.[8] * x.[3] // AB + A | ligation: AB + A <-> ABA
            -1.0 * x.[27] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[11] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 28 - ABB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[28] // ABB | ligation: AB + B <-> ABB
            1.0 * x.[8] * x.[4] // AB + B | ligation: AB + B <-> ABB
            -1.0 * x.[28] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[12] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 29 - ABa
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[29] // ABa | ligation: AB + a <-> ABa
            1.0 * x.[8] * x.[5] // AB + a | ligation: AB + a <-> ABa
            -1.0 * x.[29] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[13] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 30 - ABb
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[30] * x.[32] // ABb + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            4404.73200206935 * x.[8] * x.[6] * x.[32] // AB + b + AaB | catalytic ligation: AB + b + AaB <-> ABb + AaB
            -112.941846206906 * x.[30] * x.[58] // ABb + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            112.941846206906 * x.[8] * x.[6] * x.[58] // AB + b + aAb | catalytic ligation: AB + b + aAb <-> ABb + aAb
            -1.0 * x.[30] // ABb | ligation: AB + b <-> ABb
            1.0 * x.[8] * x.[6] // AB + b | ligation: AB + b <-> ABb
            -1.0 * x.[30] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[14] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 31 - AaA
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            4404.73200206935 * x.[9] * x.[3] * x.[58] // Aa + A + aAb | catalytic ligation: Aa + A + aAb <-> AaA + aAb
            -112.941846206906 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            112.941846206906 * x.[9] * x.[3] * x.[32] // Aa + A + AaB | catalytic ligation: Aa + A + AaB <-> AaA + AaB
            -4404.73200206935 * x.[31] * x.[32] // AaA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            4404.73200206935 * x.[3] * x.[15] * x.[32] // A + aA + AaB | catalytic ligation: A + aA + AaB <-> AaA + AaB
            -112.941846206906 * x.[31] * x.[58] // AaA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            112.941846206906 * x.[3] * x.[15] * x.[58] // A + aA + aAb | catalytic ligation: A + aA + aAb <-> AaA + aAb
            -1.0 * x.[31] // AaA | ligation: Aa + A <-> AaA
            1.0 * x.[9] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            -1.0 * x.[31] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[15] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 32 - AaB
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -4404.73200206935 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -4404.73200206935 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            4404.73200206935 * x.[3] * x.[16] * x.[32] // A + aB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            4404.73200206935 * x.[3] * x.[16] * x.[32] // A + aB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -4404.73200206935 * x.[3] * x.[16] * x.[32] // A + aB + AaB | catalytic ligation: A + aB + AaB <-> AaB + AaB
            -112.941846206906 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            112.941846206906 * x.[3] * x.[16] * x.[58] // A + aB + aAb | catalytic ligation: A + aB + aAb <-> AaB + aAb
            -4404.73200206935 * x.[32] * x.[58] // AaB + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            4404.73200206935 * x.[9] * x.[4] * x.[58] // Aa + B + aAb | catalytic ligation: Aa + B + aAb <-> AaB + aAb
            112.941846206906 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -112.941846206906 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -112.941846206906 * x.[32] * x.[32] // AaB + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            112.941846206906 * x.[9] * x.[4] * x.[32] // Aa + B + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            112.941846206906 * x.[9] * x.[4] * x.[32] // Aa + B + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -112.941846206906 * x.[9] * x.[4] * x.[32] // Aa + B + AaB | catalytic ligation: Aa + B + AaB <-> AaB + AaB
            -1.0 * x.[32] // AaB | ligation: Aa + B <-> AaB
            1.0 * x.[9] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            -1.0 * x.[32] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[16] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 33 - Aaa
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[33] * x.[32] // Aaa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            4404.73200206935 * x.[3] * x.[17] * x.[32] // A + aa + AaB | catalytic ligation: A + aa + AaB <-> Aaa + AaB
            -112.941846206906 * x.[33] * x.[58] // Aaa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            112.941846206906 * x.[3] * x.[17] * x.[58] // A + aa + aAb | catalytic ligation: A + aa + aAb <-> Aaa + aAb
            -1.0 * x.[33] // Aaa | ligation: Aa + a <-> Aaa
            1.0 * x.[9] * x.[5] // Aa + a | ligation: Aa + a <-> Aaa
            -1.0 * x.[33] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[17] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 34 - Aab
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[34] * x.[32] // Aab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            4404.73200206935 * x.[3] * x.[18] * x.[32] // A + ab + AaB | catalytic ligation: A + ab + AaB <-> Aab + AaB
            -112.941846206906 * x.[34] * x.[58] // Aab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            112.941846206906 * x.[3] * x.[18] * x.[58] // A + ab + aAb | catalytic ligation: A + ab + aAb <-> Aab + aAb
            -1.0 * x.[34] // Aab | ligation: Aa + b <-> Aab
            1.0 * x.[9] * x.[6] // Aa + b | ligation: Aa + b <-> Aab
            -1.0 * x.[34] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[18] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 35 - AbA
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[35] * x.[32] // AbA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            4404.73200206935 * x.[3] * x.[19] * x.[32] // A + bA + AaB | catalytic ligation: A + bA + AaB <-> AbA + AaB
            -112.941846206906 * x.[35] * x.[58] // AbA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            112.941846206906 * x.[3] * x.[19] * x.[58] // A + bA + aAb | catalytic ligation: A + bA + aAb <-> AbA + aAb
            -1.0 * x.[35] // AbA | ligation: Ab + A <-> AbA
            1.0 * x.[10] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            -1.0 * x.[35] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[19] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 36 - AbB
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            4404.73200206935 * x.[3] * x.[20] * x.[32] // A + bB + AaB | catalytic ligation: A + bB + AaB <-> AbB + AaB
            -112.941846206906 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            112.941846206906 * x.[3] * x.[20] * x.[58] // A + bB + aAb | catalytic ligation: A + bB + aAb <-> AbB + aAb
            -4404.73200206935 * x.[36] * x.[58] // AbB + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            4404.73200206935 * x.[10] * x.[4] * x.[58] // Ab + B + aAb | catalytic ligation: Ab + B + aAb <-> AbB + aAb
            -112.941846206906 * x.[36] * x.[32] // AbB + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            112.941846206906 * x.[10] * x.[4] * x.[32] // Ab + B + AaB | catalytic ligation: Ab + B + AaB <-> AbB + AaB
            -1.0 * x.[36] // AbB | ligation: Ab + B <-> AbB
            1.0 * x.[10] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            -1.0 * x.[36] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[20] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 37 - Aba
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[37] * x.[32] // Aba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            4404.73200206935 * x.[3] * x.[21] * x.[32] // A + ba + AaB | catalytic ligation: A + ba + AaB <-> Aba + AaB
            -112.941846206906 * x.[37] * x.[58] // Aba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            112.941846206906 * x.[3] * x.[21] * x.[58] // A + ba + aAb | catalytic ligation: A + ba + aAb <-> Aba + aAb
            -1.0 * x.[37] // Aba | ligation: Ab + a <-> Aba
            1.0 * x.[10] * x.[5] // Ab + a | ligation: Ab + a <-> Aba
            -1.0 * x.[37] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[21] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 38 - Abb
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[38] * x.[32] // Abb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            4404.73200206935 * x.[3] * x.[22] * x.[32] // A + bb + AaB | catalytic ligation: A + bb + AaB <-> Abb + AaB
            -112.941846206907 * x.[38] * x.[58] // Abb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            112.941846206907 * x.[3] * x.[22] * x.[58] // A + bb + aAb | catalytic ligation: A + bb + aAb <-> Abb + aAb
            -1.0 * x.[38] // Abb | ligation: Ab + b <-> Abb
            1.0 * x.[10] * x.[6] // Ab + b | ligation: Ab + b <-> Abb
            -1.0 * x.[38] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[22] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 39 - BAA
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[39] // BAA | ligation: BA + A <-> BAA
            1.0 * x.[11] * x.[3] // BA + A | ligation: BA + A <-> BAA
            -1.0 * x.[39] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[7] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 40 - BAB
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[40] // BAB | ligation: BA + B <-> BAB
            1.0 * x.[11] * x.[4] // BA + B | ligation: BA + B <-> BAB
            -1.0 * x.[40] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[8] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 41 - BAa
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[41] * x.[32] // BAa + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            4404.73200206935 * x.[11] * x.[5] * x.[32] // BA + a + AaB | catalytic ligation: BA + a + AaB <-> BAa + AaB
            -112.941846206906 * x.[41] * x.[58] // BAa + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            112.941846206906 * x.[11] * x.[5] * x.[58] // BA + a + aAb | catalytic ligation: BA + a + aAb <-> BAa + aAb
            -1.0 * x.[41] // BAa | ligation: BA + a <-> BAa
            1.0 * x.[11] * x.[5] // BA + a | ligation: BA + a <-> BAa
            -1.0 * x.[41] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[9] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 42 - BAb
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[42] * x.[32] // BAb + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            4404.73200206935 * x.[11] * x.[6] * x.[32] // BA + b + AaB | catalytic ligation: BA + b + AaB <-> BAb + AaB
            -112.941846206906 * x.[42] * x.[58] // BAb + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            112.941846206906 * x.[11] * x.[6] * x.[58] // BA + b + aAb | catalytic ligation: BA + b + aAb <-> BAb + aAb
            -1.0 * x.[42] // BAb | ligation: BA + b <-> BAb
            1.0 * x.[11] * x.[6] // BA + b | ligation: BA + b <-> BAb
            -1.0 * x.[42] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[10] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 43 - BBA
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[43] // BBA | ligation: BB + A <-> BBA
            1.0 * x.[12] * x.[3] // BB + A | ligation: BB + A <-> BBA
            -1.0 * x.[43] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[11] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 44 - BBB
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[44] // BBB | ligation: BB + B <-> BBB
            1.0 * x.[12] * x.[4] // BB + B | ligation: BB + B <-> BBB
            -1.0 * x.[44] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[12] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 45 - BBa
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[45] // BBa | ligation: BB + a <-> BBa
            1.0 * x.[12] * x.[5] // BB + a | ligation: BB + a <-> BBa
            -1.0 * x.[45] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[13] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 46 - BBb
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[46] * x.[32] // BBb + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            4404.73200206935 * x.[12] * x.[6] * x.[32] // BB + b + AaB | catalytic ligation: BB + b + AaB <-> BBb + AaB
            -112.941846206906 * x.[46] * x.[58] // BBb + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            112.941846206906 * x.[12] * x.[6] * x.[58] // BB + b + aAb | catalytic ligation: BB + b + aAb <-> BBb + aAb
            -1.0 * x.[46] // BBb | ligation: BB + b <-> BBb
            1.0 * x.[12] * x.[6] // BB + b | ligation: BB + b <-> BBb
            -1.0 * x.[46] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[14] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 47 - BaA
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[47] * x.[58] // BaA + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            4404.73200206935 * x.[13] * x.[3] * x.[58] // Ba + A + aAb | catalytic ligation: Ba + A + aAb <-> BaA + aAb
            -112.941846206906 * x.[47] * x.[32] // BaA + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            112.941846206906 * x.[13] * x.[3] * x.[32] // Ba + A + AaB | catalytic ligation: Ba + A + AaB <-> BaA + AaB
            -1.0 * x.[47] // BaA | ligation: Ba + A <-> BaA
            1.0 * x.[13] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            -1.0 * x.[47] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[15] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 48 - BaB
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[48] * x.[58] // BaB + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            4404.73200206935 * x.[13] * x.[4] * x.[58] // Ba + B + aAb | catalytic ligation: Ba + B + aAb <-> BaB + aAb
            -112.941846206906 * x.[48] * x.[32] // BaB + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            112.941846206906 * x.[13] * x.[4] * x.[32] // Ba + B + AaB | catalytic ligation: Ba + B + AaB <-> BaB + AaB
            -1.0 * x.[48] // BaB | ligation: Ba + B <-> BaB
            1.0 * x.[13] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            -1.0 * x.[48] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[16] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 49 - Baa
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[49] // Baa | ligation: Ba + a <-> Baa
            1.0 * x.[13] * x.[5] // Ba + a | ligation: Ba + a <-> Baa
            -1.0 * x.[49] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[17] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 50 - Bab
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[50] // Bab | ligation: Ba + b <-> Bab
            1.0 * x.[13] * x.[6] // Ba + b | ligation: Ba + b <-> Bab
            -1.0 * x.[50] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[18] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 51 - BbA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[51] * x.[32] // BbA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            4404.73200206935 * x.[4] * x.[19] * x.[32] // B + bA + AaB | catalytic ligation: B + bA + AaB <-> BbA + AaB
            -112.941846206906 * x.[51] * x.[58] // BbA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            112.941846206906 * x.[4] * x.[19] * x.[58] // B + bA + aAb | catalytic ligation: B + bA + aAb <-> BbA + aAb
            -1.0 * x.[51] // BbA | ligation: Bb + A <-> BbA
            1.0 * x.[14] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            -1.0 * x.[51] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[19] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 52 - BbB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            4404.73200206935 * x.[14] * x.[4] * x.[58] // Bb + B + aAb | catalytic ligation: Bb + B + aAb <-> BbB + aAb
            -112.941846206906 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            112.941846206906 * x.[14] * x.[4] * x.[32] // Bb + B + AaB | catalytic ligation: Bb + B + AaB <-> BbB + AaB
            -4404.73200206935 * x.[52] * x.[32] // BbB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            4404.73200206935 * x.[4] * x.[20] * x.[32] // B + bB + AaB | catalytic ligation: B + bB + AaB <-> BbB + AaB
            -112.941846206906 * x.[52] * x.[58] // BbB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            112.941846206906 * x.[4] * x.[20] * x.[58] // B + bB + aAb | catalytic ligation: B + bB + aAb <-> BbB + aAb
            -1.0 * x.[52] // BbB | ligation: Bb + B <-> BbB
            1.0 * x.[14] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            -1.0 * x.[52] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[20] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 53 - Bba
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[53] * x.[32] // Bba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            4404.73200206935 * x.[4] * x.[21] * x.[32] // B + ba + AaB | catalytic ligation: B + ba + AaB <-> Bba + AaB
            -112.941846206906 * x.[53] * x.[58] // Bba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            112.941846206906 * x.[4] * x.[21] * x.[58] // B + ba + aAb | catalytic ligation: B + ba + aAb <-> Bba + aAb
            -1.0 * x.[53] // Bba | ligation: Bb + a <-> Bba
            1.0 * x.[14] * x.[5] // Bb + a | ligation: Bb + a <-> Bba
            -1.0 * x.[53] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[21] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 54 - Bbb
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[54] * x.[32] // Bbb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            4404.73200206935 * x.[4] * x.[22] * x.[32] // B + bb + AaB | catalytic ligation: B + bb + AaB <-> Bbb + AaB
            -112.941846206906 * x.[54] * x.[58] // Bbb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            112.941846206906 * x.[4] * x.[22] * x.[58] // B + bb + aAb | catalytic ligation: B + bb + aAb <-> Bbb + aAb
            -1.0 * x.[54] // Bbb | ligation: Bb + b <-> Bbb
            1.0 * x.[14] * x.[6] // Bb + b | ligation: Bb + b <-> Bbb
            -1.0 * x.[54] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[22] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 55 - aAA
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[55] * x.[58] // aAA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            4404.73200206935 * x.[5] * x.[7] * x.[58] // a + AA + aAb | catalytic ligation: a + AA + aAb <-> aAA + aAb
            -112.941846206906 * x.[55] * x.[32] // aAA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            112.941846206906 * x.[5] * x.[7] * x.[32] // a + AA + AaB | catalytic ligation: a + AA + AaB <-> aAA + AaB
            -1.0 * x.[55] // aAA | ligation: aA + A <-> aAA
            1.0 * x.[15] * x.[3] // aA + A | ligation: aA + A <-> aAA
            -1.0 * x.[55] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[5] * x.[7] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 56 - aAB
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[56] * x.[58] // aAB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            4404.73200206935 * x.[5] * x.[8] * x.[58] // a + AB + aAb | catalytic ligation: a + AB + aAb <-> aAB + aAb
            -112.941846206906 * x.[56] * x.[32] // aAB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            112.941846206906 * x.[5] * x.[8] * x.[32] // a + AB + AaB | catalytic ligation: a + AB + AaB <-> aAB + AaB
            -1.0 * x.[56] // aAB | ligation: aA + B <-> aAB
            1.0 * x.[15] * x.[4] // aA + B | ligation: aA + B <-> aAB
            -1.0 * x.[56] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[5] * x.[8] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 57 - aAa
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            4404.73200206935 * x.[15] * x.[5] * x.[32] // aA + a + AaB | catalytic ligation: aA + a + AaB <-> aAa + AaB
            -112.941846206906 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            112.941846206906 * x.[15] * x.[5] * x.[58] // aA + a + aAb | catalytic ligation: aA + a + aAb <-> aAa + aAb
            -4404.73200206935 * x.[57] * x.[58] // aAa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            4404.73200206935 * x.[5] * x.[9] * x.[58] // a + Aa + aAb | catalytic ligation: a + Aa + aAb <-> aAa + aAb
            -112.941846206906 * x.[57] * x.[32] // aAa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            112.941846206906 * x.[5] * x.[9] * x.[32] // a + Aa + AaB | catalytic ligation: a + Aa + AaB <-> aAa + AaB
            -1.0 * x.[57] // aAa | ligation: aA + a <-> aAa
            1.0 * x.[15] * x.[5] // aA + a | ligation: aA + a <-> aAa
            -1.0 * x.[57] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[5] * x.[9] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 58 - aAb
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            4404.73200206935 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -4404.73200206935 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -4404.73200206935 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            4404.73200206935 * x.[5] * x.[10] * x.[58] // a + Ab + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            4404.73200206935 * x.[5] * x.[10] * x.[58] // a + Ab + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -4404.73200206935 * x.[5] * x.[10] * x.[58] // a + Ab + aAb | catalytic ligation: a + Ab + aAb <-> aAb + aAb
            -112.941846206906 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            112.941846206906 * x.[5] * x.[10] * x.[32] // a + Ab + AaB | catalytic ligation: a + Ab + AaB <-> aAb + AaB
            -4404.73200206935 * x.[58] * x.[32] // aAb + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            4404.73200206935 * x.[15] * x.[6] * x.[32] // aA + b + AaB | catalytic ligation: aA + b + AaB <-> aAb + AaB
            112.941846206906 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -112.941846206906 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -112.941846206906 * x.[58] * x.[58] // aAb + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            112.941846206906 * x.[15] * x.[6] * x.[58] // aA + b + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            112.941846206906 * x.[15] * x.[6] * x.[58] // aA + b + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -112.941846206906 * x.[15] * x.[6] * x.[58] // aA + b + aAb | catalytic ligation: aA + b + aAb <-> aAb + aAb
            -1.0 * x.[58] // aAb | ligation: aA + b <-> aAb
            1.0 * x.[15] * x.[6] // aA + b | ligation: aA + b <-> aAb
            -1.0 * x.[58] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[5] * x.[10] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 59 - aBA
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[59] * x.[58] // aBA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            4404.73200206935 * x.[5] * x.[11] * x.[58] // a + BA + aAb | catalytic ligation: a + BA + aAb <-> aBA + aAb
            -112.941846206906 * x.[59] * x.[32] // aBA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            112.941846206906 * x.[5] * x.[11] * x.[32] // a + BA + AaB | catalytic ligation: a + BA + AaB <-> aBA + AaB
            -1.0 * x.[59] // aBA | ligation: aB + A <-> aBA
            1.0 * x.[16] * x.[3] // aB + A | ligation: aB + A <-> aBA
            -1.0 * x.[59] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[5] * x.[11] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 60 - aBB
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[60] * x.[58] // aBB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            4404.73200206935 * x.[5] * x.[12] * x.[58] // a + BB + aAb | catalytic ligation: a + BB + aAb <-> aBB + aAb
            -112.941846206907 * x.[60] * x.[32] // aBB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            112.941846206907 * x.[5] * x.[12] * x.[32] // a + BB + AaB | catalytic ligation: a + BB + AaB <-> aBB + AaB
            -1.0 * x.[60] // aBB | ligation: aB + B <-> aBB
            1.0 * x.[16] * x.[4] // aB + B | ligation: aB + B <-> aBB
            -1.0 * x.[60] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[5] * x.[12] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 61 - aBa
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[61] * x.[58] // aBa + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            4404.73200206935 * x.[5] * x.[13] * x.[58] // a + Ba + aAb | catalytic ligation: a + Ba + aAb <-> aBa + aAb
            -112.941846206906 * x.[61] * x.[32] // aBa + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            112.941846206906 * x.[5] * x.[13] * x.[32] // a + Ba + AaB | catalytic ligation: a + Ba + AaB <-> aBa + AaB
            -1.0 * x.[61] // aBa | ligation: aB + a <-> aBa
            1.0 * x.[16] * x.[5] // aB + a | ligation: aB + a <-> aBa
            -1.0 * x.[61] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[5] * x.[13] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 62 - aBb
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            4404.73200206935 * x.[5] * x.[14] * x.[58] // a + Bb + aAb | catalytic ligation: a + Bb + aAb <-> aBb + aAb
            -112.941846206906 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            112.941846206906 * x.[5] * x.[14] * x.[32] // a + Bb + AaB | catalytic ligation: a + Bb + AaB <-> aBb + AaB
            -4404.73200206935 * x.[62] * x.[32] // aBb + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            4404.73200206935 * x.[16] * x.[6] * x.[32] // aB + b + AaB | catalytic ligation: aB + b + AaB <-> aBb + AaB
            -112.941846206906 * x.[62] * x.[58] // aBb + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            112.941846206906 * x.[16] * x.[6] * x.[58] // aB + b + aAb | catalytic ligation: aB + b + aAb <-> aBb + aAb
            -1.0 * x.[62] // aBb | ligation: aB + b <-> aBb
            1.0 * x.[16] * x.[6] // aB + b | ligation: aB + b <-> aBb
            -1.0 * x.[62] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[5] * x.[14] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 63 - aaA
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[63] * x.[58] // aaA + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            4404.73200206935 * x.[17] * x.[3] * x.[58] // aa + A + aAb | catalytic ligation: aa + A + aAb <-> aaA + aAb
            -112.941846206906 * x.[63] * x.[32] // aaA + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            112.941846206906 * x.[17] * x.[3] * x.[32] // aa + A + AaB | catalytic ligation: aa + A + AaB <-> aaA + AaB
            -1.0 * x.[63] // aaA | ligation: aa + A <-> aaA
            1.0 * x.[17] * x.[3] // aa + A | ligation: aa + A <-> aaA
            -1.0 * x.[63] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[5] * x.[15] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 64 - aaB
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[64] * x.[58] // aaB + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            4404.73200206935 * x.[17] * x.[4] * x.[58] // aa + B + aAb | catalytic ligation: aa + B + aAb <-> aaB + aAb
            -112.941846206906 * x.[64] * x.[32] // aaB + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            112.941846206906 * x.[17] * x.[4] * x.[32] // aa + B + AaB | catalytic ligation: aa + B + AaB <-> aaB + AaB
            -1.0 * x.[64] // aaB | ligation: aa + B <-> aaB
            1.0 * x.[17] * x.[4] // aa + B | ligation: aa + B <-> aaB
            -1.0 * x.[64] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[5] * x.[16] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 65 - aaa
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[65] // aaa | ligation: aa + a <-> aaa
            1.0 * x.[17] * x.[5] // aa + a | ligation: aa + a <-> aaa
            -1.0 * x.[65] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[5] * x.[17] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 66 - aab
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[66] // aab | ligation: aa + b <-> aab
            1.0 * x.[17] * x.[6] // aa + b | ligation: aa + b <-> aab
            -1.0 * x.[66] // aab | ligation: a + ab <-> aab
            1.0 * x.[5] * x.[18] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 67 - abA
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[67] // abA | ligation: ab + A <-> abA
            1.0 * x.[18] * x.[3] // ab + A | ligation: ab + A <-> abA
            -1.0 * x.[67] // abA | ligation: a + bA <-> abA
            1.0 * x.[5] * x.[19] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 68 - abB
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[68] * x.[58] // abB + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            4404.73200206935 * x.[18] * x.[4] * x.[58] // ab + B + aAb | catalytic ligation: ab + B + aAb <-> abB + aAb
            -112.941846206906 * x.[68] * x.[32] // abB + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            112.941846206906 * x.[18] * x.[4] * x.[32] // ab + B + AaB | catalytic ligation: ab + B + AaB <-> abB + AaB
            -1.0 * x.[68] // abB | ligation: ab + B <-> abB
            1.0 * x.[18] * x.[4] // ab + B | ligation: ab + B <-> abB
            -1.0 * x.[68] // abB | ligation: a + bB <-> abB
            1.0 * x.[5] * x.[20] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 69 - aba
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[69] // aba | ligation: ab + a <-> aba
            1.0 * x.[18] * x.[5] // ab + a | ligation: ab + a <-> aba
            -1.0 * x.[69] // aba | ligation: a + ba <-> aba
            1.0 * x.[5] * x.[21] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 70 - abb
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[70] // abb | ligation: ab + b <-> abb
            1.0 * x.[18] * x.[6] // ab + b | ligation: ab + b <-> abb
            -1.0 * x.[70] // abb | ligation: a + bb <-> abb
            1.0 * x.[5] * x.[22] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 71 - bAA
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[71] // bAA | ligation: bA + A <-> bAA
            1.0 * x.[19] * x.[3] // bA + A | ligation: bA + A <-> bAA
            -1.0 * x.[71] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[6] * x.[7] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 72 - bAB
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[72] // bAB | ligation: bA + B <-> bAB
            1.0 * x.[19] * x.[4] // bA + B | ligation: bA + B <-> bAB
            -1.0 * x.[72] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[6] * x.[8] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 73 - bAa
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[73] * x.[32] // bAa + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            4404.73200206935 * x.[19] * x.[5] * x.[32] // bA + a + AaB | catalytic ligation: bA + a + AaB <-> bAa + AaB
            -112.941846206906 * x.[73] * x.[58] // bAa + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            112.941846206906 * x.[19] * x.[5] * x.[58] // bA + a + aAb | catalytic ligation: bA + a + aAb <-> bAa + aAb
            -1.0 * x.[73] // bAa | ligation: bA + a <-> bAa
            1.0 * x.[19] * x.[5] // bA + a | ligation: bA + a <-> bAa
            -1.0 * x.[73] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[6] * x.[9] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 74 - bAb
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[74] * x.[32] // bAb + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            4404.73200206935 * x.[19] * x.[6] * x.[32] // bA + b + AaB | catalytic ligation: bA + b + AaB <-> bAb + AaB
            -112.941846206906 * x.[74] * x.[58] // bAb + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            112.941846206906 * x.[19] * x.[6] * x.[58] // bA + b + aAb | catalytic ligation: bA + b + aAb <-> bAb + aAb
            -1.0 * x.[74] // bAb | ligation: bA + b <-> bAb
            1.0 * x.[19] * x.[6] // bA + b | ligation: bA + b <-> bAb
            -1.0 * x.[74] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[6] * x.[10] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 75 - bBA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[75] * x.[58] // bBA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            4404.73200206935 * x.[6] * x.[11] * x.[58] // b + BA + aAb | catalytic ligation: b + BA + aAb <-> bBA + aAb
            -112.941846206906 * x.[75] * x.[32] // bBA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            112.941846206906 * x.[6] * x.[11] * x.[32] // b + BA + AaB | catalytic ligation: b + BA + AaB <-> bBA + AaB
            -1.0 * x.[75] // bBA | ligation: bB + A <-> bBA
            1.0 * x.[20] * x.[3] // bB + A | ligation: bB + A <-> bBA
            -1.0 * x.[75] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[6] * x.[11] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 76 - bBB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[76] * x.[58] // bBB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            4404.73200206935 * x.[6] * x.[12] * x.[58] // b + BB + aAb | catalytic ligation: b + BB + aAb <-> bBB + aAb
            -112.941846206906 * x.[76] * x.[32] // bBB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            112.941846206906 * x.[6] * x.[12] * x.[32] // b + BB + AaB | catalytic ligation: b + BB + AaB <-> bBB + AaB
            -1.0 * x.[76] // bBB | ligation: bB + B <-> bBB
            1.0 * x.[20] * x.[4] // bB + B | ligation: bB + B <-> bBB
            -1.0 * x.[76] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[6] * x.[12] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 77 - bBa
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[77] * x.[58] // bBa + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            4404.73200206935 * x.[6] * x.[13] * x.[58] // b + Ba + aAb | catalytic ligation: b + Ba + aAb <-> bBa + aAb
            -112.941846206906 * x.[77] * x.[32] // bBa + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            112.941846206906 * x.[6] * x.[13] * x.[32] // b + Ba + AaB | catalytic ligation: b + Ba + AaB <-> bBa + AaB
            -1.0 * x.[77] // bBa | ligation: bB + a <-> bBa
            1.0 * x.[20] * x.[5] // bB + a | ligation: bB + a <-> bBa
            -1.0 * x.[77] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[6] * x.[13] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 78 - bBb
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            4404.73200206935 * x.[20] * x.[6] * x.[32] // bB + b + AaB | catalytic ligation: bB + b + AaB <-> bBb + AaB
            -112.941846206906 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            112.941846206906 * x.[20] * x.[6] * x.[58] // bB + b + aAb | catalytic ligation: bB + b + aAb <-> bBb + aAb
            -4404.73200206935 * x.[78] * x.[58] // bBb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            4404.73200206935 * x.[6] * x.[14] * x.[58] // b + Bb + aAb | catalytic ligation: b + Bb + aAb <-> bBb + aAb
            -112.941846206906 * x.[78] * x.[32] // bBb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            112.941846206906 * x.[6] * x.[14] * x.[32] // b + Bb + AaB | catalytic ligation: b + Bb + AaB <-> bBb + AaB
            -1.0 * x.[78] // bBb | ligation: bB + b <-> bBb
            1.0 * x.[20] * x.[6] // bB + b | ligation: bB + b <-> bBb
            -1.0 * x.[78] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[6] * x.[14] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 79 - baA
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[79] * x.[58] // baA + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            4404.73200206935 * x.[21] * x.[3] * x.[58] // ba + A + aAb | catalytic ligation: ba + A + aAb <-> baA + aAb
            -112.941846206906 * x.[79] * x.[32] // baA + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            112.941846206906 * x.[21] * x.[3] * x.[32] // ba + A + AaB | catalytic ligation: ba + A + AaB <-> baA + AaB
            -1.0 * x.[79] // baA | ligation: ba + A <-> baA
            1.0 * x.[21] * x.[3] // ba + A | ligation: ba + A <-> baA
            -1.0 * x.[79] // baA | ligation: b + aA <-> baA
            1.0 * x.[6] * x.[15] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 80 - baB
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[80] * x.[58] // baB + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            4404.73200206935 * x.[21] * x.[4] * x.[58] // ba + B + aAb | catalytic ligation: ba + B + aAb <-> baB + aAb
            -112.941846206906 * x.[80] * x.[32] // baB + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            112.941846206906 * x.[21] * x.[4] * x.[32] // ba + B + AaB | catalytic ligation: ba + B + AaB <-> baB + AaB
            -1.0 * x.[80] // baB | ligation: ba + B <-> baB
            1.0 * x.[21] * x.[4] // ba + B | ligation: ba + B <-> baB
            -1.0 * x.[80] // baB | ligation: b + aB <-> baB
            1.0 * x.[6] * x.[16] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 81 - baa
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[81] // baa | ligation: ba + a <-> baa
            1.0 * x.[21] * x.[5] // ba + a | ligation: ba + a <-> baa
            -1.0 * x.[81] // baa | ligation: b + aa <-> baa
            1.0 * x.[6] * x.[17] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 82 - bab
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[82] // bab | ligation: ba + b <-> bab
            1.0 * x.[21] * x.[6] // ba + b | ligation: ba + b <-> bab
            -1.0 * x.[82] // bab | ligation: b + ab <-> bab
            1.0 * x.[6] * x.[18] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 83 - bbA
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[83] // bbA | ligation: bb + A <-> bbA
            1.0 * x.[22] * x.[3] // bb + A | ligation: bb + A <-> bbA
            -1.0 * x.[83] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[6] * x.[19] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 84 - bbB
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -4404.73200206935 * x.[84] * x.[58] // bbB + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            4404.73200206935 * x.[22] * x.[4] * x.[58] // bb + B + aAb | catalytic ligation: bb + B + aAb <-> bbB + aAb
            -112.941846206906 * x.[84] * x.[32] // bbB + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            112.941846206906 * x.[22] * x.[4] * x.[32] // bb + B + AaB | catalytic ligation: bb + B + AaB <-> bbB + AaB
            -1.0 * x.[84] // bbB | ligation: bb + B <-> bbB
            1.0 * x.[22] * x.[4] // bb + B | ligation: bb + B <-> bbB
            -1.0 * x.[84] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[6] * x.[20] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 85 - bba
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[85] // bba | ligation: bb + a <-> bba
            1.0 * x.[22] * x.[5] // bb + a | ligation: bb + a <-> bba
            -1.0 * x.[85] // bba | ligation: b + ba <-> bba
            1.0 * x.[6] * x.[21] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 86 - bbb
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[86] // bbb | ligation: bb + b <-> bbb
            1.0 * x.[22] * x.[6] // bb + b | ligation: bb + b <-> bbb
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
                                                fileStructureVersion = 4.0000m
                                                versionNumber = "5.0.3"
                                                modelDataId = ("f40d9be2-ce30-4981-9c1e-7b2ed1138425" |> Guid |> ModelDataId)
                                                numberOfSubstances = 87
                                                numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                                                maxPeptideLength = MaxPeptideLength.ThreeMax
                                                seedValue = 1118393691
                                                clmDefaultValueId = ClmDefaultValueId 4002000020L
                                            }

                                        allParams =
                                            [|
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

                                                {
                                                    modelParam = 
                                                        {
                                                            ligationParam = 
                                                                {
                                                                    ligationDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                                    forwardScale = Some 1.0
                                                                    backwardScale = Some 1.0
                                                                }
                                                                |> LigRndParam

                                                            catLigRndEeParams = 
                                                                {
                                                                    rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0005; scale = Some 2000.0; shift = None } } |> Distribution |> RateMultDistr
                                                                    eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                                    eeBackwardDistribution = None
                                                                }
                                                        }
                                                        |> CatLigRndParam
                                                        |> CatalyticLigationRateParam
                                                    usage = DependsOnParam
                                                }

                                                {
                                                    modelParam = 
                                                        {
                                                            catLigParam = 
                                                                {
                                                                    ligationParam = 
                                                                        {
                                                                            ligationDistribution = { distributionType = Delta; distributionParams = { threshold = None; scale = None; shift = Some 1.0 } } |> Distribution
                                                                            forwardScale = Some 1.0
                                                                            backwardScale = Some 1.0
                                                                        }
                                                                        |> LigRndParam

                                                                    catLigRndEeParams = 
                                                                        {
                                                                            rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0005; scale = Some 2000.0; shift = None } } |> Distribution |> RateMultDistr
                                                                            eeForwardDistribution = { distributionType = BiDelta; distributionParams = { threshold = None; scale = Some 0.95; shift = None } } |> Distribution |> EeDistribution |> Some
                                                                            eeBackwardDistribution = None
                                                                        }
                                                                }

                                                            catLigSimParam = 
                                                                {
                                                                    catRatesSimGeneration = { distributionType = Uniform; distributionParams = { threshold = Some 0.3; scale = None; shift = Some 1.0 } } |> Distribution |> FixedValue
                                                                    getRateMultiplierDistr = DeltaRateMultDistrGetter
                                                                    getForwardEeDistr = DeltaEeDistributionGetter
                                                                    getBackwardEeDistr = DeltaEeDistributionGetter
                                                                }

                                                        }
                                                        |> CatLigSimParam
                                                        |> CatalyticLigationRateParam
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
                                                (SynthesisName, 4L)
                                                (DestructionName, 4L)
                                                (CatalyticSynthesisName, 256L)
                                                (CatalyticDestructionName, 256L)
                                                (LigationName, 144L)
                                                (CatalyticLigationName, 9216L)
                                                (SedimentationDirectName, 7056L)
                                                (SedimentationAllName, 4L)
                                                (RacemizationName, 4L)
                                                (CatalyticRacemizationName, 256L)
                                            ]
                                        allReactions =
                                            [
                                                (LigationName, 144L)
                                                (CatalyticLigationName, 108L)
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


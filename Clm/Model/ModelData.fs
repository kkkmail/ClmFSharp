namespace Clm.Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 685070710
    let numberOfAminoAcids = NumberOfAminoAcids.ThreeAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 261

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
            x.[5] // C
            x.[6] // a
            x.[7] // b
            x.[8] // c
            2.0 * x.[9] // AA
            2.0 * x.[10] // AB
            2.0 * x.[11] // AC
            2.0 * x.[12] // Aa
            2.0 * x.[13] // Ab
            2.0 * x.[14] // Ac
            2.0 * x.[15] // BA
            2.0 * x.[16] // BB
            2.0 * x.[17] // BC
            2.0 * x.[18] // Ba
            2.0 * x.[19] // Bb
            2.0 * x.[20] // Bc
            2.0 * x.[21] // CA
            2.0 * x.[22] // CB
            2.0 * x.[23] // CC
            2.0 * x.[24] // Ca
            2.0 * x.[25] // Cb
            2.0 * x.[26] // Cc
            2.0 * x.[27] // aA
            2.0 * x.[28] // aB
            2.0 * x.[29] // aC
            2.0 * x.[30] // aa
            2.0 * x.[31] // ab
            2.0 * x.[32] // ac
            2.0 * x.[33] // bA
            2.0 * x.[34] // bB
            2.0 * x.[35] // bC
            2.0 * x.[36] // ba
            2.0 * x.[37] // bb
            2.0 * x.[38] // bc
            2.0 * x.[39] // cA
            2.0 * x.[40] // cB
            2.0 * x.[41] // cC
            2.0 * x.[42] // ca
            2.0 * x.[43] // cb
            2.0 * x.[44] // cc
            3.0 * x.[45] // AAA
            3.0 * x.[46] // AAB
            3.0 * x.[47] // AAC
            3.0 * x.[48] // AAa
            3.0 * x.[49] // AAb
            3.0 * x.[50] // AAc
            3.0 * x.[51] // ABA
            3.0 * x.[52] // ABB
            3.0 * x.[53] // ABC
            3.0 * x.[54] // ABa
            3.0 * x.[55] // ABb
            3.0 * x.[56] // ABc
            3.0 * x.[57] // ACA
            3.0 * x.[58] // ACB
            3.0 * x.[59] // ACC
            3.0 * x.[60] // ACa
            3.0 * x.[61] // ACb
            3.0 * x.[62] // ACc
            3.0 * x.[63] // AaA
            3.0 * x.[64] // AaB
            3.0 * x.[65] // AaC
            3.0 * x.[66] // Aaa
            3.0 * x.[67] // Aab
            3.0 * x.[68] // Aac
            3.0 * x.[69] // AbA
            3.0 * x.[70] // AbB
            3.0 * x.[71] // AbC
            3.0 * x.[72] // Aba
            3.0 * x.[73] // Abb
            3.0 * x.[74] // Abc
            3.0 * x.[75] // AcA
            3.0 * x.[76] // AcB
            3.0 * x.[77] // AcC
            3.0 * x.[78] // Aca
            3.0 * x.[79] // Acb
            3.0 * x.[80] // Acc
            3.0 * x.[81] // BAA
            3.0 * x.[82] // BAB
            3.0 * x.[83] // BAC
            3.0 * x.[84] // BAa
            3.0 * x.[85] // BAb
            3.0 * x.[86] // BAc
            3.0 * x.[87] // BBA
            3.0 * x.[88] // BBB
            3.0 * x.[89] // BBC
            3.0 * x.[90] // BBa
            3.0 * x.[91] // BBb
            3.0 * x.[92] // BBc
            3.0 * x.[93] // BCA
            3.0 * x.[94] // BCB
            3.0 * x.[95] // BCC
            3.0 * x.[96] // BCa
            3.0 * x.[97] // BCb
            3.0 * x.[98] // BCc
            3.0 * x.[99] // BaA
            3.0 * x.[100] // BaB
            3.0 * x.[101] // BaC
            3.0 * x.[102] // Baa
            3.0 * x.[103] // Bab
            3.0 * x.[104] // Bac
            3.0 * x.[105] // BbA
            3.0 * x.[106] // BbB
            3.0 * x.[107] // BbC
            3.0 * x.[108] // Bba
            3.0 * x.[109] // Bbb
            3.0 * x.[110] // Bbc
            3.0 * x.[111] // BcA
            3.0 * x.[112] // BcB
            3.0 * x.[113] // BcC
            3.0 * x.[114] // Bca
            3.0 * x.[115] // Bcb
            3.0 * x.[116] // Bcc
            3.0 * x.[117] // CAA
            3.0 * x.[118] // CAB
            3.0 * x.[119] // CAC
            3.0 * x.[120] // CAa
            3.0 * x.[121] // CAb
            3.0 * x.[122] // CAc
            3.0 * x.[123] // CBA
            3.0 * x.[124] // CBB
            3.0 * x.[125] // CBC
            3.0 * x.[126] // CBa
            3.0 * x.[127] // CBb
            3.0 * x.[128] // CBc
            3.0 * x.[129] // CCA
            3.0 * x.[130] // CCB
            3.0 * x.[131] // CCC
            3.0 * x.[132] // CCa
            3.0 * x.[133] // CCb
            3.0 * x.[134] // CCc
            3.0 * x.[135] // CaA
            3.0 * x.[136] // CaB
            3.0 * x.[137] // CaC
            3.0 * x.[138] // Caa
            3.0 * x.[139] // Cab
            3.0 * x.[140] // Cac
            3.0 * x.[141] // CbA
            3.0 * x.[142] // CbB
            3.0 * x.[143] // CbC
            3.0 * x.[144] // Cba
            3.0 * x.[145] // Cbb
            3.0 * x.[146] // Cbc
            3.0 * x.[147] // CcA
            3.0 * x.[148] // CcB
            3.0 * x.[149] // CcC
            3.0 * x.[150] // Cca
            3.0 * x.[151] // Ccb
            3.0 * x.[152] // Ccc
            3.0 * x.[153] // aAA
            3.0 * x.[154] // aAB
            3.0 * x.[155] // aAC
            3.0 * x.[156] // aAa
            3.0 * x.[157] // aAb
            3.0 * x.[158] // aAc
            3.0 * x.[159] // aBA
            3.0 * x.[160] // aBB
            3.0 * x.[161] // aBC
            3.0 * x.[162] // aBa
            3.0 * x.[163] // aBb
            3.0 * x.[164] // aBc
            3.0 * x.[165] // aCA
            3.0 * x.[166] // aCB
            3.0 * x.[167] // aCC
            3.0 * x.[168] // aCa
            3.0 * x.[169] // aCb
            3.0 * x.[170] // aCc
            3.0 * x.[171] // aaA
            3.0 * x.[172] // aaB
            3.0 * x.[173] // aaC
            3.0 * x.[174] // aaa
            3.0 * x.[175] // aab
            3.0 * x.[176] // aac
            3.0 * x.[177] // abA
            3.0 * x.[178] // abB
            3.0 * x.[179] // abC
            3.0 * x.[180] // aba
            3.0 * x.[181] // abb
            3.0 * x.[182] // abc
            3.0 * x.[183] // acA
            3.0 * x.[184] // acB
            3.0 * x.[185] // acC
            3.0 * x.[186] // aca
            3.0 * x.[187] // acb
            3.0 * x.[188] // acc
            3.0 * x.[189] // bAA
            3.0 * x.[190] // bAB
            3.0 * x.[191] // bAC
            3.0 * x.[192] // bAa
            3.0 * x.[193] // bAb
            3.0 * x.[194] // bAc
            3.0 * x.[195] // bBA
            3.0 * x.[196] // bBB
            3.0 * x.[197] // bBC
            3.0 * x.[198] // bBa
            3.0 * x.[199] // bBb
            3.0 * x.[200] // bBc
            3.0 * x.[201] // bCA
            3.0 * x.[202] // bCB
            3.0 * x.[203] // bCC
            3.0 * x.[204] // bCa
            3.0 * x.[205] // bCb
            3.0 * x.[206] // bCc
            3.0 * x.[207] // baA
            3.0 * x.[208] // baB
            3.0 * x.[209] // baC
            3.0 * x.[210] // baa
            3.0 * x.[211] // bab
            3.0 * x.[212] // bac
            3.0 * x.[213] // bbA
            3.0 * x.[214] // bbB
            3.0 * x.[215] // bbC
            3.0 * x.[216] // bba
            3.0 * x.[217] // bbb
            3.0 * x.[218] // bbc
            3.0 * x.[219] // bcA
            3.0 * x.[220] // bcB
            3.0 * x.[221] // bcC
            3.0 * x.[222] // bca
            3.0 * x.[223] // bcb
            3.0 * x.[224] // bcc
            3.0 * x.[225] // cAA
            3.0 * x.[226] // cAB
            3.0 * x.[227] // cAC
            3.0 * x.[228] // cAa
            3.0 * x.[229] // cAb
            3.0 * x.[230] // cAc
            3.0 * x.[231] // cBA
            3.0 * x.[232] // cBB
            3.0 * x.[233] // cBC
            3.0 * x.[234] // cBa
            3.0 * x.[235] // cBb
            3.0 * x.[236] // cBc
            3.0 * x.[237] // cCA
            3.0 * x.[238] // cCB
            3.0 * x.[239] // cCC
            3.0 * x.[240] // cCa
            3.0 * x.[241] // cCb
            3.0 * x.[242] // cCc
            3.0 * x.[243] // caA
            3.0 * x.[244] // caB
            3.0 * x.[245] // caC
            3.0 * x.[246] // caa
            3.0 * x.[247] // cab
            3.0 * x.[248] // cac
            3.0 * x.[249] // cbA
            3.0 * x.[250] // cbB
            3.0 * x.[251] // cbC
            3.0 * x.[252] // cba
            3.0 * x.[253] // cbb
            3.0 * x.[254] // cbc
            3.0 * x.[255] // ccA
            3.0 * x.[256] // ccB
            3.0 * x.[257] // ccC
            3.0 * x.[258] // cca
            3.0 * x.[259] // ccb
            3.0 * x.[260] // ccc
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[3] // A
                    2.0 * x.[9] // AA
                    x.[10] // AB
                    x.[11] // AC
                    x.[12] // Aa
                    x.[13] // Ab
                    x.[14] // Ac
                    x.[15] // BA
                    x.[21] // CA
                    x.[27] // aA
                    x.[33] // bA
                    x.[39] // cA
                    3.0 * x.[45] // AAA
                    2.0 * x.[46] // AAB
                    2.0 * x.[47] // AAC
                    2.0 * x.[48] // AAa
                    2.0 * x.[49] // AAb
                    2.0 * x.[50] // AAc
                    2.0 * x.[51] // ABA
                    x.[52] // ABB
                    x.[53] // ABC
                    x.[54] // ABa
                    x.[55] // ABb
                    x.[56] // ABc
                    2.0 * x.[57] // ACA
                    x.[58] // ACB
                    x.[59] // ACC
                    x.[60] // ACa
                    x.[61] // ACb
                    x.[62] // ACc
                    2.0 * x.[63] // AaA
                    x.[64] // AaB
                    x.[65] // AaC
                    x.[66] // Aaa
                    x.[67] // Aab
                    x.[68] // Aac
                    2.0 * x.[69] // AbA
                    x.[70] // AbB
                    x.[71] // AbC
                    x.[72] // Aba
                    x.[73] // Abb
                    x.[74] // Abc
                    2.0 * x.[75] // AcA
                    x.[76] // AcB
                    x.[77] // AcC
                    x.[78] // Aca
                    x.[79] // Acb
                    x.[80] // Acc
                    2.0 * x.[81] // BAA
                    x.[82] // BAB
                    x.[83] // BAC
                    x.[84] // BAa
                    x.[85] // BAb
                    x.[86] // BAc
                    x.[87] // BBA
                    x.[93] // BCA
                    x.[99] // BaA
                    x.[105] // BbA
                    x.[111] // BcA
                    2.0 * x.[117] // CAA
                    x.[118] // CAB
                    x.[119] // CAC
                    x.[120] // CAa
                    x.[121] // CAb
                    x.[122] // CAc
                    x.[123] // CBA
                    x.[129] // CCA
                    x.[135] // CaA
                    x.[141] // CbA
                    x.[147] // CcA
                    2.0 * x.[153] // aAA
                    x.[154] // aAB
                    x.[155] // aAC
                    x.[156] // aAa
                    x.[157] // aAb
                    x.[158] // aAc
                    x.[159] // aBA
                    x.[165] // aCA
                    x.[171] // aaA
                    x.[177] // abA
                    x.[183] // acA
                    2.0 * x.[189] // bAA
                    x.[190] // bAB
                    x.[191] // bAC
                    x.[192] // bAa
                    x.[193] // bAb
                    x.[194] // bAc
                    x.[195] // bBA
                    x.[201] // bCA
                    x.[207] // baA
                    x.[213] // bbA
                    x.[219] // bcA
                    2.0 * x.[225] // cAA
                    x.[226] // cAB
                    x.[227] // cAC
                    x.[228] // cAa
                    x.[229] // cAb
                    x.[230] // cAc
                    x.[231] // cBA
                    x.[237] // cCA
                    x.[243] // caA
                    x.[249] // cbA
                    x.[255] // ccA
                |]
                |> Array.sum
                ,
                [|
                    x.[6] // a
                    x.[12] // Aa
                    x.[18] // Ba
                    x.[24] // Ca
                    x.[27] // aA
                    x.[28] // aB
                    x.[29] // aC
                    2.0 * x.[30] // aa
                    x.[31] // ab
                    x.[32] // ac
                    x.[36] // ba
                    x.[42] // ca
                    x.[48] // AAa
                    x.[54] // ABa
                    x.[60] // ACa
                    x.[63] // AaA
                    x.[64] // AaB
                    x.[65] // AaC
                    2.0 * x.[66] // Aaa
                    x.[67] // Aab
                    x.[68] // Aac
                    x.[72] // Aba
                    x.[78] // Aca
                    x.[84] // BAa
                    x.[90] // BBa
                    x.[96] // BCa
                    x.[99] // BaA
                    x.[100] // BaB
                    x.[101] // BaC
                    2.0 * x.[102] // Baa
                    x.[103] // Bab
                    x.[104] // Bac
                    x.[108] // Bba
                    x.[114] // Bca
                    x.[120] // CAa
                    x.[126] // CBa
                    x.[132] // CCa
                    x.[135] // CaA
                    x.[136] // CaB
                    x.[137] // CaC
                    2.0 * x.[138] // Caa
                    x.[139] // Cab
                    x.[140] // Cac
                    x.[144] // Cba
                    x.[150] // Cca
                    x.[153] // aAA
                    x.[154] // aAB
                    x.[155] // aAC
                    2.0 * x.[156] // aAa
                    x.[157] // aAb
                    x.[158] // aAc
                    x.[159] // aBA
                    x.[160] // aBB
                    x.[161] // aBC
                    2.0 * x.[162] // aBa
                    x.[163] // aBb
                    x.[164] // aBc
                    x.[165] // aCA
                    x.[166] // aCB
                    x.[167] // aCC
                    2.0 * x.[168] // aCa
                    x.[169] // aCb
                    x.[170] // aCc
                    2.0 * x.[171] // aaA
                    2.0 * x.[172] // aaB
                    2.0 * x.[173] // aaC
                    3.0 * x.[174] // aaa
                    2.0 * x.[175] // aab
                    2.0 * x.[176] // aac
                    x.[177] // abA
                    x.[178] // abB
                    x.[179] // abC
                    2.0 * x.[180] // aba
                    x.[181] // abb
                    x.[182] // abc
                    x.[183] // acA
                    x.[184] // acB
                    x.[185] // acC
                    2.0 * x.[186] // aca
                    x.[187] // acb
                    x.[188] // acc
                    x.[192] // bAa
                    x.[198] // bBa
                    x.[204] // bCa
                    x.[207] // baA
                    x.[208] // baB
                    x.[209] // baC
                    2.0 * x.[210] // baa
                    x.[211] // bab
                    x.[212] // bac
                    x.[216] // bba
                    x.[222] // bca
                    x.[228] // cAa
                    x.[234] // cBa
                    x.[240] // cCa
                    x.[243] // caA
                    x.[244] // caB
                    x.[245] // caC
                    2.0 * x.[246] // caa
                    x.[247] // cab
                    x.[248] // cac
                    x.[252] // cba
                    x.[258] // cca
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[4] // B
                    x.[10] // AB
                    x.[15] // BA
                    2.0 * x.[16] // BB
                    x.[17] // BC
                    x.[18] // Ba
                    x.[19] // Bb
                    x.[20] // Bc
                    x.[22] // CB
                    x.[28] // aB
                    x.[34] // bB
                    x.[40] // cB
                    x.[46] // AAB
                    x.[51] // ABA
                    2.0 * x.[52] // ABB
                    x.[53] // ABC
                    x.[54] // ABa
                    x.[55] // ABb
                    x.[56] // ABc
                    x.[58] // ACB
                    x.[64] // AaB
                    x.[70] // AbB
                    x.[76] // AcB
                    x.[81] // BAA
                    2.0 * x.[82] // BAB
                    x.[83] // BAC
                    x.[84] // BAa
                    x.[85] // BAb
                    x.[86] // BAc
                    2.0 * x.[87] // BBA
                    3.0 * x.[88] // BBB
                    2.0 * x.[89] // BBC
                    2.0 * x.[90] // BBa
                    2.0 * x.[91] // BBb
                    2.0 * x.[92] // BBc
                    x.[93] // BCA
                    2.0 * x.[94] // BCB
                    x.[95] // BCC
                    x.[96] // BCa
                    x.[97] // BCb
                    x.[98] // BCc
                    x.[99] // BaA
                    2.0 * x.[100] // BaB
                    x.[101] // BaC
                    x.[102] // Baa
                    x.[103] // Bab
                    x.[104] // Bac
                    x.[105] // BbA
                    2.0 * x.[106] // BbB
                    x.[107] // BbC
                    x.[108] // Bba
                    x.[109] // Bbb
                    x.[110] // Bbc
                    x.[111] // BcA
                    2.0 * x.[112] // BcB
                    x.[113] // BcC
                    x.[114] // Bca
                    x.[115] // Bcb
                    x.[116] // Bcc
                    x.[118] // CAB
                    x.[123] // CBA
                    2.0 * x.[124] // CBB
                    x.[125] // CBC
                    x.[126] // CBa
                    x.[127] // CBb
                    x.[128] // CBc
                    x.[130] // CCB
                    x.[136] // CaB
                    x.[142] // CbB
                    x.[148] // CcB
                    x.[154] // aAB
                    x.[159] // aBA
                    2.0 * x.[160] // aBB
                    x.[161] // aBC
                    x.[162] // aBa
                    x.[163] // aBb
                    x.[164] // aBc
                    x.[166] // aCB
                    x.[172] // aaB
                    x.[178] // abB
                    x.[184] // acB
                    x.[190] // bAB
                    x.[195] // bBA
                    2.0 * x.[196] // bBB
                    x.[197] // bBC
                    x.[198] // bBa
                    x.[199] // bBb
                    x.[200] // bBc
                    x.[202] // bCB
                    x.[208] // baB
                    x.[214] // bbB
                    x.[220] // bcB
                    x.[226] // cAB
                    x.[231] // cBA
                    2.0 * x.[232] // cBB
                    x.[233] // cBC
                    x.[234] // cBa
                    x.[235] // cBb
                    x.[236] // cBc
                    x.[238] // cCB
                    x.[244] // caB
                    x.[250] // cbB
                    x.[256] // ccB
                |]
                |> Array.sum
                ,
                [|
                    x.[7] // b
                    x.[13] // Ab
                    x.[19] // Bb
                    x.[25] // Cb
                    x.[31] // ab
                    x.[33] // bA
                    x.[34] // bB
                    x.[35] // bC
                    x.[36] // ba
                    2.0 * x.[37] // bb
                    x.[38] // bc
                    x.[43] // cb
                    x.[49] // AAb
                    x.[55] // ABb
                    x.[61] // ACb
                    x.[67] // Aab
                    x.[69] // AbA
                    x.[70] // AbB
                    x.[71] // AbC
                    x.[72] // Aba
                    2.0 * x.[73] // Abb
                    x.[74] // Abc
                    x.[79] // Acb
                    x.[85] // BAb
                    x.[91] // BBb
                    x.[97] // BCb
                    x.[103] // Bab
                    x.[105] // BbA
                    x.[106] // BbB
                    x.[107] // BbC
                    x.[108] // Bba
                    2.0 * x.[109] // Bbb
                    x.[110] // Bbc
                    x.[115] // Bcb
                    x.[121] // CAb
                    x.[127] // CBb
                    x.[133] // CCb
                    x.[139] // Cab
                    x.[141] // CbA
                    x.[142] // CbB
                    x.[143] // CbC
                    x.[144] // Cba
                    2.0 * x.[145] // Cbb
                    x.[146] // Cbc
                    x.[151] // Ccb
                    x.[157] // aAb
                    x.[163] // aBb
                    x.[169] // aCb
                    x.[175] // aab
                    x.[177] // abA
                    x.[178] // abB
                    x.[179] // abC
                    x.[180] // aba
                    2.0 * x.[181] // abb
                    x.[182] // abc
                    x.[187] // acb
                    x.[189] // bAA
                    x.[190] // bAB
                    x.[191] // bAC
                    x.[192] // bAa
                    2.0 * x.[193] // bAb
                    x.[194] // bAc
                    x.[195] // bBA
                    x.[196] // bBB
                    x.[197] // bBC
                    x.[198] // bBa
                    2.0 * x.[199] // bBb
                    x.[200] // bBc
                    x.[201] // bCA
                    x.[202] // bCB
                    x.[203] // bCC
                    x.[204] // bCa
                    2.0 * x.[205] // bCb
                    x.[206] // bCc
                    x.[207] // baA
                    x.[208] // baB
                    x.[209] // baC
                    x.[210] // baa
                    2.0 * x.[211] // bab
                    x.[212] // bac
                    2.0 * x.[213] // bbA
                    2.0 * x.[214] // bbB
                    2.0 * x.[215] // bbC
                    2.0 * x.[216] // bba
                    3.0 * x.[217] // bbb
                    2.0 * x.[218] // bbc
                    x.[219] // bcA
                    x.[220] // bcB
                    x.[221] // bcC
                    x.[222] // bca
                    2.0 * x.[223] // bcb
                    x.[224] // bcc
                    x.[229] // cAb
                    x.[235] // cBb
                    x.[241] // cCb
                    x.[247] // cab
                    x.[249] // cbA
                    x.[250] // cbB
                    x.[251] // cbC
                    x.[252] // cba
                    2.0 * x.[253] // cbb
                    x.[254] // cbc
                    x.[259] // ccb
                |]
                |> Array.sum
            )

            // C
            (
                [|
                    x.[5] // C
                    x.[11] // AC
                    x.[17] // BC
                    x.[21] // CA
                    x.[22] // CB
                    2.0 * x.[23] // CC
                    x.[24] // Ca
                    x.[25] // Cb
                    x.[26] // Cc
                    x.[29] // aC
                    x.[35] // bC
                    x.[41] // cC
                    x.[47] // AAC
                    x.[53] // ABC
                    x.[57] // ACA
                    x.[58] // ACB
                    2.0 * x.[59] // ACC
                    x.[60] // ACa
                    x.[61] // ACb
                    x.[62] // ACc
                    x.[65] // AaC
                    x.[71] // AbC
                    x.[77] // AcC
                    x.[83] // BAC
                    x.[89] // BBC
                    x.[93] // BCA
                    x.[94] // BCB
                    2.0 * x.[95] // BCC
                    x.[96] // BCa
                    x.[97] // BCb
                    x.[98] // BCc
                    x.[101] // BaC
                    x.[107] // BbC
                    x.[113] // BcC
                    x.[117] // CAA
                    x.[118] // CAB
                    2.0 * x.[119] // CAC
                    x.[120] // CAa
                    x.[121] // CAb
                    x.[122] // CAc
                    x.[123] // CBA
                    x.[124] // CBB
                    2.0 * x.[125] // CBC
                    x.[126] // CBa
                    x.[127] // CBb
                    x.[128] // CBc
                    2.0 * x.[129] // CCA
                    2.0 * x.[130] // CCB
                    3.0 * x.[131] // CCC
                    2.0 * x.[132] // CCa
                    2.0 * x.[133] // CCb
                    2.0 * x.[134] // CCc
                    x.[135] // CaA
                    x.[136] // CaB
                    2.0 * x.[137] // CaC
                    x.[138] // Caa
                    x.[139] // Cab
                    x.[140] // Cac
                    x.[141] // CbA
                    x.[142] // CbB
                    2.0 * x.[143] // CbC
                    x.[144] // Cba
                    x.[145] // Cbb
                    x.[146] // Cbc
                    x.[147] // CcA
                    x.[148] // CcB
                    2.0 * x.[149] // CcC
                    x.[150] // Cca
                    x.[151] // Ccb
                    x.[152] // Ccc
                    x.[155] // aAC
                    x.[161] // aBC
                    x.[165] // aCA
                    x.[166] // aCB
                    2.0 * x.[167] // aCC
                    x.[168] // aCa
                    x.[169] // aCb
                    x.[170] // aCc
                    x.[173] // aaC
                    x.[179] // abC
                    x.[185] // acC
                    x.[191] // bAC
                    x.[197] // bBC
                    x.[201] // bCA
                    x.[202] // bCB
                    2.0 * x.[203] // bCC
                    x.[204] // bCa
                    x.[205] // bCb
                    x.[206] // bCc
                    x.[209] // baC
                    x.[215] // bbC
                    x.[221] // bcC
                    x.[227] // cAC
                    x.[233] // cBC
                    x.[237] // cCA
                    x.[238] // cCB
                    2.0 * x.[239] // cCC
                    x.[240] // cCa
                    x.[241] // cCb
                    x.[242] // cCc
                    x.[245] // caC
                    x.[251] // cbC
                    x.[257] // ccC
                |]
                |> Array.sum
                ,
                [|
                    x.[8] // c
                    x.[14] // Ac
                    x.[20] // Bc
                    x.[26] // Cc
                    x.[32] // ac
                    x.[38] // bc
                    x.[39] // cA
                    x.[40] // cB
                    x.[41] // cC
                    x.[42] // ca
                    x.[43] // cb
                    2.0 * x.[44] // cc
                    x.[50] // AAc
                    x.[56] // ABc
                    x.[62] // ACc
                    x.[68] // Aac
                    x.[74] // Abc
                    x.[75] // AcA
                    x.[76] // AcB
                    x.[77] // AcC
                    x.[78] // Aca
                    x.[79] // Acb
                    2.0 * x.[80] // Acc
                    x.[86] // BAc
                    x.[92] // BBc
                    x.[98] // BCc
                    x.[104] // Bac
                    x.[110] // Bbc
                    x.[111] // BcA
                    x.[112] // BcB
                    x.[113] // BcC
                    x.[114] // Bca
                    x.[115] // Bcb
                    2.0 * x.[116] // Bcc
                    x.[122] // CAc
                    x.[128] // CBc
                    x.[134] // CCc
                    x.[140] // Cac
                    x.[146] // Cbc
                    x.[147] // CcA
                    x.[148] // CcB
                    x.[149] // CcC
                    x.[150] // Cca
                    x.[151] // Ccb
                    2.0 * x.[152] // Ccc
                    x.[158] // aAc
                    x.[164] // aBc
                    x.[170] // aCc
                    x.[176] // aac
                    x.[182] // abc
                    x.[183] // acA
                    x.[184] // acB
                    x.[185] // acC
                    x.[186] // aca
                    x.[187] // acb
                    2.0 * x.[188] // acc
                    x.[194] // bAc
                    x.[200] // bBc
                    x.[206] // bCc
                    x.[212] // bac
                    x.[218] // bbc
                    x.[219] // bcA
                    x.[220] // bcB
                    x.[221] // bcC
                    x.[222] // bca
                    x.[223] // bcb
                    2.0 * x.[224] // bcc
                    x.[225] // cAA
                    x.[226] // cAB
                    x.[227] // cAC
                    x.[228] // cAa
                    x.[229] // cAb
                    2.0 * x.[230] // cAc
                    x.[231] // cBA
                    x.[232] // cBB
                    x.[233] // cBC
                    x.[234] // cBa
                    x.[235] // cBb
                    2.0 * x.[236] // cBc
                    x.[237] // cCA
                    x.[238] // cCB
                    x.[239] // cCC
                    x.[240] // cCa
                    x.[241] // cCb
                    2.0 * x.[242] // cCc
                    x.[243] // caA
                    x.[244] // caB
                    x.[245] // caC
                    x.[246] // caa
                    x.[247] // cab
                    2.0 * x.[248] // cac
                    x.[249] // cbA
                    x.[250] // cbB
                    x.[251] // cbC
                    x.[252] // cba
                    x.[253] // cbb
                    2.0 * x.[254] // cbc
                    2.0 * x.[255] // ccA
                    2.0 * x.[256] // ccB
                    2.0 * x.[257] // ccC
                    2.0 * x.[258] // cca
                    2.0 * x.[259] // ccb
                    3.0 * x.[260] // ccc
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
            0.001 * x.[8] // c | synthesis: Y <-> c
            -0.001 * x.[1] // Y | synthesis: Y <-> c
            0.001 * x.[5] // C | synthesis: Y <-> C
            -0.001 * x.[1] // Y | synthesis: Y <-> C
            0.001 * x.[7] // b | synthesis: Y <-> b
            -0.001 * x.[1] // Y | synthesis: Y <-> b
            0.001 * x.[4] // B | synthesis: Y <-> B
            -0.001 * x.[1] // Y | synthesis: Y <-> B
            0.001 * x.[6] // a | synthesis: Y <-> a
            -0.001 * x.[1] // Y | synthesis: Y <-> a
            0.001 * x.[3] // A | synthesis: Y <-> A
            -0.001 * x.[1] // Y | synthesis: Y <-> A
            0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 2 - Z
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -0.001 * x.[2] // Z | destruction: c <-> Z
            0.001 * x.[8] // c | destruction: c <-> Z
            -0.001 * x.[2] // Z | destruction: C <-> Z
            0.001 * x.[5] // C | destruction: C <-> Z
            -0.001 * x.[2] // Z | destruction: b <-> Z
            0.001 * x.[7] // b | destruction: b <-> Z
            -0.001 * x.[2] // Z | destruction: B <-> Z
            0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[2] // Z | destruction: a <-> Z
            0.001 * x.[6] // a | destruction: a <-> Z
            -0.001 * x.[2] // Z | destruction: A <-> Z
            0.001 * x.[3] // A | destruction: A <-> Z
            -0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[39] // cA | ligation: c + A <-> cA
            -1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[33] // bA | ligation: b + A <-> bA
            -1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[80] // Acc | ligation: A + cc <-> Acc
            -1.0 * x.[3] * x.[44] // A + cc | ligation: A + cc <-> Acc
            1.0 * x.[79] // Acb | ligation: A + cb <-> Acb
            -1.0 * x.[3] * x.[43] // A + cb | ligation: A + cb <-> Acb
            1.0 * x.[78] // Aca | ligation: A + ca <-> Aca
            -1.0 * x.[3] * x.[42] // A + ca | ligation: A + ca <-> Aca
            1.0 * x.[77] // AcC | ligation: A + cC <-> AcC
            -1.0 * x.[3] * x.[41] // A + cC | ligation: A + cC <-> AcC
            1.0 * x.[76] // AcB | ligation: A + cB <-> AcB
            -1.0 * x.[3] * x.[40] // A + cB | ligation: A + cB <-> AcB
            1.0 * x.[75] // AcA | ligation: A + cA <-> AcA
            -1.0 * x.[3] * x.[39] // A + cA | ligation: A + cA <-> AcA
            1.0 * x.[74] // Abc | ligation: A + bc <-> Abc
            -1.0 * x.[3] * x.[38] // A + bc | ligation: A + bc <-> Abc
            1.0 * x.[73] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[37] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[72] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[36] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[71] // AbC | ligation: A + bC <-> AbC
            -1.0 * x.[3] * x.[35] // A + bC | ligation: A + bC <-> AbC
            1.0 * x.[70] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[34] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[69] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[33] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[68] // Aac | ligation: A + ac <-> Aac
            -1.0 * x.[3] * x.[32] // A + ac | ligation: A + ac <-> Aac
            1.0 * x.[67] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[31] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[66] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[30] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[65] // AaC | ligation: A + aC <-> AaC
            -1.0 * x.[3] * x.[29] // A + aC | ligation: A + aC <-> AaC
            1.0 * x.[64] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[28] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[63] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[27] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[62] // ACc | ligation: A + Cc <-> ACc
            -1.0 * x.[3] * x.[26] // A + Cc | ligation: A + Cc <-> ACc
            1.0 * x.[61] // ACb | ligation: A + Cb <-> ACb
            -1.0 * x.[3] * x.[25] // A + Cb | ligation: A + Cb <-> ACb
            1.0 * x.[60] // ACa | ligation: A + Ca <-> ACa
            -1.0 * x.[3] * x.[24] // A + Ca | ligation: A + Ca <-> ACa
            1.0 * x.[59] // ACC | ligation: A + CC <-> ACC
            -1.0 * x.[3] * x.[23] // A + CC | ligation: A + CC <-> ACC
            1.0 * x.[58] // ACB | ligation: A + CB <-> ACB
            -1.0 * x.[3] * x.[22] // A + CB | ligation: A + CB <-> ACB
            1.0 * x.[57] // ACA | ligation: A + CA <-> ACA
            -1.0 * x.[3] * x.[21] // A + CA | ligation: A + CA <-> ACA
            1.0 * x.[56] // ABc | ligation: A + Bc <-> ABc
            -1.0 * x.[3] * x.[20] // A + Bc | ligation: A + Bc <-> ABc
            1.0 * x.[55] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[19] // A + Bb | ligation: A + Bb <-> ABb
            1.0 * x.[54] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[18] // A + Ba | ligation: A + Ba <-> ABa
            1.0 * x.[53] // ABC | ligation: A + BC <-> ABC
            -1.0 * x.[3] * x.[17] // A + BC | ligation: A + BC <-> ABC
            1.0 * x.[52] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[16] // A + BB | ligation: A + BB <-> ABB
            1.0 * x.[51] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[15] // A + BA | ligation: A + BA <-> ABA
            1.0 * x.[50] // AAc | ligation: A + Ac <-> AAc
            -1.0 * x.[3] * x.[14] // A + Ac | ligation: A + Ac <-> AAc
            1.0 * x.[49] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[13] // A + Ab | ligation: A + Ab <-> AAb
            1.0 * x.[48] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[12] // A + Aa | ligation: A + Aa <-> AAa
            1.0 * x.[47] // AAC | ligation: A + AC <-> AAC
            -1.0 * x.[3] * x.[11] // A + AC | ligation: A + AC <-> AAC
            1.0 * x.[46] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[10] // A + AB | ligation: A + AB <-> AAB
            1.0 * x.[45] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[9] // A + AA | ligation: A + AA <-> AAA
            1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
            1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
            1.0 * x.[27] // aA | ligation: a + A <-> aA
            -1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
            1.0 * x.[11] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
            1.0 * x.[10] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[9] // AA | ligation: A + A <-> AA
            1.0 * x.[9] // AA | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            0.001 * x.[2] // Z | destruction: A <-> Z
            -0.001 * x.[3] // A | destruction: A <-> Z
            -0.001 * x.[3] // A | synthesis: Y <-> A
            0.001 * x.[1] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 4 - B
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[40] // cB | ligation: c + B <-> cB
            -1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[116] // Bcc | ligation: B + cc <-> Bcc
            -1.0 * x.[4] * x.[44] // B + cc | ligation: B + cc <-> Bcc
            1.0 * x.[115] // Bcb | ligation: B + cb <-> Bcb
            -1.0 * x.[4] * x.[43] // B + cb | ligation: B + cb <-> Bcb
            1.0 * x.[114] // Bca | ligation: B + ca <-> Bca
            -1.0 * x.[4] * x.[42] // B + ca | ligation: B + ca <-> Bca
            1.0 * x.[113] // BcC | ligation: B + cC <-> BcC
            -1.0 * x.[4] * x.[41] // B + cC | ligation: B + cC <-> BcC
            1.0 * x.[112] // BcB | ligation: B + cB <-> BcB
            -1.0 * x.[4] * x.[40] // B + cB | ligation: B + cB <-> BcB
            1.0 * x.[111] // BcA | ligation: B + cA <-> BcA
            -1.0 * x.[4] * x.[39] // B + cA | ligation: B + cA <-> BcA
            1.0 * x.[110] // Bbc | ligation: B + bc <-> Bbc
            -1.0 * x.[4] * x.[38] // B + bc | ligation: B + bc <-> Bbc
            1.0 * x.[109] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[37] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[108] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[36] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[107] // BbC | ligation: B + bC <-> BbC
            -1.0 * x.[4] * x.[35] // B + bC | ligation: B + bC <-> BbC
            1.0 * x.[106] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[34] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[105] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[33] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[104] // Bac | ligation: B + ac <-> Bac
            -1.0 * x.[4] * x.[32] // B + ac | ligation: B + ac <-> Bac
            1.0 * x.[103] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[31] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[102] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[30] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[101] // BaC | ligation: B + aC <-> BaC
            -1.0 * x.[4] * x.[29] // B + aC | ligation: B + aC <-> BaC
            1.0 * x.[100] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[28] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[99] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[27] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[98] // BCc | ligation: B + Cc <-> BCc
            -1.0 * x.[4] * x.[26] // B + Cc | ligation: B + Cc <-> BCc
            1.0 * x.[97] // BCb | ligation: B + Cb <-> BCb
            -1.0 * x.[4] * x.[25] // B + Cb | ligation: B + Cb <-> BCb
            1.0 * x.[96] // BCa | ligation: B + Ca <-> BCa
            -1.0 * x.[4] * x.[24] // B + Ca | ligation: B + Ca <-> BCa
            1.0 * x.[95] // BCC | ligation: B + CC <-> BCC
            -1.0 * x.[4] * x.[23] // B + CC | ligation: B + CC <-> BCC
            1.0 * x.[94] // BCB | ligation: B + CB <-> BCB
            -1.0 * x.[4] * x.[22] // B + CB | ligation: B + CB <-> BCB
            1.0 * x.[93] // BCA | ligation: B + CA <-> BCA
            -1.0 * x.[4] * x.[21] // B + CA | ligation: B + CA <-> BCA
            1.0 * x.[92] // BBc | ligation: B + Bc <-> BBc
            -1.0 * x.[4] * x.[20] // B + Bc | ligation: B + Bc <-> BBc
            1.0 * x.[91] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[19] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[90] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[18] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[89] // BBC | ligation: B + BC <-> BBC
            -1.0 * x.[4] * x.[17] // B + BC | ligation: B + BC <-> BBC
            1.0 * x.[88] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[16] // B + BB | ligation: B + BB <-> BBB
            1.0 * x.[87] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[15] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[86] // BAc | ligation: B + Ac <-> BAc
            -1.0 * x.[4] * x.[14] // B + Ac | ligation: B + Ac <-> BAc
            1.0 * x.[85] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[13] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[84] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[12] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[83] // BAC | ligation: B + AC <-> BAC
            -1.0 * x.[4] * x.[11] // B + AC | ligation: B + AC <-> BAC
            1.0 * x.[82] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[10] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[81] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[9] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
            1.0 * x.[34] // bB | ligation: b + B <-> bB
            -1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
            1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
            1.0 * x.[17] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[16] // BB | ligation: B + B <-> BB
            1.0 * x.[16] // BB | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[28] // aB | ligation: a + B <-> aB
            -1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[10] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            0.001 * x.[2] // Z | destruction: B <-> Z
            -0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[4] // B | synthesis: Y <-> B
            0.001 * x.[1] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 5 - C
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[152] // Ccc | ligation: C + cc <-> Ccc
            -1.0 * x.[5] * x.[44] // C + cc | ligation: C + cc <-> Ccc
            1.0 * x.[151] // Ccb | ligation: C + cb <-> Ccb
            -1.0 * x.[5] * x.[43] // C + cb | ligation: C + cb <-> Ccb
            1.0 * x.[150] // Cca | ligation: C + ca <-> Cca
            -1.0 * x.[5] * x.[42] // C + ca | ligation: C + ca <-> Cca
            1.0 * x.[149] // CcC | ligation: C + cC <-> CcC
            -1.0 * x.[5] * x.[41] // C + cC | ligation: C + cC <-> CcC
            1.0 * x.[148] // CcB | ligation: C + cB <-> CcB
            -1.0 * x.[5] * x.[40] // C + cB | ligation: C + cB <-> CcB
            1.0 * x.[147] // CcA | ligation: C + cA <-> CcA
            -1.0 * x.[5] * x.[39] // C + cA | ligation: C + cA <-> CcA
            1.0 * x.[146] // Cbc | ligation: C + bc <-> Cbc
            -1.0 * x.[5] * x.[38] // C + bc | ligation: C + bc <-> Cbc
            1.0 * x.[145] // Cbb | ligation: C + bb <-> Cbb
            -1.0 * x.[5] * x.[37] // C + bb | ligation: C + bb <-> Cbb
            1.0 * x.[144] // Cba | ligation: C + ba <-> Cba
            -1.0 * x.[5] * x.[36] // C + ba | ligation: C + ba <-> Cba
            1.0 * x.[143] // CbC | ligation: C + bC <-> CbC
            -1.0 * x.[5] * x.[35] // C + bC | ligation: C + bC <-> CbC
            1.0 * x.[142] // CbB | ligation: C + bB <-> CbB
            -1.0 * x.[5] * x.[34] // C + bB | ligation: C + bB <-> CbB
            1.0 * x.[141] // CbA | ligation: C + bA <-> CbA
            -1.0 * x.[5] * x.[33] // C + bA | ligation: C + bA <-> CbA
            1.0 * x.[140] // Cac | ligation: C + ac <-> Cac
            -1.0 * x.[5] * x.[32] // C + ac | ligation: C + ac <-> Cac
            1.0 * x.[139] // Cab | ligation: C + ab <-> Cab
            -1.0 * x.[5] * x.[31] // C + ab | ligation: C + ab <-> Cab
            1.0 * x.[138] // Caa | ligation: C + aa <-> Caa
            -1.0 * x.[5] * x.[30] // C + aa | ligation: C + aa <-> Caa
            1.0 * x.[137] // CaC | ligation: C + aC <-> CaC
            -1.0 * x.[5] * x.[29] // C + aC | ligation: C + aC <-> CaC
            1.0 * x.[136] // CaB | ligation: C + aB <-> CaB
            -1.0 * x.[5] * x.[28] // C + aB | ligation: C + aB <-> CaB
            1.0 * x.[135] // CaA | ligation: C + aA <-> CaA
            -1.0 * x.[5] * x.[27] // C + aA | ligation: C + aA <-> CaA
            1.0 * x.[134] // CCc | ligation: C + Cc <-> CCc
            -1.0 * x.[5] * x.[26] // C + Cc | ligation: C + Cc <-> CCc
            1.0 * x.[133] // CCb | ligation: C + Cb <-> CCb
            -1.0 * x.[5] * x.[25] // C + Cb | ligation: C + Cb <-> CCb
            1.0 * x.[132] // CCa | ligation: C + Ca <-> CCa
            -1.0 * x.[5] * x.[24] // C + Ca | ligation: C + Ca <-> CCa
            1.0 * x.[131] // CCC | ligation: C + CC <-> CCC
            -1.0 * x.[5] * x.[23] // C + CC | ligation: C + CC <-> CCC
            1.0 * x.[130] // CCB | ligation: C + CB <-> CCB
            -1.0 * x.[5] * x.[22] // C + CB | ligation: C + CB <-> CCB
            1.0 * x.[129] // CCA | ligation: C + CA <-> CCA
            -1.0 * x.[5] * x.[21] // C + CA | ligation: C + CA <-> CCA
            1.0 * x.[128] // CBc | ligation: C + Bc <-> CBc
            -1.0 * x.[5] * x.[20] // C + Bc | ligation: C + Bc <-> CBc
            1.0 * x.[127] // CBb | ligation: C + Bb <-> CBb
            -1.0 * x.[5] * x.[19] // C + Bb | ligation: C + Bb <-> CBb
            1.0 * x.[126] // CBa | ligation: C + Ba <-> CBa
            -1.0 * x.[5] * x.[18] // C + Ba | ligation: C + Ba <-> CBa
            1.0 * x.[125] // CBC | ligation: C + BC <-> CBC
            -1.0 * x.[5] * x.[17] // C + BC | ligation: C + BC <-> CBC
            1.0 * x.[124] // CBB | ligation: C + BB <-> CBB
            -1.0 * x.[5] * x.[16] // C + BB | ligation: C + BB <-> CBB
            1.0 * x.[123] // CBA | ligation: C + BA <-> CBA
            -1.0 * x.[5] * x.[15] // C + BA | ligation: C + BA <-> CBA
            1.0 * x.[122] // CAc | ligation: C + Ac <-> CAc
            -1.0 * x.[5] * x.[14] // C + Ac | ligation: C + Ac <-> CAc
            1.0 * x.[121] // CAb | ligation: C + Ab <-> CAb
            -1.0 * x.[5] * x.[13] // C + Ab | ligation: C + Ab <-> CAb
            1.0 * x.[120] // CAa | ligation: C + Aa <-> CAa
            -1.0 * x.[5] * x.[12] // C + Aa | ligation: C + Aa <-> CAa
            1.0 * x.[119] // CAC | ligation: C + AC <-> CAC
            -1.0 * x.[5] * x.[11] // C + AC | ligation: C + AC <-> CAC
            1.0 * x.[118] // CAB | ligation: C + AB <-> CAB
            -1.0 * x.[5] * x.[10] // C + AB | ligation: C + AB <-> CAB
            1.0 * x.[117] // CAA | ligation: C + AA <-> CAA
            -1.0 * x.[5] * x.[9] // C + AA | ligation: C + AA <-> CAA
            1.0 * x.[41] // cC | ligation: c + C <-> cC
            -1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
            1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
            1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
            1.0 * x.[23] // CC | ligation: C + C <-> CC
            1.0 * x.[23] // CC | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            1.0 * x.[35] // bC | ligation: b + C <-> bC
            -1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[17] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[29] // aC | ligation: a + C <-> aC
            -1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[11] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
            0.001 * x.[2] // Z | destruction: C <-> Z
            -0.001 * x.[5] // C | destruction: C <-> Z
            -0.001 * x.[5] // C | synthesis: Y <-> C
            0.001 * x.[1] // Y | synthesis: Y <-> C
        |]
        |> Array.sum


    // 6 - a
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
            1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
            1.0 * x.[167] // aCC | ligation: a + CC <-> aCC
            -1.0 * x.[6] * x.[23] // a + CC | ligation: a + CC <-> aCC
            1.0 * x.[166] // aCB | ligation: a + CB <-> aCB
            -1.0 * x.[6] * x.[22] // a + CB | ligation: a + CB <-> aCB
            1.0 * x.[165] // aCA | ligation: a + CA <-> aCA
            -1.0 * x.[6] * x.[21] // a + CA | ligation: a + CA <-> aCA
            1.0 * x.[170] // aCc | ligation: a + Cc <-> aCc
            -1.0 * x.[6] * x.[26] // a + Cc | ligation: a + Cc <-> aCc
            1.0 * x.[169] // aCb | ligation: a + Cb <-> aCb
            -1.0 * x.[6] * x.[25] // a + Cb | ligation: a + Cb <-> aCb
            1.0 * x.[168] // aCa | ligation: a + Ca <-> aCa
            -1.0 * x.[6] * x.[24] // a + Ca | ligation: a + Ca <-> aCa
            1.0 * x.[161] // aBC | ligation: a + BC <-> aBC
            -1.0 * x.[6] * x.[17] // a + BC | ligation: a + BC <-> aBC
            1.0 * x.[160] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[6] * x.[16] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[159] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[6] * x.[15] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[164] // aBc | ligation: a + Bc <-> aBc
            -1.0 * x.[6] * x.[20] // a + Bc | ligation: a + Bc <-> aBc
            1.0 * x.[163] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[6] * x.[19] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[162] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[6] * x.[18] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[155] // aAC | ligation: a + AC <-> aAC
            -1.0 * x.[6] * x.[11] // a + AC | ligation: a + AC <-> aAC
            1.0 * x.[154] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[6] * x.[10] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[153] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[6] * x.[9] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[158] // aAc | ligation: a + Ac <-> aAc
            -1.0 * x.[6] * x.[14] // a + Ac | ligation: a + Ac <-> aAc
            1.0 * x.[157] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[6] * x.[13] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[156] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[6] * x.[12] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[185] // acC | ligation: a + cC <-> acC
            -1.0 * x.[6] * x.[41] // a + cC | ligation: a + cC <-> acC
            1.0 * x.[184] // acB | ligation: a + cB <-> acB
            -1.0 * x.[6] * x.[40] // a + cB | ligation: a + cB <-> acB
            1.0 * x.[183] // acA | ligation: a + cA <-> acA
            -1.0 * x.[6] * x.[39] // a + cA | ligation: a + cA <-> acA
            1.0 * x.[188] // acc | ligation: a + cc <-> acc
            -1.0 * x.[6] * x.[44] // a + cc | ligation: a + cc <-> acc
            1.0 * x.[187] // acb | ligation: a + cb <-> acb
            -1.0 * x.[6] * x.[43] // a + cb | ligation: a + cb <-> acb
            1.0 * x.[186] // aca | ligation: a + ca <-> aca
            -1.0 * x.[6] * x.[42] // a + ca | ligation: a + ca <-> aca
            1.0 * x.[179] // abC | ligation: a + bC <-> abC
            -1.0 * x.[6] * x.[35] // a + bC | ligation: a + bC <-> abC
            1.0 * x.[178] // abB | ligation: a + bB <-> abB
            -1.0 * x.[6] * x.[34] // a + bB | ligation: a + bB <-> abB
            1.0 * x.[177] // abA | ligation: a + bA <-> abA
            -1.0 * x.[6] * x.[33] // a + bA | ligation: a + bA <-> abA
            1.0 * x.[182] // abc | ligation: a + bc <-> abc
            -1.0 * x.[6] * x.[38] // a + bc | ligation: a + bc <-> abc
            1.0 * x.[181] // abb | ligation: a + bb <-> abb
            -1.0 * x.[6] * x.[37] // a + bb | ligation: a + bb <-> abb
            1.0 * x.[180] // aba | ligation: a + ba <-> aba
            -1.0 * x.[6] * x.[36] // a + ba | ligation: a + ba <-> aba
            1.0 * x.[173] // aaC | ligation: a + aC <-> aaC
            -1.0 * x.[6] * x.[29] // a + aC | ligation: a + aC <-> aaC
            1.0 * x.[172] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[6] * x.[28] // a + aB | ligation: a + aB <-> aaB
            1.0 * x.[171] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[6] * x.[27] // a + aA | ligation: a + aA <-> aaA
            1.0 * x.[176] // aac | ligation: a + ac <-> aac
            -1.0 * x.[6] * x.[32] // a + ac | ligation: a + ac <-> aac
            1.0 * x.[175] // aab | ligation: a + ab <-> aab
            -1.0 * x.[6] * x.[31] // a + ab | ligation: a + ab <-> aab
            1.0 * x.[174] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[6] * x.[30] // a + aa | ligation: a + aa <-> aaa
            1.0 * x.[29] // aC | ligation: a + C <-> aC
            -1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[28] // aB | ligation: a + B <-> aB
            -1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[27] // aA | ligation: a + A <-> aA
            -1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
            1.0 * x.[32] // ac | ligation: a + c <-> ac
            -1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
            1.0 * x.[31] // ab | ligation: a + b <-> ab
            -1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
            1.0 * x.[30] // aa | ligation: a + a <-> aa
            1.0 * x.[30] // aa | ligation: a + a <-> aa
            -1.0 * x.[6] * x.[6] // a + a | ligation: a + a <-> aa
            -1.0 * x.[6] * x.[6] // a + a | ligation: a + a <-> aa
            0.001 * x.[2] // Z | destruction: a <-> Z
            -0.001 * x.[6] // a | destruction: a <-> Z
            -0.001 * x.[6] // a | synthesis: Y <-> a
            0.001 * x.[1] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 7 - b
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
            1.0 * x.[203] // bCC | ligation: b + CC <-> bCC
            -1.0 * x.[7] * x.[23] // b + CC | ligation: b + CC <-> bCC
            1.0 * x.[202] // bCB | ligation: b + CB <-> bCB
            -1.0 * x.[7] * x.[22] // b + CB | ligation: b + CB <-> bCB
            1.0 * x.[201] // bCA | ligation: b + CA <-> bCA
            -1.0 * x.[7] * x.[21] // b + CA | ligation: b + CA <-> bCA
            1.0 * x.[206] // bCc | ligation: b + Cc <-> bCc
            -1.0 * x.[7] * x.[26] // b + Cc | ligation: b + Cc <-> bCc
            1.0 * x.[205] // bCb | ligation: b + Cb <-> bCb
            -1.0 * x.[7] * x.[25] // b + Cb | ligation: b + Cb <-> bCb
            1.0 * x.[204] // bCa | ligation: b + Ca <-> bCa
            -1.0 * x.[7] * x.[24] // b + Ca | ligation: b + Ca <-> bCa
            1.0 * x.[197] // bBC | ligation: b + BC <-> bBC
            -1.0 * x.[7] * x.[17] // b + BC | ligation: b + BC <-> bBC
            1.0 * x.[196] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[7] * x.[16] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[195] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[7] * x.[15] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[200] // bBc | ligation: b + Bc <-> bBc
            -1.0 * x.[7] * x.[20] // b + Bc | ligation: b + Bc <-> bBc
            1.0 * x.[199] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[7] * x.[19] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[198] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[7] * x.[18] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[191] // bAC | ligation: b + AC <-> bAC
            -1.0 * x.[7] * x.[11] // b + AC | ligation: b + AC <-> bAC
            1.0 * x.[190] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[7] * x.[10] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[189] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[7] * x.[9] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[194] // bAc | ligation: b + Ac <-> bAc
            -1.0 * x.[7] * x.[14] // b + Ac | ligation: b + Ac <-> bAc
            1.0 * x.[193] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[7] * x.[13] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[192] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[7] * x.[12] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[221] // bcC | ligation: b + cC <-> bcC
            -1.0 * x.[7] * x.[41] // b + cC | ligation: b + cC <-> bcC
            1.0 * x.[220] // bcB | ligation: b + cB <-> bcB
            -1.0 * x.[7] * x.[40] // b + cB | ligation: b + cB <-> bcB
            1.0 * x.[219] // bcA | ligation: b + cA <-> bcA
            -1.0 * x.[7] * x.[39] // b + cA | ligation: b + cA <-> bcA
            1.0 * x.[224] // bcc | ligation: b + cc <-> bcc
            -1.0 * x.[7] * x.[44] // b + cc | ligation: b + cc <-> bcc
            1.0 * x.[223] // bcb | ligation: b + cb <-> bcb
            -1.0 * x.[7] * x.[43] // b + cb | ligation: b + cb <-> bcb
            1.0 * x.[222] // bca | ligation: b + ca <-> bca
            -1.0 * x.[7] * x.[42] // b + ca | ligation: b + ca <-> bca
            1.0 * x.[215] // bbC | ligation: b + bC <-> bbC
            -1.0 * x.[7] * x.[35] // b + bC | ligation: b + bC <-> bbC
            1.0 * x.[214] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[7] * x.[34] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[213] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[7] * x.[33] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[218] // bbc | ligation: b + bc <-> bbc
            -1.0 * x.[7] * x.[38] // b + bc | ligation: b + bc <-> bbc
            1.0 * x.[217] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[7] * x.[37] // b + bb | ligation: b + bb <-> bbb
            1.0 * x.[216] // bba | ligation: b + ba <-> bba
            -1.0 * x.[7] * x.[36] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[209] // baC | ligation: b + aC <-> baC
            -1.0 * x.[7] * x.[29] // b + aC | ligation: b + aC <-> baC
            1.0 * x.[208] // baB | ligation: b + aB <-> baB
            -1.0 * x.[7] * x.[28] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[207] // baA | ligation: b + aA <-> baA
            -1.0 * x.[7] * x.[27] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[212] // bac | ligation: b + ac <-> bac
            -1.0 * x.[7] * x.[32] // b + ac | ligation: b + ac <-> bac
            1.0 * x.[211] // bab | ligation: b + ab <-> bab
            -1.0 * x.[7] * x.[31] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[210] // baa | ligation: b + aa <-> baa
            -1.0 * x.[7] * x.[30] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[35] // bC | ligation: b + C <-> bC
            -1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[34] // bB | ligation: b + B <-> bB
            -1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
            1.0 * x.[33] // bA | ligation: b + A <-> bA
            -1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[38] // bc | ligation: b + c <-> bc
            -1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
            1.0 * x.[37] // bb | ligation: b + b <-> bb
            1.0 * x.[37] // bb | ligation: b + b <-> bb
            -1.0 * x.[7] * x.[7] // b + b | ligation: b + b <-> bb
            -1.0 * x.[7] * x.[7] // b + b | ligation: b + b <-> bb
            1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
            1.0 * x.[31] // ab | ligation: a + b <-> ab
            -1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
            0.001 * x.[2] // Z | destruction: b <-> Z
            -0.001 * x.[7] // b | destruction: b <-> Z
            -0.001 * x.[7] // b | synthesis: Y <-> b
            0.001 * x.[1] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 8 - c
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[239] // cCC | ligation: c + CC <-> cCC
            -1.0 * x.[8] * x.[23] // c + CC | ligation: c + CC <-> cCC
            1.0 * x.[238] // cCB | ligation: c + CB <-> cCB
            -1.0 * x.[8] * x.[22] // c + CB | ligation: c + CB <-> cCB
            1.0 * x.[237] // cCA | ligation: c + CA <-> cCA
            -1.0 * x.[8] * x.[21] // c + CA | ligation: c + CA <-> cCA
            1.0 * x.[242] // cCc | ligation: c + Cc <-> cCc
            -1.0 * x.[8] * x.[26] // c + Cc | ligation: c + Cc <-> cCc
            1.0 * x.[241] // cCb | ligation: c + Cb <-> cCb
            -1.0 * x.[8] * x.[25] // c + Cb | ligation: c + Cb <-> cCb
            1.0 * x.[240] // cCa | ligation: c + Ca <-> cCa
            -1.0 * x.[8] * x.[24] // c + Ca | ligation: c + Ca <-> cCa
            1.0 * x.[233] // cBC | ligation: c + BC <-> cBC
            -1.0 * x.[8] * x.[17] // c + BC | ligation: c + BC <-> cBC
            1.0 * x.[232] // cBB | ligation: c + BB <-> cBB
            -1.0 * x.[8] * x.[16] // c + BB | ligation: c + BB <-> cBB
            1.0 * x.[231] // cBA | ligation: c + BA <-> cBA
            -1.0 * x.[8] * x.[15] // c + BA | ligation: c + BA <-> cBA
            1.0 * x.[236] // cBc | ligation: c + Bc <-> cBc
            -1.0 * x.[8] * x.[20] // c + Bc | ligation: c + Bc <-> cBc
            1.0 * x.[235] // cBb | ligation: c + Bb <-> cBb
            -1.0 * x.[8] * x.[19] // c + Bb | ligation: c + Bb <-> cBb
            1.0 * x.[234] // cBa | ligation: c + Ba <-> cBa
            -1.0 * x.[8] * x.[18] // c + Ba | ligation: c + Ba <-> cBa
            1.0 * x.[227] // cAC | ligation: c + AC <-> cAC
            -1.0 * x.[8] * x.[11] // c + AC | ligation: c + AC <-> cAC
            1.0 * x.[226] // cAB | ligation: c + AB <-> cAB
            -1.0 * x.[8] * x.[10] // c + AB | ligation: c + AB <-> cAB
            1.0 * x.[225] // cAA | ligation: c + AA <-> cAA
            -1.0 * x.[8] * x.[9] // c + AA | ligation: c + AA <-> cAA
            1.0 * x.[230] // cAc | ligation: c + Ac <-> cAc
            -1.0 * x.[8] * x.[14] // c + Ac | ligation: c + Ac <-> cAc
            1.0 * x.[229] // cAb | ligation: c + Ab <-> cAb
            -1.0 * x.[8] * x.[13] // c + Ab | ligation: c + Ab <-> cAb
            1.0 * x.[228] // cAa | ligation: c + Aa <-> cAa
            -1.0 * x.[8] * x.[12] // c + Aa | ligation: c + Aa <-> cAa
            1.0 * x.[257] // ccC | ligation: c + cC <-> ccC
            -1.0 * x.[8] * x.[41] // c + cC | ligation: c + cC <-> ccC
            1.0 * x.[256] // ccB | ligation: c + cB <-> ccB
            -1.0 * x.[8] * x.[40] // c + cB | ligation: c + cB <-> ccB
            1.0 * x.[255] // ccA | ligation: c + cA <-> ccA
            -1.0 * x.[8] * x.[39] // c + cA | ligation: c + cA <-> ccA
            1.0 * x.[260] // ccc | ligation: c + cc <-> ccc
            -1.0 * x.[8] * x.[44] // c + cc | ligation: c + cc <-> ccc
            1.0 * x.[259] // ccb | ligation: c + cb <-> ccb
            -1.0 * x.[8] * x.[43] // c + cb | ligation: c + cb <-> ccb
            1.0 * x.[258] // cca | ligation: c + ca <-> cca
            -1.0 * x.[8] * x.[42] // c + ca | ligation: c + ca <-> cca
            1.0 * x.[251] // cbC | ligation: c + bC <-> cbC
            -1.0 * x.[8] * x.[35] // c + bC | ligation: c + bC <-> cbC
            1.0 * x.[250] // cbB | ligation: c + bB <-> cbB
            -1.0 * x.[8] * x.[34] // c + bB | ligation: c + bB <-> cbB
            1.0 * x.[249] // cbA | ligation: c + bA <-> cbA
            -1.0 * x.[8] * x.[33] // c + bA | ligation: c + bA <-> cbA
            1.0 * x.[254] // cbc | ligation: c + bc <-> cbc
            -1.0 * x.[8] * x.[38] // c + bc | ligation: c + bc <-> cbc
            1.0 * x.[253] // cbb | ligation: c + bb <-> cbb
            -1.0 * x.[8] * x.[37] // c + bb | ligation: c + bb <-> cbb
            1.0 * x.[252] // cba | ligation: c + ba <-> cba
            -1.0 * x.[8] * x.[36] // c + ba | ligation: c + ba <-> cba
            1.0 * x.[245] // caC | ligation: c + aC <-> caC
            -1.0 * x.[8] * x.[29] // c + aC | ligation: c + aC <-> caC
            1.0 * x.[244] // caB | ligation: c + aB <-> caB
            -1.0 * x.[8] * x.[28] // c + aB | ligation: c + aB <-> caB
            1.0 * x.[243] // caA | ligation: c + aA <-> caA
            -1.0 * x.[8] * x.[27] // c + aA | ligation: c + aA <-> caA
            1.0 * x.[248] // cac | ligation: c + ac <-> cac
            -1.0 * x.[8] * x.[32] // c + ac | ligation: c + ac <-> cac
            1.0 * x.[247] // cab | ligation: c + ab <-> cab
            -1.0 * x.[8] * x.[31] // c + ab | ligation: c + ab <-> cab
            1.0 * x.[246] // caa | ligation: c + aa <-> caa
            -1.0 * x.[8] * x.[30] // c + aa | ligation: c + aa <-> caa
            1.0 * x.[41] // cC | ligation: c + C <-> cC
            -1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
            1.0 * x.[40] // cB | ligation: c + B <-> cB
            -1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[39] // cA | ligation: c + A <-> cA
            -1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[44] // cc | ligation: c + c <-> cc
            1.0 * x.[44] // cc | ligation: c + c <-> cc
            -1.0 * x.[8] * x.[8] // c + c | ligation: c + c <-> cc
            -1.0 * x.[8] * x.[8] // c + c | ligation: c + c <-> cc
            1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
            1.0 * x.[38] // bc | ligation: b + c <-> bc
            -1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
            1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
            1.0 * x.[32] // ac | ligation: a + c <-> ac
            -1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
            0.001 * x.[2] // Z | destruction: c <-> Z
            -0.001 * x.[8] // c | destruction: c <-> Z
            -0.001 * x.[8] // c | synthesis: Y <-> c
            0.001 * x.[1] // Y | synthesis: Y <-> c
        |]
        |> Array.sum


    // 9 - AA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[225] // cAA | ligation: c + AA <-> cAA
            -1.0 * x.[8] * x.[9] // c + AA | ligation: c + AA <-> cAA
            1.0 * x.[117] // CAA | ligation: C + AA <-> CAA
            -1.0 * x.[5] * x.[9] // C + AA | ligation: C + AA <-> CAA
            1.0 * x.[189] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[7] * x.[9] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[81] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[9] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[153] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[6] * x.[9] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[45] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[9] // A + AA | ligation: A + AA <-> AAA
            -1.0 * x.[9] // AA | ligation: A + A <-> AA
            1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 10 - AB
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[226] // cAB | ligation: c + AB <-> cAB
            -1.0 * x.[8] * x.[10] // c + AB | ligation: c + AB <-> cAB
            1.0 * x.[118] // CAB | ligation: C + AB <-> CAB
            -1.0 * x.[5] * x.[10] // C + AB | ligation: C + AB <-> CAB
            1.0 * x.[190] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[7] * x.[10] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[82] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[10] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[154] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[6] * x.[10] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[46] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[10] // A + AB | ligation: A + AB <-> AAB
            -1.0 * x.[10] // AB | ligation: A + B <-> AB
            1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 11 - AC
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[227] // cAC | ligation: c + AC <-> cAC
            -1.0 * x.[8] * x.[11] // c + AC | ligation: c + AC <-> cAC
            1.0 * x.[119] // CAC | ligation: C + AC <-> CAC
            -1.0 * x.[5] * x.[11] // C + AC | ligation: C + AC <-> CAC
            1.0 * x.[191] // bAC | ligation: b + AC <-> bAC
            -1.0 * x.[7] * x.[11] // b + AC | ligation: b + AC <-> bAC
            1.0 * x.[83] // BAC | ligation: B + AC <-> BAC
            -1.0 * x.[4] * x.[11] // B + AC | ligation: B + AC <-> BAC
            1.0 * x.[155] // aAC | ligation: a + AC <-> aAC
            -1.0 * x.[6] * x.[11] // a + AC | ligation: a + AC <-> aAC
            1.0 * x.[47] // AAC | ligation: A + AC <-> AAC
            -1.0 * x.[3] * x.[11] // A + AC | ligation: A + AC <-> AAC
            -1.0 * x.[11] // AC | ligation: A + C <-> AC
            1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
        |]
        |> Array.sum


    // 12 - Aa
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[228] // cAa | ligation: c + Aa <-> cAa
            -1.0 * x.[8] * x.[12] // c + Aa | ligation: c + Aa <-> cAa
            1.0 * x.[120] // CAa | ligation: C + Aa <-> CAa
            -1.0 * x.[5] * x.[12] // C + Aa | ligation: C + Aa <-> CAa
            1.0 * x.[192] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[7] * x.[12] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[84] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[12] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[156] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[6] * x.[12] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[48] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[12] // A + Aa | ligation: A + Aa <-> AAa
            -1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
        |]
        |> Array.sum


    // 13 - Ab
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[229] // cAb | ligation: c + Ab <-> cAb
            -1.0 * x.[8] * x.[13] // c + Ab | ligation: c + Ab <-> cAb
            1.0 * x.[121] // CAb | ligation: C + Ab <-> CAb
            -1.0 * x.[5] * x.[13] // C + Ab | ligation: C + Ab <-> CAb
            1.0 * x.[193] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[7] * x.[13] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[85] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[13] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[157] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[6] * x.[13] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[49] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[13] // A + Ab | ligation: A + Ab <-> AAb
            -1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
        |]
        |> Array.sum


    // 14 - Ac
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[230] // cAc | ligation: c + Ac <-> cAc
            -1.0 * x.[8] * x.[14] // c + Ac | ligation: c + Ac <-> cAc
            1.0 * x.[122] // CAc | ligation: C + Ac <-> CAc
            -1.0 * x.[5] * x.[14] // C + Ac | ligation: C + Ac <-> CAc
            1.0 * x.[194] // bAc | ligation: b + Ac <-> bAc
            -1.0 * x.[7] * x.[14] // b + Ac | ligation: b + Ac <-> bAc
            1.0 * x.[86] // BAc | ligation: B + Ac <-> BAc
            -1.0 * x.[4] * x.[14] // B + Ac | ligation: B + Ac <-> BAc
            1.0 * x.[158] // aAc | ligation: a + Ac <-> aAc
            -1.0 * x.[6] * x.[14] // a + Ac | ligation: a + Ac <-> aAc
            1.0 * x.[50] // AAc | ligation: A + Ac <-> AAc
            -1.0 * x.[3] * x.[14] // A + Ac | ligation: A + Ac <-> AAc
            -1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
        |]
        |> Array.sum


    // 15 - BA
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[231] // cBA | ligation: c + BA <-> cBA
            -1.0 * x.[8] * x.[15] // c + BA | ligation: c + BA <-> cBA
            1.0 * x.[123] // CBA | ligation: C + BA <-> CBA
            -1.0 * x.[5] * x.[15] // C + BA | ligation: C + BA <-> CBA
            1.0 * x.[195] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[7] * x.[15] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[87] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[15] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[159] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[6] * x.[15] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[51] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[15] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 16 - BB
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[232] // cBB | ligation: c + BB <-> cBB
            -1.0 * x.[8] * x.[16] // c + BB | ligation: c + BB <-> cBB
            1.0 * x.[124] // CBB | ligation: C + BB <-> CBB
            -1.0 * x.[5] * x.[16] // C + BB | ligation: C + BB <-> CBB
            1.0 * x.[196] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[7] * x.[16] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[88] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[16] // B + BB | ligation: B + BB <-> BBB
            -1.0 * x.[16] // BB | ligation: B + B <-> BB
            1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[160] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[6] * x.[16] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[52] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[16] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 17 - BC
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[233] // cBC | ligation: c + BC <-> cBC
            -1.0 * x.[8] * x.[17] // c + BC | ligation: c + BC <-> cBC
            1.0 * x.[125] // CBC | ligation: C + BC <-> CBC
            -1.0 * x.[5] * x.[17] // C + BC | ligation: C + BC <-> CBC
            1.0 * x.[197] // bBC | ligation: b + BC <-> bBC
            -1.0 * x.[7] * x.[17] // b + BC | ligation: b + BC <-> bBC
            1.0 * x.[89] // BBC | ligation: B + BC <-> BBC
            -1.0 * x.[4] * x.[17] // B + BC | ligation: B + BC <-> BBC
            -1.0 * x.[17] // BC | ligation: B + C <-> BC
            1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[161] // aBC | ligation: a + BC <-> aBC
            -1.0 * x.[6] * x.[17] // a + BC | ligation: a + BC <-> aBC
            1.0 * x.[53] // ABC | ligation: A + BC <-> ABC
            -1.0 * x.[3] * x.[17] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 18 - Ba
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[234] // cBa | ligation: c + Ba <-> cBa
            -1.0 * x.[8] * x.[18] // c + Ba | ligation: c + Ba <-> cBa
            1.0 * x.[126] // CBa | ligation: C + Ba <-> CBa
            -1.0 * x.[5] * x.[18] // C + Ba | ligation: C + Ba <-> CBa
            1.0 * x.[198] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[7] * x.[18] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[90] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[18] // B + Ba | ligation: B + Ba <-> BBa
            -1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
            1.0 * x.[162] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[6] * x.[18] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[54] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[18] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 19 - Bb
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[235] // cBb | ligation: c + Bb <-> cBb
            -1.0 * x.[8] * x.[19] // c + Bb | ligation: c + Bb <-> cBb
            1.0 * x.[127] // CBb | ligation: C + Bb <-> CBb
            -1.0 * x.[5] * x.[19] // C + Bb | ligation: C + Bb <-> CBb
            1.0 * x.[199] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[7] * x.[19] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[91] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[19] // B + Bb | ligation: B + Bb <-> BBb
            -1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
            1.0 * x.[163] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[6] * x.[19] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[55] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[19] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 20 - Bc
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[236] // cBc | ligation: c + Bc <-> cBc
            -1.0 * x.[8] * x.[20] // c + Bc | ligation: c + Bc <-> cBc
            1.0 * x.[128] // CBc | ligation: C + Bc <-> CBc
            -1.0 * x.[5] * x.[20] // C + Bc | ligation: C + Bc <-> CBc
            1.0 * x.[200] // bBc | ligation: b + Bc <-> bBc
            -1.0 * x.[7] * x.[20] // b + Bc | ligation: b + Bc <-> bBc
            1.0 * x.[92] // BBc | ligation: B + Bc <-> BBc
            -1.0 * x.[4] * x.[20] // B + Bc | ligation: B + Bc <-> BBc
            -1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
            1.0 * x.[164] // aBc | ligation: a + Bc <-> aBc
            -1.0 * x.[6] * x.[20] // a + Bc | ligation: a + Bc <-> aBc
            1.0 * x.[56] // ABc | ligation: A + Bc <-> ABc
            -1.0 * x.[3] * x.[20] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 21 - CA
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[237] // cCA | ligation: c + CA <-> cCA
            -1.0 * x.[8] * x.[21] // c + CA | ligation: c + CA <-> cCA
            1.0 * x.[129] // CCA | ligation: C + CA <-> CCA
            -1.0 * x.[5] * x.[21] // C + CA | ligation: C + CA <-> CCA
            1.0 * x.[201] // bCA | ligation: b + CA <-> bCA
            -1.0 * x.[7] * x.[21] // b + CA | ligation: b + CA <-> bCA
            1.0 * x.[93] // BCA | ligation: B + CA <-> BCA
            -1.0 * x.[4] * x.[21] // B + CA | ligation: B + CA <-> BCA
            1.0 * x.[165] // aCA | ligation: a + CA <-> aCA
            -1.0 * x.[6] * x.[21] // a + CA | ligation: a + CA <-> aCA
            1.0 * x.[57] // ACA | ligation: A + CA <-> ACA
            -1.0 * x.[3] * x.[21] // A + CA | ligation: A + CA <-> ACA
        |]
        |> Array.sum


    // 22 - CB
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[238] // cCB | ligation: c + CB <-> cCB
            -1.0 * x.[8] * x.[22] // c + CB | ligation: c + CB <-> cCB
            1.0 * x.[130] // CCB | ligation: C + CB <-> CCB
            -1.0 * x.[5] * x.[22] // C + CB | ligation: C + CB <-> CCB
            1.0 * x.[202] // bCB | ligation: b + CB <-> bCB
            -1.0 * x.[7] * x.[22] // b + CB | ligation: b + CB <-> bCB
            1.0 * x.[94] // BCB | ligation: B + CB <-> BCB
            -1.0 * x.[4] * x.[22] // B + CB | ligation: B + CB <-> BCB
            1.0 * x.[166] // aCB | ligation: a + CB <-> aCB
            -1.0 * x.[6] * x.[22] // a + CB | ligation: a + CB <-> aCB
            1.0 * x.[58] // ACB | ligation: A + CB <-> ACB
            -1.0 * x.[3] * x.[22] // A + CB | ligation: A + CB <-> ACB
        |]
        |> Array.sum


    // 23 - CC
    let d23 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[239] // cCC | ligation: c + CC <-> cCC
            -1.0 * x.[8] * x.[23] // c + CC | ligation: c + CC <-> cCC
            1.0 * x.[131] // CCC | ligation: C + CC <-> CCC
            -1.0 * x.[5] * x.[23] // C + CC | ligation: C + CC <-> CCC
            -1.0 * x.[23] // CC | ligation: C + C <-> CC
            1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            1.0 * x.[203] // bCC | ligation: b + CC <-> bCC
            -1.0 * x.[7] * x.[23] // b + CC | ligation: b + CC <-> bCC
            1.0 * x.[95] // BCC | ligation: B + CC <-> BCC
            -1.0 * x.[4] * x.[23] // B + CC | ligation: B + CC <-> BCC
            1.0 * x.[167] // aCC | ligation: a + CC <-> aCC
            -1.0 * x.[6] * x.[23] // a + CC | ligation: a + CC <-> aCC
            1.0 * x.[59] // ACC | ligation: A + CC <-> ACC
            -1.0 * x.[3] * x.[23] // A + CC | ligation: A + CC <-> ACC
        |]
        |> Array.sum


    // 24 - Ca
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[240] // cCa | ligation: c + Ca <-> cCa
            -1.0 * x.[8] * x.[24] // c + Ca | ligation: c + Ca <-> cCa
            1.0 * x.[132] // CCa | ligation: C + Ca <-> CCa
            -1.0 * x.[5] * x.[24] // C + Ca | ligation: C + Ca <-> CCa
            -1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
            1.0 * x.[204] // bCa | ligation: b + Ca <-> bCa
            -1.0 * x.[7] * x.[24] // b + Ca | ligation: b + Ca <-> bCa
            1.0 * x.[96] // BCa | ligation: B + Ca <-> BCa
            -1.0 * x.[4] * x.[24] // B + Ca | ligation: B + Ca <-> BCa
            1.0 * x.[168] // aCa | ligation: a + Ca <-> aCa
            -1.0 * x.[6] * x.[24] // a + Ca | ligation: a + Ca <-> aCa
            1.0 * x.[60] // ACa | ligation: A + Ca <-> ACa
            -1.0 * x.[3] * x.[24] // A + Ca | ligation: A + Ca <-> ACa
        |]
        |> Array.sum


    // 25 - Cb
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[241] // cCb | ligation: c + Cb <-> cCb
            -1.0 * x.[8] * x.[25] // c + Cb | ligation: c + Cb <-> cCb
            1.0 * x.[133] // CCb | ligation: C + Cb <-> CCb
            -1.0 * x.[5] * x.[25] // C + Cb | ligation: C + Cb <-> CCb
            -1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
            1.0 * x.[205] // bCb | ligation: b + Cb <-> bCb
            -1.0 * x.[7] * x.[25] // b + Cb | ligation: b + Cb <-> bCb
            1.0 * x.[97] // BCb | ligation: B + Cb <-> BCb
            -1.0 * x.[4] * x.[25] // B + Cb | ligation: B + Cb <-> BCb
            1.0 * x.[169] // aCb | ligation: a + Cb <-> aCb
            -1.0 * x.[6] * x.[25] // a + Cb | ligation: a + Cb <-> aCb
            1.0 * x.[61] // ACb | ligation: A + Cb <-> ACb
            -1.0 * x.[3] * x.[25] // A + Cb | ligation: A + Cb <-> ACb
        |]
        |> Array.sum


    // 26 - Cc
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[242] // cCc | ligation: c + Cc <-> cCc
            -1.0 * x.[8] * x.[26] // c + Cc | ligation: c + Cc <-> cCc
            1.0 * x.[134] // CCc | ligation: C + Cc <-> CCc
            -1.0 * x.[5] * x.[26] // C + Cc | ligation: C + Cc <-> CCc
            -1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
            1.0 * x.[206] // bCc | ligation: b + Cc <-> bCc
            -1.0 * x.[7] * x.[26] // b + Cc | ligation: b + Cc <-> bCc
            1.0 * x.[98] // BCc | ligation: B + Cc <-> BCc
            -1.0 * x.[4] * x.[26] // B + Cc | ligation: B + Cc <-> BCc
            1.0 * x.[170] // aCc | ligation: a + Cc <-> aCc
            -1.0 * x.[6] * x.[26] // a + Cc | ligation: a + Cc <-> aCc
            1.0 * x.[62] // ACc | ligation: A + Cc <-> ACc
            -1.0 * x.[3] * x.[26] // A + Cc | ligation: A + Cc <-> ACc
        |]
        |> Array.sum


    // 27 - aA
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[135] // CaA | ligation: C + aA <-> CaA
            -1.0 * x.[5] * x.[27] // C + aA | ligation: C + aA <-> CaA
            1.0 * x.[243] // caA | ligation: c + aA <-> caA
            -1.0 * x.[8] * x.[27] // c + aA | ligation: c + aA <-> caA
            1.0 * x.[99] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[27] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[207] // baA | ligation: b + aA <-> baA
            -1.0 * x.[7] * x.[27] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[63] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[27] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[171] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[6] * x.[27] // a + aA | ligation: a + aA <-> aaA
            -1.0 * x.[27] // aA | ligation: a + A <-> aA
            1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
        |]
        |> Array.sum


    // 28 - aB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[136] // CaB | ligation: C + aB <-> CaB
            -1.0 * x.[5] * x.[28] // C + aB | ligation: C + aB <-> CaB
            1.0 * x.[244] // caB | ligation: c + aB <-> caB
            -1.0 * x.[8] * x.[28] // c + aB | ligation: c + aB <-> caB
            1.0 * x.[100] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[28] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[208] // baB | ligation: b + aB <-> baB
            -1.0 * x.[7] * x.[28] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[64] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[28] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[172] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[6] * x.[28] // a + aB | ligation: a + aB <-> aaB
            -1.0 * x.[28] // aB | ligation: a + B <-> aB
            1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
        |]
        |> Array.sum


    // 29 - aC
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[137] // CaC | ligation: C + aC <-> CaC
            -1.0 * x.[5] * x.[29] // C + aC | ligation: C + aC <-> CaC
            1.0 * x.[245] // caC | ligation: c + aC <-> caC
            -1.0 * x.[8] * x.[29] // c + aC | ligation: c + aC <-> caC
            1.0 * x.[101] // BaC | ligation: B + aC <-> BaC
            -1.0 * x.[4] * x.[29] // B + aC | ligation: B + aC <-> BaC
            1.0 * x.[209] // baC | ligation: b + aC <-> baC
            -1.0 * x.[7] * x.[29] // b + aC | ligation: b + aC <-> baC
            1.0 * x.[65] // AaC | ligation: A + aC <-> AaC
            -1.0 * x.[3] * x.[29] // A + aC | ligation: A + aC <-> AaC
            1.0 * x.[173] // aaC | ligation: a + aC <-> aaC
            -1.0 * x.[6] * x.[29] // a + aC | ligation: a + aC <-> aaC
            -1.0 * x.[29] // aC | ligation: a + C <-> aC
            1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
        |]
        |> Array.sum


    // 30 - aa
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[138] // Caa | ligation: C + aa <-> Caa
            -1.0 * x.[5] * x.[30] // C + aa | ligation: C + aa <-> Caa
            1.0 * x.[246] // caa | ligation: c + aa <-> caa
            -1.0 * x.[8] * x.[30] // c + aa | ligation: c + aa <-> caa
            1.0 * x.[102] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[30] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[210] // baa | ligation: b + aa <-> baa
            -1.0 * x.[7] * x.[30] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[66] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[30] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[174] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[6] * x.[30] // a + aa | ligation: a + aa <-> aaa
            -1.0 * x.[30] // aa | ligation: a + a <-> aa
            1.0 * x.[6] * x.[6] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 31 - ab
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[139] // Cab | ligation: C + ab <-> Cab
            -1.0 * x.[5] * x.[31] // C + ab | ligation: C + ab <-> Cab
            1.0 * x.[247] // cab | ligation: c + ab <-> cab
            -1.0 * x.[8] * x.[31] // c + ab | ligation: c + ab <-> cab
            1.0 * x.[103] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[31] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[211] // bab | ligation: b + ab <-> bab
            -1.0 * x.[7] * x.[31] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[67] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[31] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[175] // aab | ligation: a + ab <-> aab
            -1.0 * x.[6] * x.[31] // a + ab | ligation: a + ab <-> aab
            -1.0 * x.[31] // ab | ligation: a + b <-> ab
            1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 32 - ac
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[140] // Cac | ligation: C + ac <-> Cac
            -1.0 * x.[5] * x.[32] // C + ac | ligation: C + ac <-> Cac
            1.0 * x.[248] // cac | ligation: c + ac <-> cac
            -1.0 * x.[8] * x.[32] // c + ac | ligation: c + ac <-> cac
            1.0 * x.[104] // Bac | ligation: B + ac <-> Bac
            -1.0 * x.[4] * x.[32] // B + ac | ligation: B + ac <-> Bac
            1.0 * x.[212] // bac | ligation: b + ac <-> bac
            -1.0 * x.[7] * x.[32] // b + ac | ligation: b + ac <-> bac
            1.0 * x.[68] // Aac | ligation: A + ac <-> Aac
            -1.0 * x.[3] * x.[32] // A + ac | ligation: A + ac <-> Aac
            1.0 * x.[176] // aac | ligation: a + ac <-> aac
            -1.0 * x.[6] * x.[32] // a + ac | ligation: a + ac <-> aac
            -1.0 * x.[32] // ac | ligation: a + c <-> ac
            1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
        |]
        |> Array.sum


    // 33 - bA
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[141] // CbA | ligation: C + bA <-> CbA
            -1.0 * x.[5] * x.[33] // C + bA | ligation: C + bA <-> CbA
            1.0 * x.[249] // cbA | ligation: c + bA <-> cbA
            -1.0 * x.[8] * x.[33] // c + bA | ligation: c + bA <-> cbA
            1.0 * x.[105] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[33] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[213] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[7] * x.[33] // b + bA | ligation: b + bA <-> bbA
            -1.0 * x.[33] // bA | ligation: b + A <-> bA
            1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[69] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[33] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[177] // abA | ligation: a + bA <-> abA
            -1.0 * x.[6] * x.[33] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 34 - bB
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[142] // CbB | ligation: C + bB <-> CbB
            -1.0 * x.[5] * x.[34] // C + bB | ligation: C + bB <-> CbB
            1.0 * x.[250] // cbB | ligation: c + bB <-> cbB
            -1.0 * x.[8] * x.[34] // c + bB | ligation: c + bB <-> cbB
            1.0 * x.[106] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[34] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[214] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[7] * x.[34] // b + bB | ligation: b + bB <-> bbB
            -1.0 * x.[34] // bB | ligation: b + B <-> bB
            1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[70] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[34] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[178] // abB | ligation: a + bB <-> abB
            -1.0 * x.[6] * x.[34] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 35 - bC
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[143] // CbC | ligation: C + bC <-> CbC
            -1.0 * x.[5] * x.[35] // C + bC | ligation: C + bC <-> CbC
            1.0 * x.[251] // cbC | ligation: c + bC <-> cbC
            -1.0 * x.[8] * x.[35] // c + bC | ligation: c + bC <-> cbC
            1.0 * x.[107] // BbC | ligation: B + bC <-> BbC
            -1.0 * x.[4] * x.[35] // B + bC | ligation: B + bC <-> BbC
            1.0 * x.[215] // bbC | ligation: b + bC <-> bbC
            -1.0 * x.[7] * x.[35] // b + bC | ligation: b + bC <-> bbC
            -1.0 * x.[35] // bC | ligation: b + C <-> bC
            1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[71] // AbC | ligation: A + bC <-> AbC
            -1.0 * x.[3] * x.[35] // A + bC | ligation: A + bC <-> AbC
            1.0 * x.[179] // abC | ligation: a + bC <-> abC
            -1.0 * x.[6] * x.[35] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 36 - ba
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[144] // Cba | ligation: C + ba <-> Cba
            -1.0 * x.[5] * x.[36] // C + ba | ligation: C + ba <-> Cba
            1.0 * x.[252] // cba | ligation: c + ba <-> cba
            -1.0 * x.[8] * x.[36] // c + ba | ligation: c + ba <-> cba
            1.0 * x.[108] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[36] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[216] // bba | ligation: b + ba <-> bba
            -1.0 * x.[7] * x.[36] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[72] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[36] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[180] // aba | ligation: a + ba <-> aba
            -1.0 * x.[6] * x.[36] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 37 - bb
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[145] // Cbb | ligation: C + bb <-> Cbb
            -1.0 * x.[5] * x.[37] // C + bb | ligation: C + bb <-> Cbb
            1.0 * x.[253] // cbb | ligation: c + bb <-> cbb
            -1.0 * x.[8] * x.[37] // c + bb | ligation: c + bb <-> cbb
            1.0 * x.[109] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[37] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[217] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[7] * x.[37] // b + bb | ligation: b + bb <-> bbb
            -1.0 * x.[37] // bb | ligation: b + b <-> bb
            1.0 * x.[7] * x.[7] // b + b | ligation: b + b <-> bb
            1.0 * x.[73] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[37] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[181] // abb | ligation: a + bb <-> abb
            -1.0 * x.[6] * x.[37] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 38 - bc
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[146] // Cbc | ligation: C + bc <-> Cbc
            -1.0 * x.[5] * x.[38] // C + bc | ligation: C + bc <-> Cbc
            1.0 * x.[254] // cbc | ligation: c + bc <-> cbc
            -1.0 * x.[8] * x.[38] // c + bc | ligation: c + bc <-> cbc
            1.0 * x.[110] // Bbc | ligation: B + bc <-> Bbc
            -1.0 * x.[4] * x.[38] // B + bc | ligation: B + bc <-> Bbc
            1.0 * x.[218] // bbc | ligation: b + bc <-> bbc
            -1.0 * x.[7] * x.[38] // b + bc | ligation: b + bc <-> bbc
            -1.0 * x.[38] // bc | ligation: b + c <-> bc
            1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
            1.0 * x.[74] // Abc | ligation: A + bc <-> Abc
            -1.0 * x.[3] * x.[38] // A + bc | ligation: A + bc <-> Abc
            1.0 * x.[182] // abc | ligation: a + bc <-> abc
            -1.0 * x.[6] * x.[38] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 39 - cA
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[147] // CcA | ligation: C + cA <-> CcA
            -1.0 * x.[5] * x.[39] // C + cA | ligation: C + cA <-> CcA
            1.0 * x.[255] // ccA | ligation: c + cA <-> ccA
            -1.0 * x.[8] * x.[39] // c + cA | ligation: c + cA <-> ccA
            -1.0 * x.[39] // cA | ligation: c + A <-> cA
            1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[111] // BcA | ligation: B + cA <-> BcA
            -1.0 * x.[4] * x.[39] // B + cA | ligation: B + cA <-> BcA
            1.0 * x.[219] // bcA | ligation: b + cA <-> bcA
            -1.0 * x.[7] * x.[39] // b + cA | ligation: b + cA <-> bcA
            1.0 * x.[75] // AcA | ligation: A + cA <-> AcA
            -1.0 * x.[3] * x.[39] // A + cA | ligation: A + cA <-> AcA
            1.0 * x.[183] // acA | ligation: a + cA <-> acA
            -1.0 * x.[6] * x.[39] // a + cA | ligation: a + cA <-> acA
        |]
        |> Array.sum


    // 40 - cB
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[148] // CcB | ligation: C + cB <-> CcB
            -1.0 * x.[5] * x.[40] // C + cB | ligation: C + cB <-> CcB
            1.0 * x.[256] // ccB | ligation: c + cB <-> ccB
            -1.0 * x.[8] * x.[40] // c + cB | ligation: c + cB <-> ccB
            -1.0 * x.[40] // cB | ligation: c + B <-> cB
            1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[112] // BcB | ligation: B + cB <-> BcB
            -1.0 * x.[4] * x.[40] // B + cB | ligation: B + cB <-> BcB
            1.0 * x.[220] // bcB | ligation: b + cB <-> bcB
            -1.0 * x.[7] * x.[40] // b + cB | ligation: b + cB <-> bcB
            1.0 * x.[76] // AcB | ligation: A + cB <-> AcB
            -1.0 * x.[3] * x.[40] // A + cB | ligation: A + cB <-> AcB
            1.0 * x.[184] // acB | ligation: a + cB <-> acB
            -1.0 * x.[6] * x.[40] // a + cB | ligation: a + cB <-> acB
        |]
        |> Array.sum


    // 41 - cC
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[149] // CcC | ligation: C + cC <-> CcC
            -1.0 * x.[5] * x.[41] // C + cC | ligation: C + cC <-> CcC
            1.0 * x.[257] // ccC | ligation: c + cC <-> ccC
            -1.0 * x.[8] * x.[41] // c + cC | ligation: c + cC <-> ccC
            -1.0 * x.[41] // cC | ligation: c + C <-> cC
            1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[113] // BcC | ligation: B + cC <-> BcC
            -1.0 * x.[4] * x.[41] // B + cC | ligation: B + cC <-> BcC
            1.0 * x.[221] // bcC | ligation: b + cC <-> bcC
            -1.0 * x.[7] * x.[41] // b + cC | ligation: b + cC <-> bcC
            1.0 * x.[77] // AcC | ligation: A + cC <-> AcC
            -1.0 * x.[3] * x.[41] // A + cC | ligation: A + cC <-> AcC
            1.0 * x.[185] // acC | ligation: a + cC <-> acC
            -1.0 * x.[6] * x.[41] // a + cC | ligation: a + cC <-> acC
        |]
        |> Array.sum


    // 42 - ca
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[150] // Cca | ligation: C + ca <-> Cca
            -1.0 * x.[5] * x.[42] // C + ca | ligation: C + ca <-> Cca
            1.0 * x.[258] // cca | ligation: c + ca <-> cca
            -1.0 * x.[8] * x.[42] // c + ca | ligation: c + ca <-> cca
            1.0 * x.[114] // Bca | ligation: B + ca <-> Bca
            -1.0 * x.[4] * x.[42] // B + ca | ligation: B + ca <-> Bca
            1.0 * x.[222] // bca | ligation: b + ca <-> bca
            -1.0 * x.[7] * x.[42] // b + ca | ligation: b + ca <-> bca
            1.0 * x.[78] // Aca | ligation: A + ca <-> Aca
            -1.0 * x.[3] * x.[42] // A + ca | ligation: A + ca <-> Aca
            1.0 * x.[186] // aca | ligation: a + ca <-> aca
            -1.0 * x.[6] * x.[42] // a + ca | ligation: a + ca <-> aca
        |]
        |> Array.sum


    // 43 - cb
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[151] // Ccb | ligation: C + cb <-> Ccb
            -1.0 * x.[5] * x.[43] // C + cb | ligation: C + cb <-> Ccb
            1.0 * x.[259] // ccb | ligation: c + cb <-> ccb
            -1.0 * x.[8] * x.[43] // c + cb | ligation: c + cb <-> ccb
            1.0 * x.[115] // Bcb | ligation: B + cb <-> Bcb
            -1.0 * x.[4] * x.[43] // B + cb | ligation: B + cb <-> Bcb
            1.0 * x.[223] // bcb | ligation: b + cb <-> bcb
            -1.0 * x.[7] * x.[43] // b + cb | ligation: b + cb <-> bcb
            1.0 * x.[79] // Acb | ligation: A + cb <-> Acb
            -1.0 * x.[3] * x.[43] // A + cb | ligation: A + cb <-> Acb
            1.0 * x.[187] // acb | ligation: a + cb <-> acb
            -1.0 * x.[6] * x.[43] // a + cb | ligation: a + cb <-> acb
        |]
        |> Array.sum


    // 44 - cc
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[152] // Ccc | ligation: C + cc <-> Ccc
            -1.0 * x.[5] * x.[44] // C + cc | ligation: C + cc <-> Ccc
            1.0 * x.[260] // ccc | ligation: c + cc <-> ccc
            -1.0 * x.[8] * x.[44] // c + cc | ligation: c + cc <-> ccc
            -1.0 * x.[44] // cc | ligation: c + c <-> cc
            1.0 * x.[8] * x.[8] // c + c | ligation: c + c <-> cc
            1.0 * x.[116] // Bcc | ligation: B + cc <-> Bcc
            -1.0 * x.[4] * x.[44] // B + cc | ligation: B + cc <-> Bcc
            1.0 * x.[224] // bcc | ligation: b + cc <-> bcc
            -1.0 * x.[7] * x.[44] // b + cc | ligation: b + cc <-> bcc
            1.0 * x.[80] // Acc | ligation: A + cc <-> Acc
            -1.0 * x.[3] * x.[44] // A + cc | ligation: A + cc <-> Acc
            1.0 * x.[188] // acc | ligation: a + cc <-> acc
            -1.0 * x.[6] * x.[44] // a + cc | ligation: a + cc <-> acc
        |]
        |> Array.sum


    // 45 - AAA
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[45] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[9] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 46 - AAB
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[46] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[10] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 47 - AAC
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[47] // AAC | ligation: A + AC <-> AAC
            1.0 * x.[3] * x.[11] // A + AC | ligation: A + AC <-> AAC
        |]
        |> Array.sum


    // 48 - AAa
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[48] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[12] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 49 - AAb
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[49] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[13] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 50 - AAc
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[50] // AAc | ligation: A + Ac <-> AAc
            1.0 * x.[3] * x.[14] // A + Ac | ligation: A + Ac <-> AAc
        |]
        |> Array.sum


    // 51 - ABA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[51] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[15] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 52 - ABB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[52] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[16] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 53 - ABC
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[53] // ABC | ligation: A + BC <-> ABC
            1.0 * x.[3] * x.[17] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 54 - ABa
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[54] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[18] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 55 - ABb
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[55] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[19] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 56 - ABc
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[56] // ABc | ligation: A + Bc <-> ABc
            1.0 * x.[3] * x.[20] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 57 - ACA
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[57] // ACA | ligation: A + CA <-> ACA
            1.0 * x.[3] * x.[21] // A + CA | ligation: A + CA <-> ACA
        |]
        |> Array.sum


    // 58 - ACB
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[58] // ACB | ligation: A + CB <-> ACB
            1.0 * x.[3] * x.[22] // A + CB | ligation: A + CB <-> ACB
        |]
        |> Array.sum


    // 59 - ACC
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[59] // ACC | ligation: A + CC <-> ACC
            1.0 * x.[3] * x.[23] // A + CC | ligation: A + CC <-> ACC
        |]
        |> Array.sum


    // 60 - ACa
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[60] // ACa | ligation: A + Ca <-> ACa
            1.0 * x.[3] * x.[24] // A + Ca | ligation: A + Ca <-> ACa
        |]
        |> Array.sum


    // 61 - ACb
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[61] // ACb | ligation: A + Cb <-> ACb
            1.0 * x.[3] * x.[25] // A + Cb | ligation: A + Cb <-> ACb
        |]
        |> Array.sum


    // 62 - ACc
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[62] // ACc | ligation: A + Cc <-> ACc
            1.0 * x.[3] * x.[26] // A + Cc | ligation: A + Cc <-> ACc
        |]
        |> Array.sum


    // 63 - AaA
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[63] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[27] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 64 - AaB
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[64] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[28] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 65 - AaC
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[65] // AaC | ligation: A + aC <-> AaC
            1.0 * x.[3] * x.[29] // A + aC | ligation: A + aC <-> AaC
        |]
        |> Array.sum


    // 66 - Aaa
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[66] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[30] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 67 - Aab
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[67] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[31] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 68 - Aac
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[68] // Aac | ligation: A + ac <-> Aac
            1.0 * x.[3] * x.[32] // A + ac | ligation: A + ac <-> Aac
        |]
        |> Array.sum


    // 69 - AbA
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[69] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[33] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 70 - AbB
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[70] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[34] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 71 - AbC
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[71] // AbC | ligation: A + bC <-> AbC
            1.0 * x.[3] * x.[35] // A + bC | ligation: A + bC <-> AbC
        |]
        |> Array.sum


    // 72 - Aba
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[72] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[36] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 73 - Abb
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[73] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[37] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 74 - Abc
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[74] // Abc | ligation: A + bc <-> Abc
            1.0 * x.[3] * x.[38] // A + bc | ligation: A + bc <-> Abc
        |]
        |> Array.sum


    // 75 - AcA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[75] // AcA | ligation: A + cA <-> AcA
            1.0 * x.[3] * x.[39] // A + cA | ligation: A + cA <-> AcA
        |]
        |> Array.sum


    // 76 - AcB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[76] // AcB | ligation: A + cB <-> AcB
            1.0 * x.[3] * x.[40] // A + cB | ligation: A + cB <-> AcB
        |]
        |> Array.sum


    // 77 - AcC
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[77] // AcC | ligation: A + cC <-> AcC
            1.0 * x.[3] * x.[41] // A + cC | ligation: A + cC <-> AcC
        |]
        |> Array.sum


    // 78 - Aca
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[78] // Aca | ligation: A + ca <-> Aca
            1.0 * x.[3] * x.[42] // A + ca | ligation: A + ca <-> Aca
        |]
        |> Array.sum


    // 79 - Acb
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[79] // Acb | ligation: A + cb <-> Acb
            1.0 * x.[3] * x.[43] // A + cb | ligation: A + cb <-> Acb
        |]
        |> Array.sum


    // 80 - Acc
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[80] // Acc | ligation: A + cc <-> Acc
            1.0 * x.[3] * x.[44] // A + cc | ligation: A + cc <-> Acc
        |]
        |> Array.sum


    // 81 - BAA
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[81] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[9] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 82 - BAB
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[82] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[10] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 83 - BAC
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[83] // BAC | ligation: B + AC <-> BAC
            1.0 * x.[4] * x.[11] // B + AC | ligation: B + AC <-> BAC
        |]
        |> Array.sum


    // 84 - BAa
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[84] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[12] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 85 - BAb
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[85] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[13] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 86 - BAc
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[86] // BAc | ligation: B + Ac <-> BAc
            1.0 * x.[4] * x.[14] // B + Ac | ligation: B + Ac <-> BAc
        |]
        |> Array.sum


    // 87 - BBA
    let d87 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[87] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[15] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 88 - BBB
    let d88 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[88] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[16] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 89 - BBC
    let d89 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[89] // BBC | ligation: B + BC <-> BBC
            1.0 * x.[4] * x.[17] // B + BC | ligation: B + BC <-> BBC
        |]
        |> Array.sum


    // 90 - BBa
    let d90 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[90] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[18] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 91 - BBb
    let d91 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[91] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[19] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 92 - BBc
    let d92 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[92] // BBc | ligation: B + Bc <-> BBc
            1.0 * x.[4] * x.[20] // B + Bc | ligation: B + Bc <-> BBc
        |]
        |> Array.sum


    // 93 - BCA
    let d93 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[93] // BCA | ligation: B + CA <-> BCA
            1.0 * x.[4] * x.[21] // B + CA | ligation: B + CA <-> BCA
        |]
        |> Array.sum


    // 94 - BCB
    let d94 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[94] // BCB | ligation: B + CB <-> BCB
            1.0 * x.[4] * x.[22] // B + CB | ligation: B + CB <-> BCB
        |]
        |> Array.sum


    // 95 - BCC
    let d95 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[95] // BCC | ligation: B + CC <-> BCC
            1.0 * x.[4] * x.[23] // B + CC | ligation: B + CC <-> BCC
        |]
        |> Array.sum


    // 96 - BCa
    let d96 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[96] // BCa | ligation: B + Ca <-> BCa
            1.0 * x.[4] * x.[24] // B + Ca | ligation: B + Ca <-> BCa
        |]
        |> Array.sum


    // 97 - BCb
    let d97 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[97] // BCb | ligation: B + Cb <-> BCb
            1.0 * x.[4] * x.[25] // B + Cb | ligation: B + Cb <-> BCb
        |]
        |> Array.sum


    // 98 - BCc
    let d98 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[98] // BCc | ligation: B + Cc <-> BCc
            1.0 * x.[4] * x.[26] // B + Cc | ligation: B + Cc <-> BCc
        |]
        |> Array.sum


    // 99 - BaA
    let d99 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[99] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[27] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 100 - BaB
    let d100 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[100] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[28] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 101 - BaC
    let d101 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[101] // BaC | ligation: B + aC <-> BaC
            1.0 * x.[4] * x.[29] // B + aC | ligation: B + aC <-> BaC
        |]
        |> Array.sum


    // 102 - Baa
    let d102 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[102] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[30] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 103 - Bab
    let d103 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[103] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[31] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 104 - Bac
    let d104 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[104] // Bac | ligation: B + ac <-> Bac
            1.0 * x.[4] * x.[32] // B + ac | ligation: B + ac <-> Bac
        |]
        |> Array.sum


    // 105 - BbA
    let d105 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[105] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[33] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 106 - BbB
    let d106 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[106] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[34] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 107 - BbC
    let d107 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[107] // BbC | ligation: B + bC <-> BbC
            1.0 * x.[4] * x.[35] // B + bC | ligation: B + bC <-> BbC
        |]
        |> Array.sum


    // 108 - Bba
    let d108 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[108] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[36] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 109 - Bbb
    let d109 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[109] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[37] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 110 - Bbc
    let d110 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[110] // Bbc | ligation: B + bc <-> Bbc
            1.0 * x.[4] * x.[38] // B + bc | ligation: B + bc <-> Bbc
        |]
        |> Array.sum


    // 111 - BcA
    let d111 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[111] // BcA | ligation: B + cA <-> BcA
            1.0 * x.[4] * x.[39] // B + cA | ligation: B + cA <-> BcA
        |]
        |> Array.sum


    // 112 - BcB
    let d112 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[112] // BcB | ligation: B + cB <-> BcB
            1.0 * x.[4] * x.[40] // B + cB | ligation: B + cB <-> BcB
        |]
        |> Array.sum


    // 113 - BcC
    let d113 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[113] // BcC | ligation: B + cC <-> BcC
            1.0 * x.[4] * x.[41] // B + cC | ligation: B + cC <-> BcC
        |]
        |> Array.sum


    // 114 - Bca
    let d114 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[114] // Bca | ligation: B + ca <-> Bca
            1.0 * x.[4] * x.[42] // B + ca | ligation: B + ca <-> Bca
        |]
        |> Array.sum


    // 115 - Bcb
    let d115 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[115] // Bcb | ligation: B + cb <-> Bcb
            1.0 * x.[4] * x.[43] // B + cb | ligation: B + cb <-> Bcb
        |]
        |> Array.sum


    // 116 - Bcc
    let d116 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[116] // Bcc | ligation: B + cc <-> Bcc
            1.0 * x.[4] * x.[44] // B + cc | ligation: B + cc <-> Bcc
        |]
        |> Array.sum


    // 117 - CAA
    let d117 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[117] // CAA | ligation: C + AA <-> CAA
            1.0 * x.[5] * x.[9] // C + AA | ligation: C + AA <-> CAA
        |]
        |> Array.sum


    // 118 - CAB
    let d118 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[118] // CAB | ligation: C + AB <-> CAB
            1.0 * x.[5] * x.[10] // C + AB | ligation: C + AB <-> CAB
        |]
        |> Array.sum


    // 119 - CAC
    let d119 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[119] // CAC | ligation: C + AC <-> CAC
            1.0 * x.[5] * x.[11] // C + AC | ligation: C + AC <-> CAC
        |]
        |> Array.sum


    // 120 - CAa
    let d120 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[120] // CAa | ligation: C + Aa <-> CAa
            1.0 * x.[5] * x.[12] // C + Aa | ligation: C + Aa <-> CAa
        |]
        |> Array.sum


    // 121 - CAb
    let d121 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[121] // CAb | ligation: C + Ab <-> CAb
            1.0 * x.[5] * x.[13] // C + Ab | ligation: C + Ab <-> CAb
        |]
        |> Array.sum


    // 122 - CAc
    let d122 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[122] // CAc | ligation: C + Ac <-> CAc
            1.0 * x.[5] * x.[14] // C + Ac | ligation: C + Ac <-> CAc
        |]
        |> Array.sum


    // 123 - CBA
    let d123 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[123] // CBA | ligation: C + BA <-> CBA
            1.0 * x.[5] * x.[15] // C + BA | ligation: C + BA <-> CBA
        |]
        |> Array.sum


    // 124 - CBB
    let d124 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[124] // CBB | ligation: C + BB <-> CBB
            1.0 * x.[5] * x.[16] // C + BB | ligation: C + BB <-> CBB
        |]
        |> Array.sum


    // 125 - CBC
    let d125 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[125] // CBC | ligation: C + BC <-> CBC
            1.0 * x.[5] * x.[17] // C + BC | ligation: C + BC <-> CBC
        |]
        |> Array.sum


    // 126 - CBa
    let d126 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[126] // CBa | ligation: C + Ba <-> CBa
            1.0 * x.[5] * x.[18] // C + Ba | ligation: C + Ba <-> CBa
        |]
        |> Array.sum


    // 127 - CBb
    let d127 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[127] // CBb | ligation: C + Bb <-> CBb
            1.0 * x.[5] * x.[19] // C + Bb | ligation: C + Bb <-> CBb
        |]
        |> Array.sum


    // 128 - CBc
    let d128 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[128] // CBc | ligation: C + Bc <-> CBc
            1.0 * x.[5] * x.[20] // C + Bc | ligation: C + Bc <-> CBc
        |]
        |> Array.sum


    // 129 - CCA
    let d129 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[129] // CCA | ligation: C + CA <-> CCA
            1.0 * x.[5] * x.[21] // C + CA | ligation: C + CA <-> CCA
        |]
        |> Array.sum


    // 130 - CCB
    let d130 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[130] // CCB | ligation: C + CB <-> CCB
            1.0 * x.[5] * x.[22] // C + CB | ligation: C + CB <-> CCB
        |]
        |> Array.sum


    // 131 - CCC
    let d131 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[131] // CCC | ligation: C + CC <-> CCC
            1.0 * x.[5] * x.[23] // C + CC | ligation: C + CC <-> CCC
        |]
        |> Array.sum


    // 132 - CCa
    let d132 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[132] // CCa | ligation: C + Ca <-> CCa
            1.0 * x.[5] * x.[24] // C + Ca | ligation: C + Ca <-> CCa
        |]
        |> Array.sum


    // 133 - CCb
    let d133 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[133] // CCb | ligation: C + Cb <-> CCb
            1.0 * x.[5] * x.[25] // C + Cb | ligation: C + Cb <-> CCb
        |]
        |> Array.sum


    // 134 - CCc
    let d134 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[134] // CCc | ligation: C + Cc <-> CCc
            1.0 * x.[5] * x.[26] // C + Cc | ligation: C + Cc <-> CCc
        |]
        |> Array.sum


    // 135 - CaA
    let d135 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[135] // CaA | ligation: C + aA <-> CaA
            1.0 * x.[5] * x.[27] // C + aA | ligation: C + aA <-> CaA
        |]
        |> Array.sum


    // 136 - CaB
    let d136 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[136] // CaB | ligation: C + aB <-> CaB
            1.0 * x.[5] * x.[28] // C + aB | ligation: C + aB <-> CaB
        |]
        |> Array.sum


    // 137 - CaC
    let d137 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[137] // CaC | ligation: C + aC <-> CaC
            1.0 * x.[5] * x.[29] // C + aC | ligation: C + aC <-> CaC
        |]
        |> Array.sum


    // 138 - Caa
    let d138 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[138] // Caa | ligation: C + aa <-> Caa
            1.0 * x.[5] * x.[30] // C + aa | ligation: C + aa <-> Caa
        |]
        |> Array.sum


    // 139 - Cab
    let d139 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[139] // Cab | ligation: C + ab <-> Cab
            1.0 * x.[5] * x.[31] // C + ab | ligation: C + ab <-> Cab
        |]
        |> Array.sum


    // 140 - Cac
    let d140 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[140] // Cac | ligation: C + ac <-> Cac
            1.0 * x.[5] * x.[32] // C + ac | ligation: C + ac <-> Cac
        |]
        |> Array.sum


    // 141 - CbA
    let d141 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[141] // CbA | ligation: C + bA <-> CbA
            1.0 * x.[5] * x.[33] // C + bA | ligation: C + bA <-> CbA
        |]
        |> Array.sum


    // 142 - CbB
    let d142 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[142] // CbB | ligation: C + bB <-> CbB
            1.0 * x.[5] * x.[34] // C + bB | ligation: C + bB <-> CbB
        |]
        |> Array.sum


    // 143 - CbC
    let d143 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[143] // CbC | ligation: C + bC <-> CbC
            1.0 * x.[5] * x.[35] // C + bC | ligation: C + bC <-> CbC
        |]
        |> Array.sum


    // 144 - Cba
    let d144 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[144] // Cba | ligation: C + ba <-> Cba
            1.0 * x.[5] * x.[36] // C + ba | ligation: C + ba <-> Cba
        |]
        |> Array.sum


    // 145 - Cbb
    let d145 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[145] // Cbb | ligation: C + bb <-> Cbb
            1.0 * x.[5] * x.[37] // C + bb | ligation: C + bb <-> Cbb
        |]
        |> Array.sum


    // 146 - Cbc
    let d146 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[146] // Cbc | ligation: C + bc <-> Cbc
            1.0 * x.[5] * x.[38] // C + bc | ligation: C + bc <-> Cbc
        |]
        |> Array.sum


    // 147 - CcA
    let d147 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[147] // CcA | ligation: C + cA <-> CcA
            1.0 * x.[5] * x.[39] // C + cA | ligation: C + cA <-> CcA
        |]
        |> Array.sum


    // 148 - CcB
    let d148 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[148] // CcB | ligation: C + cB <-> CcB
            1.0 * x.[5] * x.[40] // C + cB | ligation: C + cB <-> CcB
        |]
        |> Array.sum


    // 149 - CcC
    let d149 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[149] // CcC | ligation: C + cC <-> CcC
            1.0 * x.[5] * x.[41] // C + cC | ligation: C + cC <-> CcC
        |]
        |> Array.sum


    // 150 - Cca
    let d150 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[150] // Cca | ligation: C + ca <-> Cca
            1.0 * x.[5] * x.[42] // C + ca | ligation: C + ca <-> Cca
        |]
        |> Array.sum


    // 151 - Ccb
    let d151 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[151] // Ccb | ligation: C + cb <-> Ccb
            1.0 * x.[5] * x.[43] // C + cb | ligation: C + cb <-> Ccb
        |]
        |> Array.sum


    // 152 - Ccc
    let d152 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[152] // Ccc | ligation: C + cc <-> Ccc
            1.0 * x.[5] * x.[44] // C + cc | ligation: C + cc <-> Ccc
        |]
        |> Array.sum


    // 153 - aAA
    let d153 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[153] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[6] * x.[9] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 154 - aAB
    let d154 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[154] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[6] * x.[10] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 155 - aAC
    let d155 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[155] // aAC | ligation: a + AC <-> aAC
            1.0 * x.[6] * x.[11] // a + AC | ligation: a + AC <-> aAC
        |]
        |> Array.sum


    // 156 - aAa
    let d156 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[156] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[6] * x.[12] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 157 - aAb
    let d157 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[157] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[6] * x.[13] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 158 - aAc
    let d158 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[158] // aAc | ligation: a + Ac <-> aAc
            1.0 * x.[6] * x.[14] // a + Ac | ligation: a + Ac <-> aAc
        |]
        |> Array.sum


    // 159 - aBA
    let d159 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[159] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[6] * x.[15] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 160 - aBB
    let d160 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[160] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[6] * x.[16] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 161 - aBC
    let d161 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[161] // aBC | ligation: a + BC <-> aBC
            1.0 * x.[6] * x.[17] // a + BC | ligation: a + BC <-> aBC
        |]
        |> Array.sum


    // 162 - aBa
    let d162 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[162] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[6] * x.[18] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 163 - aBb
    let d163 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[163] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[6] * x.[19] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 164 - aBc
    let d164 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[164] // aBc | ligation: a + Bc <-> aBc
            1.0 * x.[6] * x.[20] // a + Bc | ligation: a + Bc <-> aBc
        |]
        |> Array.sum


    // 165 - aCA
    let d165 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[165] // aCA | ligation: a + CA <-> aCA
            1.0 * x.[6] * x.[21] // a + CA | ligation: a + CA <-> aCA
        |]
        |> Array.sum


    // 166 - aCB
    let d166 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[166] // aCB | ligation: a + CB <-> aCB
            1.0 * x.[6] * x.[22] // a + CB | ligation: a + CB <-> aCB
        |]
        |> Array.sum


    // 167 - aCC
    let d167 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[167] // aCC | ligation: a + CC <-> aCC
            1.0 * x.[6] * x.[23] // a + CC | ligation: a + CC <-> aCC
        |]
        |> Array.sum


    // 168 - aCa
    let d168 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[168] // aCa | ligation: a + Ca <-> aCa
            1.0 * x.[6] * x.[24] // a + Ca | ligation: a + Ca <-> aCa
        |]
        |> Array.sum


    // 169 - aCb
    let d169 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[169] // aCb | ligation: a + Cb <-> aCb
            1.0 * x.[6] * x.[25] // a + Cb | ligation: a + Cb <-> aCb
        |]
        |> Array.sum


    // 170 - aCc
    let d170 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[170] // aCc | ligation: a + Cc <-> aCc
            1.0 * x.[6] * x.[26] // a + Cc | ligation: a + Cc <-> aCc
        |]
        |> Array.sum


    // 171 - aaA
    let d171 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[171] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[6] * x.[27] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 172 - aaB
    let d172 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[172] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[6] * x.[28] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 173 - aaC
    let d173 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[173] // aaC | ligation: a + aC <-> aaC
            1.0 * x.[6] * x.[29] // a + aC | ligation: a + aC <-> aaC
        |]
        |> Array.sum


    // 174 - aaa
    let d174 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[174] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[6] * x.[30] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 175 - aab
    let d175 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[175] // aab | ligation: a + ab <-> aab
            1.0 * x.[6] * x.[31] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 176 - aac
    let d176 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[176] // aac | ligation: a + ac <-> aac
            1.0 * x.[6] * x.[32] // a + ac | ligation: a + ac <-> aac
        |]
        |> Array.sum


    // 177 - abA
    let d177 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[177] // abA | ligation: a + bA <-> abA
            1.0 * x.[6] * x.[33] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 178 - abB
    let d178 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[178] // abB | ligation: a + bB <-> abB
            1.0 * x.[6] * x.[34] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 179 - abC
    let d179 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[179] // abC | ligation: a + bC <-> abC
            1.0 * x.[6] * x.[35] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 180 - aba
    let d180 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[180] // aba | ligation: a + ba <-> aba
            1.0 * x.[6] * x.[36] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 181 - abb
    let d181 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[181] // abb | ligation: a + bb <-> abb
            1.0 * x.[6] * x.[37] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 182 - abc
    let d182 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[182] // abc | ligation: a + bc <-> abc
            1.0 * x.[6] * x.[38] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 183 - acA
    let d183 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[183] // acA | ligation: a + cA <-> acA
            1.0 * x.[6] * x.[39] // a + cA | ligation: a + cA <-> acA
        |]
        |> Array.sum


    // 184 - acB
    let d184 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[184] // acB | ligation: a + cB <-> acB
            1.0 * x.[6] * x.[40] // a + cB | ligation: a + cB <-> acB
        |]
        |> Array.sum


    // 185 - acC
    let d185 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[185] // acC | ligation: a + cC <-> acC
            1.0 * x.[6] * x.[41] // a + cC | ligation: a + cC <-> acC
        |]
        |> Array.sum


    // 186 - aca
    let d186 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[186] // aca | ligation: a + ca <-> aca
            1.0 * x.[6] * x.[42] // a + ca | ligation: a + ca <-> aca
        |]
        |> Array.sum


    // 187 - acb
    let d187 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[187] // acb | ligation: a + cb <-> acb
            1.0 * x.[6] * x.[43] // a + cb | ligation: a + cb <-> acb
        |]
        |> Array.sum


    // 188 - acc
    let d188 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[188] // acc | ligation: a + cc <-> acc
            1.0 * x.[6] * x.[44] // a + cc | ligation: a + cc <-> acc
        |]
        |> Array.sum


    // 189 - bAA
    let d189 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[189] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[7] * x.[9] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 190 - bAB
    let d190 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[190] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[7] * x.[10] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 191 - bAC
    let d191 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[191] // bAC | ligation: b + AC <-> bAC
            1.0 * x.[7] * x.[11] // b + AC | ligation: b + AC <-> bAC
        |]
        |> Array.sum


    // 192 - bAa
    let d192 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[192] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[7] * x.[12] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 193 - bAb
    let d193 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[193] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[7] * x.[13] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 194 - bAc
    let d194 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[194] // bAc | ligation: b + Ac <-> bAc
            1.0 * x.[7] * x.[14] // b + Ac | ligation: b + Ac <-> bAc
        |]
        |> Array.sum


    // 195 - bBA
    let d195 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[195] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[7] * x.[15] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 196 - bBB
    let d196 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[196] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[7] * x.[16] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 197 - bBC
    let d197 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[197] // bBC | ligation: b + BC <-> bBC
            1.0 * x.[7] * x.[17] // b + BC | ligation: b + BC <-> bBC
        |]
        |> Array.sum


    // 198 - bBa
    let d198 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[198] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[7] * x.[18] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 199 - bBb
    let d199 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[199] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[7] * x.[19] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 200 - bBc
    let d200 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[200] // bBc | ligation: b + Bc <-> bBc
            1.0 * x.[7] * x.[20] // b + Bc | ligation: b + Bc <-> bBc
        |]
        |> Array.sum


    // 201 - bCA
    let d201 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[201] // bCA | ligation: b + CA <-> bCA
            1.0 * x.[7] * x.[21] // b + CA | ligation: b + CA <-> bCA
        |]
        |> Array.sum


    // 202 - bCB
    let d202 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[202] // bCB | ligation: b + CB <-> bCB
            1.0 * x.[7] * x.[22] // b + CB | ligation: b + CB <-> bCB
        |]
        |> Array.sum


    // 203 - bCC
    let d203 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[203] // bCC | ligation: b + CC <-> bCC
            1.0 * x.[7] * x.[23] // b + CC | ligation: b + CC <-> bCC
        |]
        |> Array.sum


    // 204 - bCa
    let d204 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[204] // bCa | ligation: b + Ca <-> bCa
            1.0 * x.[7] * x.[24] // b + Ca | ligation: b + Ca <-> bCa
        |]
        |> Array.sum


    // 205 - bCb
    let d205 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[205] // bCb | ligation: b + Cb <-> bCb
            1.0 * x.[7] * x.[25] // b + Cb | ligation: b + Cb <-> bCb
        |]
        |> Array.sum


    // 206 - bCc
    let d206 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[206] // bCc | ligation: b + Cc <-> bCc
            1.0 * x.[7] * x.[26] // b + Cc | ligation: b + Cc <-> bCc
        |]
        |> Array.sum


    // 207 - baA
    let d207 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[207] // baA | ligation: b + aA <-> baA
            1.0 * x.[7] * x.[27] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 208 - baB
    let d208 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[208] // baB | ligation: b + aB <-> baB
            1.0 * x.[7] * x.[28] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 209 - baC
    let d209 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[209] // baC | ligation: b + aC <-> baC
            1.0 * x.[7] * x.[29] // b + aC | ligation: b + aC <-> baC
        |]
        |> Array.sum


    // 210 - baa
    let d210 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[210] // baa | ligation: b + aa <-> baa
            1.0 * x.[7] * x.[30] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 211 - bab
    let d211 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[211] // bab | ligation: b + ab <-> bab
            1.0 * x.[7] * x.[31] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 212 - bac
    let d212 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[212] // bac | ligation: b + ac <-> bac
            1.0 * x.[7] * x.[32] // b + ac | ligation: b + ac <-> bac
        |]
        |> Array.sum


    // 213 - bbA
    let d213 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[213] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[7] * x.[33] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 214 - bbB
    let d214 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[214] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[7] * x.[34] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 215 - bbC
    let d215 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[215] // bbC | ligation: b + bC <-> bbC
            1.0 * x.[7] * x.[35] // b + bC | ligation: b + bC <-> bbC
        |]
        |> Array.sum


    // 216 - bba
    let d216 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[216] // bba | ligation: b + ba <-> bba
            1.0 * x.[7] * x.[36] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 217 - bbb
    let d217 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[217] // bbb | ligation: b + bb <-> bbb
            1.0 * x.[7] * x.[37] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum


    // 218 - bbc
    let d218 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[218] // bbc | ligation: b + bc <-> bbc
            1.0 * x.[7] * x.[38] // b + bc | ligation: b + bc <-> bbc
        |]
        |> Array.sum


    // 219 - bcA
    let d219 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[219] // bcA | ligation: b + cA <-> bcA
            1.0 * x.[7] * x.[39] // b + cA | ligation: b + cA <-> bcA
        |]
        |> Array.sum


    // 220 - bcB
    let d220 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[220] // bcB | ligation: b + cB <-> bcB
            1.0 * x.[7] * x.[40] // b + cB | ligation: b + cB <-> bcB
        |]
        |> Array.sum


    // 221 - bcC
    let d221 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[221] // bcC | ligation: b + cC <-> bcC
            1.0 * x.[7] * x.[41] // b + cC | ligation: b + cC <-> bcC
        |]
        |> Array.sum


    // 222 - bca
    let d222 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[222] // bca | ligation: b + ca <-> bca
            1.0 * x.[7] * x.[42] // b + ca | ligation: b + ca <-> bca
        |]
        |> Array.sum


    // 223 - bcb
    let d223 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[223] // bcb | ligation: b + cb <-> bcb
            1.0 * x.[7] * x.[43] // b + cb | ligation: b + cb <-> bcb
        |]
        |> Array.sum


    // 224 - bcc
    let d224 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[224] // bcc | ligation: b + cc <-> bcc
            1.0 * x.[7] * x.[44] // b + cc | ligation: b + cc <-> bcc
        |]
        |> Array.sum


    // 225 - cAA
    let d225 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[225] // cAA | ligation: c + AA <-> cAA
            1.0 * x.[8] * x.[9] // c + AA | ligation: c + AA <-> cAA
        |]
        |> Array.sum


    // 226 - cAB
    let d226 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[226] // cAB | ligation: c + AB <-> cAB
            1.0 * x.[8] * x.[10] // c + AB | ligation: c + AB <-> cAB
        |]
        |> Array.sum


    // 227 - cAC
    let d227 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[227] // cAC | ligation: c + AC <-> cAC
            1.0 * x.[8] * x.[11] // c + AC | ligation: c + AC <-> cAC
        |]
        |> Array.sum


    // 228 - cAa
    let d228 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[228] // cAa | ligation: c + Aa <-> cAa
            1.0 * x.[8] * x.[12] // c + Aa | ligation: c + Aa <-> cAa
        |]
        |> Array.sum


    // 229 - cAb
    let d229 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[229] // cAb | ligation: c + Ab <-> cAb
            1.0 * x.[8] * x.[13] // c + Ab | ligation: c + Ab <-> cAb
        |]
        |> Array.sum


    // 230 - cAc
    let d230 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[230] // cAc | ligation: c + Ac <-> cAc
            1.0 * x.[8] * x.[14] // c + Ac | ligation: c + Ac <-> cAc
        |]
        |> Array.sum


    // 231 - cBA
    let d231 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[231] // cBA | ligation: c + BA <-> cBA
            1.0 * x.[8] * x.[15] // c + BA | ligation: c + BA <-> cBA
        |]
        |> Array.sum


    // 232 - cBB
    let d232 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[232] // cBB | ligation: c + BB <-> cBB
            1.0 * x.[8] * x.[16] // c + BB | ligation: c + BB <-> cBB
        |]
        |> Array.sum


    // 233 - cBC
    let d233 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[233] // cBC | ligation: c + BC <-> cBC
            1.0 * x.[8] * x.[17] // c + BC | ligation: c + BC <-> cBC
        |]
        |> Array.sum


    // 234 - cBa
    let d234 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[234] // cBa | ligation: c + Ba <-> cBa
            1.0 * x.[8] * x.[18] // c + Ba | ligation: c + Ba <-> cBa
        |]
        |> Array.sum


    // 235 - cBb
    let d235 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[235] // cBb | ligation: c + Bb <-> cBb
            1.0 * x.[8] * x.[19] // c + Bb | ligation: c + Bb <-> cBb
        |]
        |> Array.sum


    // 236 - cBc
    let d236 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[236] // cBc | ligation: c + Bc <-> cBc
            1.0 * x.[8] * x.[20] // c + Bc | ligation: c + Bc <-> cBc
        |]
        |> Array.sum


    // 237 - cCA
    let d237 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[237] // cCA | ligation: c + CA <-> cCA
            1.0 * x.[8] * x.[21] // c + CA | ligation: c + CA <-> cCA
        |]
        |> Array.sum


    // 238 - cCB
    let d238 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[238] // cCB | ligation: c + CB <-> cCB
            1.0 * x.[8] * x.[22] // c + CB | ligation: c + CB <-> cCB
        |]
        |> Array.sum


    // 239 - cCC
    let d239 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[239] // cCC | ligation: c + CC <-> cCC
            1.0 * x.[8] * x.[23] // c + CC | ligation: c + CC <-> cCC
        |]
        |> Array.sum


    // 240 - cCa
    let d240 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[240] // cCa | ligation: c + Ca <-> cCa
            1.0 * x.[8] * x.[24] // c + Ca | ligation: c + Ca <-> cCa
        |]
        |> Array.sum


    // 241 - cCb
    let d241 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[241] // cCb | ligation: c + Cb <-> cCb
            1.0 * x.[8] * x.[25] // c + Cb | ligation: c + Cb <-> cCb
        |]
        |> Array.sum


    // 242 - cCc
    let d242 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[242] // cCc | ligation: c + Cc <-> cCc
            1.0 * x.[8] * x.[26] // c + Cc | ligation: c + Cc <-> cCc
        |]
        |> Array.sum


    // 243 - caA
    let d243 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[243] // caA | ligation: c + aA <-> caA
            1.0 * x.[8] * x.[27] // c + aA | ligation: c + aA <-> caA
        |]
        |> Array.sum


    // 244 - caB
    let d244 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[244] // caB | ligation: c + aB <-> caB
            1.0 * x.[8] * x.[28] // c + aB | ligation: c + aB <-> caB
        |]
        |> Array.sum


    // 245 - caC
    let d245 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[245] // caC | ligation: c + aC <-> caC
            1.0 * x.[8] * x.[29] // c + aC | ligation: c + aC <-> caC
        |]
        |> Array.sum


    // 246 - caa
    let d246 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[246] // caa | ligation: c + aa <-> caa
            1.0 * x.[8] * x.[30] // c + aa | ligation: c + aa <-> caa
        |]
        |> Array.sum


    // 247 - cab
    let d247 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[247] // cab | ligation: c + ab <-> cab
            1.0 * x.[8] * x.[31] // c + ab | ligation: c + ab <-> cab
        |]
        |> Array.sum


    // 248 - cac
    let d248 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[248] // cac | ligation: c + ac <-> cac
            1.0 * x.[8] * x.[32] // c + ac | ligation: c + ac <-> cac
        |]
        |> Array.sum


    // 249 - cbA
    let d249 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[249] // cbA | ligation: c + bA <-> cbA
            1.0 * x.[8] * x.[33] // c + bA | ligation: c + bA <-> cbA
        |]
        |> Array.sum


    // 250 - cbB
    let d250 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[250] // cbB | ligation: c + bB <-> cbB
            1.0 * x.[8] * x.[34] // c + bB | ligation: c + bB <-> cbB
        |]
        |> Array.sum


    // 251 - cbC
    let d251 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[251] // cbC | ligation: c + bC <-> cbC
            1.0 * x.[8] * x.[35] // c + bC | ligation: c + bC <-> cbC
        |]
        |> Array.sum


    // 252 - cba
    let d252 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[252] // cba | ligation: c + ba <-> cba
            1.0 * x.[8] * x.[36] // c + ba | ligation: c + ba <-> cba
        |]
        |> Array.sum


    // 253 - cbb
    let d253 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[253] // cbb | ligation: c + bb <-> cbb
            1.0 * x.[8] * x.[37] // c + bb | ligation: c + bb <-> cbb
        |]
        |> Array.sum


    // 254 - cbc
    let d254 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[254] // cbc | ligation: c + bc <-> cbc
            1.0 * x.[8] * x.[38] // c + bc | ligation: c + bc <-> cbc
        |]
        |> Array.sum


    // 255 - ccA
    let d255 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[255] // ccA | ligation: c + cA <-> ccA
            1.0 * x.[8] * x.[39] // c + cA | ligation: c + cA <-> ccA
        |]
        |> Array.sum


    // 256 - ccB
    let d256 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[256] // ccB | ligation: c + cB <-> ccB
            1.0 * x.[8] * x.[40] // c + cB | ligation: c + cB <-> ccB
        |]
        |> Array.sum


    // 257 - ccC
    let d257 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[257] // ccC | ligation: c + cC <-> ccC
            1.0 * x.[8] * x.[41] // c + cC | ligation: c + cC <-> ccC
        |]
        |> Array.sum


    // 258 - cca
    let d258 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[258] // cca | ligation: c + ca <-> cca
            1.0 * x.[8] * x.[42] // c + ca | ligation: c + ca <-> cca
        |]
        |> Array.sum


    // 259 - ccb
    let d259 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[259] // ccb | ligation: c + cb <-> ccb
            1.0 * x.[8] * x.[43] // c + cb | ligation: c + cb <-> ccb
        |]
        |> Array.sum


    // 260 - ccc
    let d260 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[260] // ccc | ligation: c + cc <-> ccc
            1.0 * x.[8] * x.[44] // c + cc | ligation: c + cc <-> ccc
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
                1.0 * x.[5] // C
                1.0 * x.[6] // a
                1.0 * x.[7] // b
                1.0 * x.[8] // c
                2.0 * x.[9] // AA
                2.0 * x.[10] // AB
                2.0 * x.[11] // AC
                2.0 * x.[12] // Aa
                2.0 * x.[13] // Ab
                2.0 * x.[14] // Ac
                2.0 * x.[15] // BA
                2.0 * x.[16] // BB
                2.0 * x.[17] // BC
                2.0 * x.[18] // Ba
                2.0 * x.[19] // Bb
                2.0 * x.[20] // Bc
                2.0 * x.[21] // CA
                2.0 * x.[22] // CB
                2.0 * x.[23] // CC
                2.0 * x.[24] // Ca
                2.0 * x.[25] // Cb
                2.0 * x.[26] // Cc
                2.0 * x.[27] // aA
                2.0 * x.[28] // aB
                2.0 * x.[29] // aC
                2.0 * x.[30] // aa
                2.0 * x.[31] // ab
                2.0 * x.[32] // ac
                2.0 * x.[33] // bA
                2.0 * x.[34] // bB
                2.0 * x.[35] // bC
                2.0 * x.[36] // ba
                2.0 * x.[37] // bb
                2.0 * x.[38] // bc
                2.0 * x.[39] // cA
                2.0 * x.[40] // cB
                2.0 * x.[41] // cC
                2.0 * x.[42] // ca
                2.0 * x.[43] // cb
                2.0 * x.[44] // cc
                3.0 * x.[45] // AAA
                3.0 * x.[46] // AAB
                3.0 * x.[47] // AAC
                3.0 * x.[48] // AAa
                3.0 * x.[49] // AAb
                3.0 * x.[50] // AAc
                3.0 * x.[51] // ABA
                3.0 * x.[52] // ABB
                3.0 * x.[53] // ABC
                3.0 * x.[54] // ABa
                3.0 * x.[55] // ABb
                3.0 * x.[56] // ABc
                3.0 * x.[57] // ACA
                3.0 * x.[58] // ACB
                3.0 * x.[59] // ACC
                3.0 * x.[60] // ACa
                3.0 * x.[61] // ACb
                3.0 * x.[62] // ACc
                3.0 * x.[63] // AaA
                3.0 * x.[64] // AaB
                3.0 * x.[65] // AaC
                3.0 * x.[66] // Aaa
                3.0 * x.[67] // Aab
                3.0 * x.[68] // Aac
                3.0 * x.[69] // AbA
                3.0 * x.[70] // AbB
                3.0 * x.[71] // AbC
                3.0 * x.[72] // Aba
                3.0 * x.[73] // Abb
                3.0 * x.[74] // Abc
                3.0 * x.[75] // AcA
                3.0 * x.[76] // AcB
                3.0 * x.[77] // AcC
                3.0 * x.[78] // Aca
                3.0 * x.[79] // Acb
                3.0 * x.[80] // Acc
                3.0 * x.[81] // BAA
                3.0 * x.[82] // BAB
                3.0 * x.[83] // BAC
                3.0 * x.[84] // BAa
                3.0 * x.[85] // BAb
                3.0 * x.[86] // BAc
                3.0 * x.[87] // BBA
                3.0 * x.[88] // BBB
                3.0 * x.[89] // BBC
                3.0 * x.[90] // BBa
                3.0 * x.[91] // BBb
                3.0 * x.[92] // BBc
                3.0 * x.[93] // BCA
                3.0 * x.[94] // BCB
                3.0 * x.[95] // BCC
                3.0 * x.[96] // BCa
                3.0 * x.[97] // BCb
                3.0 * x.[98] // BCc
                3.0 * x.[99] // BaA
                3.0 * x.[100] // BaB
                3.0 * x.[101] // BaC
                3.0 * x.[102] // Baa
                3.0 * x.[103] // Bab
                3.0 * x.[104] // Bac
                3.0 * x.[105] // BbA
                3.0 * x.[106] // BbB
                3.0 * x.[107] // BbC
                3.0 * x.[108] // Bba
                3.0 * x.[109] // Bbb
                3.0 * x.[110] // Bbc
                3.0 * x.[111] // BcA
                3.0 * x.[112] // BcB
                3.0 * x.[113] // BcC
                3.0 * x.[114] // Bca
                3.0 * x.[115] // Bcb
                3.0 * x.[116] // Bcc
                3.0 * x.[117] // CAA
                3.0 * x.[118] // CAB
                3.0 * x.[119] // CAC
                3.0 * x.[120] // CAa
                3.0 * x.[121] // CAb
                3.0 * x.[122] // CAc
                3.0 * x.[123] // CBA
                3.0 * x.[124] // CBB
                3.0 * x.[125] // CBC
                3.0 * x.[126] // CBa
                3.0 * x.[127] // CBb
                3.0 * x.[128] // CBc
                3.0 * x.[129] // CCA
                3.0 * x.[130] // CCB
                3.0 * x.[131] // CCC
                3.0 * x.[132] // CCa
                3.0 * x.[133] // CCb
                3.0 * x.[134] // CCc
                3.0 * x.[135] // CaA
                3.0 * x.[136] // CaB
                3.0 * x.[137] // CaC
                3.0 * x.[138] // Caa
                3.0 * x.[139] // Cab
                3.0 * x.[140] // Cac
                3.0 * x.[141] // CbA
                3.0 * x.[142] // CbB
                3.0 * x.[143] // CbC
                3.0 * x.[144] // Cba
                3.0 * x.[145] // Cbb
                3.0 * x.[146] // Cbc
                3.0 * x.[147] // CcA
                3.0 * x.[148] // CcB
                3.0 * x.[149] // CcC
                3.0 * x.[150] // Cca
                3.0 * x.[151] // Ccb
                3.0 * x.[152] // Ccc
                3.0 * x.[153] // aAA
                3.0 * x.[154] // aAB
                3.0 * x.[155] // aAC
                3.0 * x.[156] // aAa
                3.0 * x.[157] // aAb
                3.0 * x.[158] // aAc
                3.0 * x.[159] // aBA
                3.0 * x.[160] // aBB
                3.0 * x.[161] // aBC
                3.0 * x.[162] // aBa
                3.0 * x.[163] // aBb
                3.0 * x.[164] // aBc
                3.0 * x.[165] // aCA
                3.0 * x.[166] // aCB
                3.0 * x.[167] // aCC
                3.0 * x.[168] // aCa
                3.0 * x.[169] // aCb
                3.0 * x.[170] // aCc
                3.0 * x.[171] // aaA
                3.0 * x.[172] // aaB
                3.0 * x.[173] // aaC
                3.0 * x.[174] // aaa
                3.0 * x.[175] // aab
                3.0 * x.[176] // aac
                3.0 * x.[177] // abA
                3.0 * x.[178] // abB
                3.0 * x.[179] // abC
                3.0 * x.[180] // aba
                3.0 * x.[181] // abb
                3.0 * x.[182] // abc
                3.0 * x.[183] // acA
                3.0 * x.[184] // acB
                3.0 * x.[185] // acC
                3.0 * x.[186] // aca
                3.0 * x.[187] // acb
                3.0 * x.[188] // acc
                3.0 * x.[189] // bAA
                3.0 * x.[190] // bAB
                3.0 * x.[191] // bAC
                3.0 * x.[192] // bAa
                3.0 * x.[193] // bAb
                3.0 * x.[194] // bAc
                3.0 * x.[195] // bBA
                3.0 * x.[196] // bBB
                3.0 * x.[197] // bBC
                3.0 * x.[198] // bBa
                3.0 * x.[199] // bBb
                3.0 * x.[200] // bBc
                3.0 * x.[201] // bCA
                3.0 * x.[202] // bCB
                3.0 * x.[203] // bCC
                3.0 * x.[204] // bCa
                3.0 * x.[205] // bCb
                3.0 * x.[206] // bCc
                3.0 * x.[207] // baA
                3.0 * x.[208] // baB
                3.0 * x.[209] // baC
                3.0 * x.[210] // baa
                3.0 * x.[211] // bab
                3.0 * x.[212] // bac
                3.0 * x.[213] // bbA
                3.0 * x.[214] // bbB
                3.0 * x.[215] // bbC
                3.0 * x.[216] // bba
                3.0 * x.[217] // bbb
                3.0 * x.[218] // bbc
                3.0 * x.[219] // bcA
                3.0 * x.[220] // bcB
                3.0 * x.[221] // bcC
                3.0 * x.[222] // bca
                3.0 * x.[223] // bcb
                3.0 * x.[224] // bcc
                3.0 * x.[225] // cAA
                3.0 * x.[226] // cAB
                3.0 * x.[227] // cAC
                3.0 * x.[228] // cAa
                3.0 * x.[229] // cAb
                3.0 * x.[230] // cAc
                3.0 * x.[231] // cBA
                3.0 * x.[232] // cBB
                3.0 * x.[233] // cBC
                3.0 * x.[234] // cBa
                3.0 * x.[235] // cBb
                3.0 * x.[236] // cBc
                3.0 * x.[237] // cCA
                3.0 * x.[238] // cCB
                3.0 * x.[239] // cCC
                3.0 * x.[240] // cCa
                3.0 * x.[241] // cCb
                3.0 * x.[242] // cCc
                3.0 * x.[243] // caA
                3.0 * x.[244] // caB
                3.0 * x.[245] // caC
                3.0 * x.[246] // caa
                3.0 * x.[247] // cab
                3.0 * x.[248] // cac
                3.0 * x.[249] // cbA
                3.0 * x.[250] // cbB
                3.0 * x.[251] // cbC
                3.0 * x.[252] // cba
                3.0 * x.[253] // cbb
                3.0 * x.[254] // cbc
                3.0 * x.[255] // ccA
                3.0 * x.[256] // ccB
                3.0 * x.[257] // ccC
                3.0 * x.[258] // cca
                3.0 * x.[259] // ccb
                3.0 * x.[260] // ccc
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[3] * x.[3] // A
                1.0 * x.[4] * x.[4] // B
                1.0 * x.[5] * x.[5] // C
                1.0 * x.[6] * x.[6] // a
                1.0 * x.[7] * x.[7] // b
                1.0 * x.[8] * x.[8] // c
                2.0 * x.[9] * x.[9] // AA
                2.0 * x.[10] * x.[10] // AB
                2.0 * x.[11] * x.[11] // AC
                2.0 * x.[12] * x.[12] // Aa
                2.0 * x.[13] * x.[13] // Ab
                2.0 * x.[14] * x.[14] // Ac
                2.0 * x.[15] * x.[15] // BA
                2.0 * x.[16] * x.[16] // BB
                2.0 * x.[17] * x.[17] // BC
                2.0 * x.[18] * x.[18] // Ba
                2.0 * x.[19] * x.[19] // Bb
                2.0 * x.[20] * x.[20] // Bc
                2.0 * x.[21] * x.[21] // CA
                2.0 * x.[22] * x.[22] // CB
                2.0 * x.[23] * x.[23] // CC
                2.0 * x.[24] * x.[24] // Ca
                2.0 * x.[25] * x.[25] // Cb
                2.0 * x.[26] * x.[26] // Cc
                2.0 * x.[27] * x.[27] // aA
                2.0 * x.[28] * x.[28] // aB
                2.0 * x.[29] * x.[29] // aC
                2.0 * x.[30] * x.[30] // aa
                2.0 * x.[31] * x.[31] // ab
                2.0 * x.[32] * x.[32] // ac
                2.0 * x.[33] * x.[33] // bA
                2.0 * x.[34] * x.[34] // bB
                2.0 * x.[35] * x.[35] // bC
                2.0 * x.[36] * x.[36] // ba
                2.0 * x.[37] * x.[37] // bb
                2.0 * x.[38] * x.[38] // bc
                2.0 * x.[39] * x.[39] // cA
                2.0 * x.[40] * x.[40] // cB
                2.0 * x.[41] * x.[41] // cC
                2.0 * x.[42] * x.[42] // ca
                2.0 * x.[43] * x.[43] // cb
                2.0 * x.[44] * x.[44] // cc
                3.0 * x.[45] * x.[45] // AAA
                3.0 * x.[46] * x.[46] // AAB
                3.0 * x.[47] * x.[47] // AAC
                3.0 * x.[48] * x.[48] // AAa
                3.0 * x.[49] * x.[49] // AAb
                3.0 * x.[50] * x.[50] // AAc
                3.0 * x.[51] * x.[51] // ABA
                3.0 * x.[52] * x.[52] // ABB
                3.0 * x.[53] * x.[53] // ABC
                3.0 * x.[54] * x.[54] // ABa
                3.0 * x.[55] * x.[55] // ABb
                3.0 * x.[56] * x.[56] // ABc
                3.0 * x.[57] * x.[57] // ACA
                3.0 * x.[58] * x.[58] // ACB
                3.0 * x.[59] * x.[59] // ACC
                3.0 * x.[60] * x.[60] // ACa
                3.0 * x.[61] * x.[61] // ACb
                3.0 * x.[62] * x.[62] // ACc
                3.0 * x.[63] * x.[63] // AaA
                3.0 * x.[64] * x.[64] // AaB
                3.0 * x.[65] * x.[65] // AaC
                3.0 * x.[66] * x.[66] // Aaa
                3.0 * x.[67] * x.[67] // Aab
                3.0 * x.[68] * x.[68] // Aac
                3.0 * x.[69] * x.[69] // AbA
                3.0 * x.[70] * x.[70] // AbB
                3.0 * x.[71] * x.[71] // AbC
                3.0 * x.[72] * x.[72] // Aba
                3.0 * x.[73] * x.[73] // Abb
                3.0 * x.[74] * x.[74] // Abc
                3.0 * x.[75] * x.[75] // AcA
                3.0 * x.[76] * x.[76] // AcB
                3.0 * x.[77] * x.[77] // AcC
                3.0 * x.[78] * x.[78] // Aca
                3.0 * x.[79] * x.[79] // Acb
                3.0 * x.[80] * x.[80] // Acc
                3.0 * x.[81] * x.[81] // BAA
                3.0 * x.[82] * x.[82] // BAB
                3.0 * x.[83] * x.[83] // BAC
                3.0 * x.[84] * x.[84] // BAa
                3.0 * x.[85] * x.[85] // BAb
                3.0 * x.[86] * x.[86] // BAc
                3.0 * x.[87] * x.[87] // BBA
                3.0 * x.[88] * x.[88] // BBB
                3.0 * x.[89] * x.[89] // BBC
                3.0 * x.[90] * x.[90] // BBa
                3.0 * x.[91] * x.[91] // BBb
                3.0 * x.[92] * x.[92] // BBc
                3.0 * x.[93] * x.[93] // BCA
                3.0 * x.[94] * x.[94] // BCB
                3.0 * x.[95] * x.[95] // BCC
                3.0 * x.[96] * x.[96] // BCa
                3.0 * x.[97] * x.[97] // BCb
                3.0 * x.[98] * x.[98] // BCc
                3.0 * x.[99] * x.[99] // BaA
                3.0 * x.[100] * x.[100] // BaB
                3.0 * x.[101] * x.[101] // BaC
                3.0 * x.[102] * x.[102] // Baa
                3.0 * x.[103] * x.[103] // Bab
                3.0 * x.[104] * x.[104] // Bac
                3.0 * x.[105] * x.[105] // BbA
                3.0 * x.[106] * x.[106] // BbB
                3.0 * x.[107] * x.[107] // BbC
                3.0 * x.[108] * x.[108] // Bba
                3.0 * x.[109] * x.[109] // Bbb
                3.0 * x.[110] * x.[110] // Bbc
                3.0 * x.[111] * x.[111] // BcA
                3.0 * x.[112] * x.[112] // BcB
                3.0 * x.[113] * x.[113] // BcC
                3.0 * x.[114] * x.[114] // Bca
                3.0 * x.[115] * x.[115] // Bcb
                3.0 * x.[116] * x.[116] // Bcc
                3.0 * x.[117] * x.[117] // CAA
                3.0 * x.[118] * x.[118] // CAB
                3.0 * x.[119] * x.[119] // CAC
                3.0 * x.[120] * x.[120] // CAa
                3.0 * x.[121] * x.[121] // CAb
                3.0 * x.[122] * x.[122] // CAc
                3.0 * x.[123] * x.[123] // CBA
                3.0 * x.[124] * x.[124] // CBB
                3.0 * x.[125] * x.[125] // CBC
                3.0 * x.[126] * x.[126] // CBa
                3.0 * x.[127] * x.[127] // CBb
                3.0 * x.[128] * x.[128] // CBc
                3.0 * x.[129] * x.[129] // CCA
                3.0 * x.[130] * x.[130] // CCB
                3.0 * x.[131] * x.[131] // CCC
                3.0 * x.[132] * x.[132] // CCa
                3.0 * x.[133] * x.[133] // CCb
                3.0 * x.[134] * x.[134] // CCc
                3.0 * x.[135] * x.[135] // CaA
                3.0 * x.[136] * x.[136] // CaB
                3.0 * x.[137] * x.[137] // CaC
                3.0 * x.[138] * x.[138] // Caa
                3.0 * x.[139] * x.[139] // Cab
                3.0 * x.[140] * x.[140] // Cac
                3.0 * x.[141] * x.[141] // CbA
                3.0 * x.[142] * x.[142] // CbB
                3.0 * x.[143] * x.[143] // CbC
                3.0 * x.[144] * x.[144] // Cba
                3.0 * x.[145] * x.[145] // Cbb
                3.0 * x.[146] * x.[146] // Cbc
                3.0 * x.[147] * x.[147] // CcA
                3.0 * x.[148] * x.[148] // CcB
                3.0 * x.[149] * x.[149] // CcC
                3.0 * x.[150] * x.[150] // Cca
                3.0 * x.[151] * x.[151] // Ccb
                3.0 * x.[152] * x.[152] // Ccc
                3.0 * x.[153] * x.[153] // aAA
                3.0 * x.[154] * x.[154] // aAB
                3.0 * x.[155] * x.[155] // aAC
                3.0 * x.[156] * x.[156] // aAa
                3.0 * x.[157] * x.[157] // aAb
                3.0 * x.[158] * x.[158] // aAc
                3.0 * x.[159] * x.[159] // aBA
                3.0 * x.[160] * x.[160] // aBB
                3.0 * x.[161] * x.[161] // aBC
                3.0 * x.[162] * x.[162] // aBa
                3.0 * x.[163] * x.[163] // aBb
                3.0 * x.[164] * x.[164] // aBc
                3.0 * x.[165] * x.[165] // aCA
                3.0 * x.[166] * x.[166] // aCB
                3.0 * x.[167] * x.[167] // aCC
                3.0 * x.[168] * x.[168] // aCa
                3.0 * x.[169] * x.[169] // aCb
                3.0 * x.[170] * x.[170] // aCc
                3.0 * x.[171] * x.[171] // aaA
                3.0 * x.[172] * x.[172] // aaB
                3.0 * x.[173] * x.[173] // aaC
                3.0 * x.[174] * x.[174] // aaa
                3.0 * x.[175] * x.[175] // aab
                3.0 * x.[176] * x.[176] // aac
                3.0 * x.[177] * x.[177] // abA
                3.0 * x.[178] * x.[178] // abB
                3.0 * x.[179] * x.[179] // abC
                3.0 * x.[180] * x.[180] // aba
                3.0 * x.[181] * x.[181] // abb
                3.0 * x.[182] * x.[182] // abc
                3.0 * x.[183] * x.[183] // acA
                3.0 * x.[184] * x.[184] // acB
                3.0 * x.[185] * x.[185] // acC
                3.0 * x.[186] * x.[186] // aca
                3.0 * x.[187] * x.[187] // acb
                3.0 * x.[188] * x.[188] // acc
                3.0 * x.[189] * x.[189] // bAA
                3.0 * x.[190] * x.[190] // bAB
                3.0 * x.[191] * x.[191] // bAC
                3.0 * x.[192] * x.[192] // bAa
                3.0 * x.[193] * x.[193] // bAb
                3.0 * x.[194] * x.[194] // bAc
                3.0 * x.[195] * x.[195] // bBA
                3.0 * x.[196] * x.[196] // bBB
                3.0 * x.[197] * x.[197] // bBC
                3.0 * x.[198] * x.[198] // bBa
                3.0 * x.[199] * x.[199] // bBb
                3.0 * x.[200] * x.[200] // bBc
                3.0 * x.[201] * x.[201] // bCA
                3.0 * x.[202] * x.[202] // bCB
                3.0 * x.[203] * x.[203] // bCC
                3.0 * x.[204] * x.[204] // bCa
                3.0 * x.[205] * x.[205] // bCb
                3.0 * x.[206] * x.[206] // bCc
                3.0 * x.[207] * x.[207] // baA
                3.0 * x.[208] * x.[208] // baB
                3.0 * x.[209] * x.[209] // baC
                3.0 * x.[210] * x.[210] // baa
                3.0 * x.[211] * x.[211] // bab
                3.0 * x.[212] * x.[212] // bac
                3.0 * x.[213] * x.[213] // bbA
                3.0 * x.[214] * x.[214] // bbB
                3.0 * x.[215] * x.[215] // bbC
                3.0 * x.[216] * x.[216] // bba
                3.0 * x.[217] * x.[217] // bbb
                3.0 * x.[218] * x.[218] // bbc
                3.0 * x.[219] * x.[219] // bcA
                3.0 * x.[220] * x.[220] // bcB
                3.0 * x.[221] * x.[221] // bcC
                3.0 * x.[222] * x.[222] // bca
                3.0 * x.[223] * x.[223] // bcb
                3.0 * x.[224] * x.[224] // bcc
                3.0 * x.[225] * x.[225] // cAA
                3.0 * x.[226] * x.[226] // cAB
                3.0 * x.[227] * x.[227] // cAC
                3.0 * x.[228] * x.[228] // cAa
                3.0 * x.[229] * x.[229] // cAb
                3.0 * x.[230] * x.[230] // cAc
                3.0 * x.[231] * x.[231] // cBA
                3.0 * x.[232] * x.[232] // cBB
                3.0 * x.[233] * x.[233] // cBC
                3.0 * x.[234] * x.[234] // cBa
                3.0 * x.[235] * x.[235] // cBb
                3.0 * x.[236] * x.[236] // cBc
                3.0 * x.[237] * x.[237] // cCA
                3.0 * x.[238] * x.[238] // cCB
                3.0 * x.[239] * x.[239] // cCC
                3.0 * x.[240] * x.[240] // cCa
                3.0 * x.[241] * x.[241] // cCb
                3.0 * x.[242] * x.[242] // cCc
                3.0 * x.[243] * x.[243] // caA
                3.0 * x.[244] * x.[244] // caB
                3.0 * x.[245] * x.[245] // caC
                3.0 * x.[246] * x.[246] // caa
                3.0 * x.[247] * x.[247] // cab
                3.0 * x.[248] * x.[248] // cac
                3.0 * x.[249] * x.[249] // cbA
                3.0 * x.[250] * x.[250] // cbB
                3.0 * x.[251] * x.[251] // cbC
                3.0 * x.[252] * x.[252] // cba
                3.0 * x.[253] * x.[253] // cbb
                3.0 * x.[254] * x.[254] // cbc
                3.0 * x.[255] * x.[255] // ccA
                3.0 * x.[256] * x.[256] // ccB
                3.0 * x.[257] * x.[257] // ccC
                3.0 * x.[258] * x.[258] // cca
                3.0 * x.[259] * x.[259] // ccb
                3.0 * x.[260] * x.[260] // ccc
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
            d87 x xSum xSumN xSumSquaredN
            d88 x xSum xSumN xSumSquaredN
            d89 x xSum xSumN xSumSquaredN
            d90 x xSum xSumN xSumSquaredN
            d91 x xSum xSumN xSumSquaredN
            d92 x xSum xSumN xSumSquaredN
            d93 x xSum xSumN xSumSquaredN
            d94 x xSum xSumN xSumSquaredN
            d95 x xSum xSumN xSumSquaredN
            d96 x xSum xSumN xSumSquaredN
            d97 x xSum xSumN xSumSquaredN
            d98 x xSum xSumN xSumSquaredN
            d99 x xSum xSumN xSumSquaredN
            d100 x xSum xSumN xSumSquaredN
            d101 x xSum xSumN xSumSquaredN
            d102 x xSum xSumN xSumSquaredN
            d103 x xSum xSumN xSumSquaredN
            d104 x xSum xSumN xSumSquaredN
            d105 x xSum xSumN xSumSquaredN
            d106 x xSum xSumN xSumSquaredN
            d107 x xSum xSumN xSumSquaredN
            d108 x xSum xSumN xSumSquaredN
            d109 x xSum xSumN xSumSquaredN
            d110 x xSum xSumN xSumSquaredN
            d111 x xSum xSumN xSumSquaredN
            d112 x xSum xSumN xSumSquaredN
            d113 x xSum xSumN xSumSquaredN
            d114 x xSum xSumN xSumSquaredN
            d115 x xSum xSumN xSumSquaredN
            d116 x xSum xSumN xSumSquaredN
            d117 x xSum xSumN xSumSquaredN
            d118 x xSum xSumN xSumSquaredN
            d119 x xSum xSumN xSumSquaredN
            d120 x xSum xSumN xSumSquaredN
            d121 x xSum xSumN xSumSquaredN
            d122 x xSum xSumN xSumSquaredN
            d123 x xSum xSumN xSumSquaredN
            d124 x xSum xSumN xSumSquaredN
            d125 x xSum xSumN xSumSquaredN
            d126 x xSum xSumN xSumSquaredN
            d127 x xSum xSumN xSumSquaredN
            d128 x xSum xSumN xSumSquaredN
            d129 x xSum xSumN xSumSquaredN
            d130 x xSum xSumN xSumSquaredN
            d131 x xSum xSumN xSumSquaredN
            d132 x xSum xSumN xSumSquaredN
            d133 x xSum xSumN xSumSquaredN
            d134 x xSum xSumN xSumSquaredN
            d135 x xSum xSumN xSumSquaredN
            d136 x xSum xSumN xSumSquaredN
            d137 x xSum xSumN xSumSquaredN
            d138 x xSum xSumN xSumSquaredN
            d139 x xSum xSumN xSumSquaredN
            d140 x xSum xSumN xSumSquaredN
            d141 x xSum xSumN xSumSquaredN
            d142 x xSum xSumN xSumSquaredN
            d143 x xSum xSumN xSumSquaredN
            d144 x xSum xSumN xSumSquaredN
            d145 x xSum xSumN xSumSquaredN
            d146 x xSum xSumN xSumSquaredN
            d147 x xSum xSumN xSumSquaredN
            d148 x xSum xSumN xSumSquaredN
            d149 x xSum xSumN xSumSquaredN
            d150 x xSum xSumN xSumSquaredN
            d151 x xSum xSumN xSumSquaredN
            d152 x xSum xSumN xSumSquaredN
            d153 x xSum xSumN xSumSquaredN
            d154 x xSum xSumN xSumSquaredN
            d155 x xSum xSumN xSumSquaredN
            d156 x xSum xSumN xSumSquaredN
            d157 x xSum xSumN xSumSquaredN
            d158 x xSum xSumN xSumSquaredN
            d159 x xSum xSumN xSumSquaredN
            d160 x xSum xSumN xSumSquaredN
            d161 x xSum xSumN xSumSquaredN
            d162 x xSum xSumN xSumSquaredN
            d163 x xSum xSumN xSumSquaredN
            d164 x xSum xSumN xSumSquaredN
            d165 x xSum xSumN xSumSquaredN
            d166 x xSum xSumN xSumSquaredN
            d167 x xSum xSumN xSumSquaredN
            d168 x xSum xSumN xSumSquaredN
            d169 x xSum xSumN xSumSquaredN
            d170 x xSum xSumN xSumSquaredN
            d171 x xSum xSumN xSumSquaredN
            d172 x xSum xSumN xSumSquaredN
            d173 x xSum xSumN xSumSquaredN
            d174 x xSum xSumN xSumSquaredN
            d175 x xSum xSumN xSumSquaredN
            d176 x xSum xSumN xSumSquaredN
            d177 x xSum xSumN xSumSquaredN
            d178 x xSum xSumN xSumSquaredN
            d179 x xSum xSumN xSumSquaredN
            d180 x xSum xSumN xSumSquaredN
            d181 x xSum xSumN xSumSquaredN
            d182 x xSum xSumN xSumSquaredN
            d183 x xSum xSumN xSumSquaredN
            d184 x xSum xSumN xSumSquaredN
            d185 x xSum xSumN xSumSquaredN
            d186 x xSum xSumN xSumSquaredN
            d187 x xSum xSumN xSumSquaredN
            d188 x xSum xSumN xSumSquaredN
            d189 x xSum xSumN xSumSquaredN
            d190 x xSum xSumN xSumSquaredN
            d191 x xSum xSumN xSumSquaredN
            d192 x xSum xSumN xSumSquaredN
            d193 x xSum xSumN xSumSquaredN
            d194 x xSum xSumN xSumSquaredN
            d195 x xSum xSumN xSumSquaredN
            d196 x xSum xSumN xSumSquaredN
            d197 x xSum xSumN xSumSquaredN
            d198 x xSum xSumN xSumSquaredN
            d199 x xSum xSumN xSumSquaredN
            d200 x xSum xSumN xSumSquaredN
            d201 x xSum xSumN xSumSquaredN
            d202 x xSum xSumN xSumSquaredN
            d203 x xSum xSumN xSumSquaredN
            d204 x xSum xSumN xSumSquaredN
            d205 x xSum xSumN xSumSquaredN
            d206 x xSum xSumN xSumSquaredN
            d207 x xSum xSumN xSumSquaredN
            d208 x xSum xSumN xSumSquaredN
            d209 x xSum xSumN xSumSquaredN
            d210 x xSum xSumN xSumSquaredN
            d211 x xSum xSumN xSumSquaredN
            d212 x xSum xSumN xSumSquaredN
            d213 x xSum xSumN xSumSquaredN
            d214 x xSum xSumN xSumSquaredN
            d215 x xSum xSumN xSumSquaredN
            d216 x xSum xSumN xSumSquaredN
            d217 x xSum xSumN xSumSquaredN
            d218 x xSum xSumN xSumSquaredN
            d219 x xSum xSumN xSumSquaredN
            d220 x xSum xSumN xSumSquaredN
            d221 x xSum xSumN xSumSquaredN
            d222 x xSum xSumN xSumSquaredN
            d223 x xSum xSumN xSumSquaredN
            d224 x xSum xSumN xSumSquaredN
            d225 x xSum xSumN xSumSquaredN
            d226 x xSum xSumN xSumSquaredN
            d227 x xSum xSumN xSumSquaredN
            d228 x xSum xSumN xSumSquaredN
            d229 x xSum xSumN xSumSquaredN
            d230 x xSum xSumN xSumSquaredN
            d231 x xSum xSumN xSumSquaredN
            d232 x xSum xSumN xSumSquaredN
            d233 x xSum xSumN xSumSquaredN
            d234 x xSum xSumN xSumSquaredN
            d235 x xSum xSumN xSumSquaredN
            d236 x xSum xSumN xSumSquaredN
            d237 x xSum xSumN xSumSquaredN
            d238 x xSum xSumN xSumSquaredN
            d239 x xSum xSumN xSumSquaredN
            d240 x xSum xSumN xSumSquaredN
            d241 x xSum xSumN xSumSquaredN
            d242 x xSum xSumN xSumSquaredN
            d243 x xSum xSumN xSumSquaredN
            d244 x xSum xSumN xSumSquaredN
            d245 x xSum xSumN xSumSquaredN
            d246 x xSum xSumN xSumSquaredN
            d247 x xSum xSumN xSumSquaredN
            d248 x xSum xSumN xSumSquaredN
            d249 x xSum xSumN xSumSquaredN
            d250 x xSum xSumN xSumSquaredN
            d251 x xSum xSumN xSumSquaredN
            d252 x xSum xSumN xSumSquaredN
            d253 x xSum xSumN xSumSquaredN
            d254 x xSum xSumN xSumSquaredN
            d255 x xSum xSumN xSumSquaredN
            d256 x xSum xSumN xSumSquaredN
            d257 x xSum xSumN xSumSquaredN
            d258 x xSum xSumN xSumSquaredN
            d259 x xSum xSumN xSumSquaredN
            d260 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData =
        {
            modelDataParams =
                {
                    modelInfo =
                        {
                            fileStructureVersionNumber = "1.6.0.1"
                            versionNumber = "1.6.0.1"
                            seedValue = seedValue
                            modelDataId = 178L
                            numberOfSubstances = 261
                            numberOfAminoAcids = ThreeAminoAcids
                            maxPeptideLength = ThreeMax
                            updateAllModels = false
                            allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
                            defaultSetIndex = 1
                        }

                    allParams = 
                        [
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
                                        synthesisDistribution = DeltaDistribution(1238238540, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
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
                                        destructionDistribution = DeltaDistribution(311171833, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
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
                                        catDestrRndEeParams = 
                                        {
                                            rateMultiplierDistr = TriangularDistribution(280964523, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                            eeForwardDistribution = BiDeltaDistribution(1359095349, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                            eeBackwardDistribution = BiDeltaDistribution(2143812225, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                        }
                                    }
                                    |> CatDestrRndParam
                                    |> CatalyticDestructionRateParam
                                usage = DependsOnParam
                            }

                            {
                                modelParam = 
                                    {
                                        simBaseDistribution = UniformDistribution(1008280070, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                                        getRateMultiplierDistr = DeltaRateMultDistrGetter
                                        getForwardEeDistr = DeltaEeDistributionGetter
                                        getBackwardEeDistr = DeltaEeDistributionGetter
                                    }
                                    |> CatDestrSimParam
                                    |> CatalyticDestructionRateParam
                                usage = PrimaryParam
                            }

                            {
                                modelParam = 
                                    {
                                        ligationDistribution = DeltaDistribution(1802884834, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
                                        forwardScale = Some 1.0
                                        backwardScale = Some 1.0
                                    }
                                    |> LigRndParam
                                    |> LigationRateParam
                                usage = PrimaryParam
                            }

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
                    (SynthesisName, 6)
                    (DestructionName, 6)
                    (CatalyticSynthesisName, 1512)
                    (CatalyticDestructionName, 1512)
                    (LigationName, 123)
                    (CatalyticLigationName, 30996)
                    (SedimentationDirectName, 22704)
                    (SedimentationAllName, 6)
                    (RacemizationName, 6)
                    (CatalyticRacemizationName, 1512)
                ]

            allReactions = 
                [
                    (WasteRecyclingName, 1)
                    (SynthesisName, 6)
                    (DestructionName, 6)
                    (LigationName, 246)
                ]
        }


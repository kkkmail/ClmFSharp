namespace Clm.Model

open System
open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmSys.ContGenPrimitives

module ModelData = 
    let seedValue = 63566386
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
            x.[0] // abundant
            x.[1] // food
            x.[2] // waste
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
            2214.00742039413 * x.[117] * x.[190] // CAA + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            -2214.00742039413 * x.[21] * x.[3] * x.[190] // CA + A + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            56.7694210357472 * x.[117] * x.[103] // CAA + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            -56.7694210357472 * x.[21] * x.[3] * x.[103] // CA + A + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            2214.00742039413 * x.[81] * x.[190] // BAA + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            -2214.00742039413 * x.[15] * x.[3] * x.[190] // BA + A + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            56.7694210357472 * x.[81] * x.[103] // BAA + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            -56.7694210357472 * x.[15] * x.[3] * x.[103] // BA + A + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            2214.00742039413 * x.[50] * x.[190] // AAc + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            -2214.00742039413 * x.[3] * x.[14] * x.[190] // A + Ac + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            56.7694210357472 * x.[50] * x.[103] // AAc + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            -56.7694210357472 * x.[3] * x.[14] * x.[103] // A + Ac + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            2214.00742039413 * x.[49] * x.[190] // AAb + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            -2214.00742039413 * x.[3] * x.[13] * x.[190] // A + Ab + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            56.7694210357472 * x.[49] * x.[103] // AAb + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            -56.7694210357472 * x.[3] * x.[13] * x.[103] // A + Ab + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            2214.00742039413 * x.[48] * x.[190] // AAa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            -2214.00742039413 * x.[3] * x.[12] * x.[190] // A + Aa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            56.7694210357472 * x.[48] * x.[103] // AAa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            -56.7694210357472 * x.[3] * x.[12] * x.[103] // A + Aa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            2214.00742039413 * x.[47] * x.[190] // AAC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            -2214.00742039413 * x.[3] * x.[11] * x.[190] // A + AC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            56.7694210357472 * x.[47] * x.[103] // AAC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            -56.7694210357472 * x.[3] * x.[11] * x.[103] // A + AC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            2214.00742039413 * x.[46] * x.[190] // AAB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            -2214.00742039413 * x.[3] * x.[10] * x.[190] // A + AB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            56.7694210357472 * x.[46] * x.[103] // AAB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            -56.7694210357472 * x.[3] * x.[10] * x.[103] // A + AB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            -2214.00742039413 * x.[3] * x.[9] * x.[190] // A + AA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            56.7694210357472 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            -56.7694210357472 * x.[3] * x.[9] * x.[103] // A + AA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            2214.00742039413 * x.[9] * x.[190] // AA + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            2214.00742039413 * x.[9] * x.[190] // AA + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            -2214.00742039413 * x.[3] * x.[3] * x.[190] // A + A + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            -2214.00742039413 * x.[3] * x.[3] * x.[190] // A + A + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            56.7694210357472 * x.[9] * x.[103] // AA + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            56.7694210357472 * x.[9] * x.[103] // AA + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            -56.7694210357472 * x.[3] * x.[3] * x.[103] // A + A + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            -56.7694210357472 * x.[3] * x.[3] * x.[103] // A + A + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            -2214.00742039413 * x.[9] * x.[3] * x.[190] // AA + A + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            56.769421035747 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            -56.769421035747 * x.[9] * x.[3] * x.[103] // AA + A + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            60.8364303415014 * x.[117] * x.[121] // CAA + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            -60.8364303415014 * x.[21] * x.[3] * x.[121] // CA + A + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            2372.62078331855 * x.[117] * x.[244] // CAA + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            -2372.62078331855 * x.[21] * x.[3] * x.[244] // CA + A + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            60.8364303415014 * x.[81] * x.[121] // BAA + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            -60.8364303415014 * x.[15] * x.[3] * x.[121] // BA + A + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            2372.62078331855 * x.[81] * x.[244] // BAA + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            -2372.62078331855 * x.[15] * x.[3] * x.[244] // BA + A + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            -60.8364303415014 * x.[9] * x.[3] * x.[121] // AA + A + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            -2372.62078331855 * x.[9] * x.[3] * x.[244] // AA + A + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            60.8364303415014 * x.[50] * x.[121] // AAc + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            -60.8364303415014 * x.[3] * x.[14] * x.[121] // A + Ac + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            2372.62078331855 * x.[50] * x.[244] // AAc + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            -2372.62078331855 * x.[3] * x.[14] * x.[244] // A + Ac + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            60.8364303415014 * x.[49] * x.[121] // AAb + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            -60.8364303415014 * x.[3] * x.[13] * x.[121] // A + Ab + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            2372.62078331855 * x.[49] * x.[244] // AAb + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            -2372.62078331855 * x.[3] * x.[13] * x.[244] // A + Ab + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            60.8364303415014 * x.[48] * x.[121] // AAa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            -60.8364303415014 * x.[3] * x.[12] * x.[121] // A + Aa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            2372.62078331855 * x.[48] * x.[244] // AAa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            -2372.62078331855 * x.[3] * x.[12] * x.[244] // A + Aa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            60.8364303415014 * x.[47] * x.[121] // AAC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            -60.8364303415014 * x.[3] * x.[11] * x.[121] // A + AC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            2372.62078331855 * x.[47] * x.[244] // AAC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            -2372.62078331855 * x.[3] * x.[11] * x.[244] // A + AC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            -60.8364303415014 * x.[3] * x.[10] * x.[121] // A + AB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            -2372.62078331855 * x.[3] * x.[10] * x.[244] // A + AB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            -60.8364303415014 * x.[3] * x.[9] * x.[121] // A + AA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            -2372.62078331855 * x.[3] * x.[9] * x.[244] // A + AA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            60.8364303415014 * x.[9] * x.[121] // AA + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            60.8364303415014 * x.[9] * x.[121] // AA + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            -60.8364303415014 * x.[3] * x.[3] * x.[121] // A + A + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            -60.8364303415014 * x.[3] * x.[3] * x.[121] // A + A + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            2372.62078331855 * x.[9] * x.[244] // AA + caB | catalytic ligation: A + A + caB <-> AA + caB
            2372.62078331855 * x.[9] * x.[244] // AA + caB | catalytic ligation: A + A + caB <-> AA + caB
            -2372.62078331855 * x.[3] * x.[3] * x.[244] // A + A + caB | catalytic ligation: A + A + caB <-> AA + caB
            -2372.62078331855 * x.[3] * x.[3] * x.[244] // A + A + caB | catalytic ligation: A + A + caB <-> AA + caB
            60.8364303415014 * x.[56] * x.[121] // ABc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            -60.8364303415014 * x.[3] * x.[20] * x.[121] // A + Bc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            2372.62078331855 * x.[56] * x.[244] // ABc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            -2372.62078331855 * x.[3] * x.[20] * x.[244] // A + Bc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            60.8364303415014 * x.[55] * x.[121] // ABb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            -60.8364303415014 * x.[3] * x.[19] * x.[121] // A + Bb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            2372.62078331855 * x.[55] * x.[244] // ABb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            -2372.62078331855 * x.[3] * x.[19] * x.[244] // A + Bb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            60.8364303415014 * x.[54] * x.[121] // ABa + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            -60.8364303415014 * x.[3] * x.[18] * x.[121] // A + Ba + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            2372.62078331855 * x.[54] * x.[244] // ABa + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            -2372.62078331855 * x.[3] * x.[18] * x.[244] // A + Ba + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            60.8364303415014 * x.[53] * x.[121] // ABC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            -60.8364303415014 * x.[3] * x.[17] * x.[121] // A + BC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            2372.62078331855 * x.[53] * x.[244] // ABC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            -2372.62078331855 * x.[3] * x.[17] * x.[244] // A + BC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            60.8364303415014 * x.[52] * x.[121] // ABB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            -60.8364303415014 * x.[3] * x.[16] * x.[121] // A + BB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            2372.62078331855 * x.[52] * x.[244] // ABB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            -2372.62078331855 * x.[3] * x.[16] * x.[244] // A + BB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            60.8364303415014 * x.[51] * x.[121] // ABA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            -60.8364303415014 * x.[3] * x.[15] * x.[121] // A + BA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            2372.62078331855 * x.[51] * x.[244] // ABA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            -2372.62078331855 * x.[3] * x.[15] * x.[244] // A + BA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            60.8364303415014 * x.[10] * x.[121] // AB + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            -60.8364303415014 * x.[3] * x.[4] * x.[121] // A + B + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            2372.62078331855 * x.[10] * x.[244] // AB + caB | catalytic ligation: A + B + caB <-> AB + caB
            -2372.62078331855 * x.[3] * x.[4] * x.[244] // A + B + caB | catalytic ligation: A + B + caB <-> AB + caB
            1.0 * x.[237] // cCA | ligation: cC + A <-> cCA
            -1.0 * x.[41] * x.[3] // cC + A | ligation: cC + A <-> cCA
            1.0 * x.[147] // CcA | ligation: Cc + A <-> CcA
            -1.0 * x.[26] * x.[3] // Cc + A | ligation: Cc + A <-> CcA
            1.0 * x.[231] // cBA | ligation: cB + A <-> cBA
            -1.0 * x.[40] * x.[3] // cB + A | ligation: cB + A <-> cBA
            1.0 * x.[141] // CbA | ligation: Cb + A <-> CbA
            -1.0 * x.[25] * x.[3] // Cb + A | ligation: Cb + A <-> CbA
            1.0 * x.[225] // cAA | ligation: cA + A <-> cAA
            -1.0 * x.[39] * x.[3] // cA + A | ligation: cA + A <-> cAA
            1.0 * x.[135] // CaA | ligation: Ca + A <-> CaA
            -1.0 * x.[24] * x.[3] // Ca + A | ligation: Ca + A <-> CaA
            1.0 * x.[255] // ccA | ligation: cc + A <-> ccA
            -1.0 * x.[44] * x.[3] // cc + A | ligation: cc + A <-> ccA
            1.0 * x.[129] // CCA | ligation: CC + A <-> CCA
            -1.0 * x.[23] * x.[3] // CC + A | ligation: CC + A <-> CCA
            1.0 * x.[249] // cbA | ligation: cb + A <-> cbA
            -1.0 * x.[43] * x.[3] // cb + A | ligation: cb + A <-> cbA
            1.0 * x.[123] // CBA | ligation: CB + A <-> CBA
            -1.0 * x.[22] * x.[3] // CB + A | ligation: CB + A <-> CBA
            1.0 * x.[243] // caA | ligation: ca + A <-> caA
            -1.0 * x.[42] * x.[3] // ca + A | ligation: ca + A <-> caA
            1.0 * x.[117] // CAA | ligation: CA + A <-> CAA
            -1.0 * x.[21] * x.[3] // CA + A | ligation: CA + A <-> CAA
            1.0 * x.[39] // cA | ligation: c + A <-> cA
            -1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[21] // CA | ligation: C + A <-> CA
            -1.0 * x.[5] * x.[3] // C + A | ligation: C + A <-> CA
            1.0 * x.[201] // bCA | ligation: bC + A <-> bCA
            -1.0 * x.[35] * x.[3] // bC + A | ligation: bC + A <-> bCA
            1.0 * x.[111] // BcA | ligation: Bc + A <-> BcA
            -1.0 * x.[20] * x.[3] // Bc + A | ligation: Bc + A <-> BcA
            1.0 * x.[195] // bBA | ligation: bB + A <-> bBA
            -1.0 * x.[34] * x.[3] // bB + A | ligation: bB + A <-> bBA
            1.0 * x.[105] // BbA | ligation: Bb + A <-> BbA
            -1.0 * x.[19] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            1.0 * x.[189] // bAA | ligation: bA + A <-> bAA
            -1.0 * x.[33] * x.[3] // bA + A | ligation: bA + A <-> bAA
            1.0 * x.[99] // BaA | ligation: Ba + A <-> BaA
            -1.0 * x.[18] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            1.0 * x.[219] // bcA | ligation: bc + A <-> bcA
            -1.0 * x.[38] * x.[3] // bc + A | ligation: bc + A <-> bcA
            1.0 * x.[93] // BCA | ligation: BC + A <-> BCA
            -1.0 * x.[17] * x.[3] // BC + A | ligation: BC + A <-> BCA
            1.0 * x.[213] // bbA | ligation: bb + A <-> bbA
            -1.0 * x.[37] * x.[3] // bb + A | ligation: bb + A <-> bbA
            1.0 * x.[87] // BBA | ligation: BB + A <-> BBA
            -1.0 * x.[16] * x.[3] // BB + A | ligation: BB + A <-> BBA
            1.0 * x.[207] // baA | ligation: ba + A <-> baA
            -1.0 * x.[36] * x.[3] // ba + A | ligation: ba + A <-> baA
            1.0 * x.[81] // BAA | ligation: BA + A <-> BAA
            -1.0 * x.[15] * x.[3] // BA + A | ligation: BA + A <-> BAA
            1.0 * x.[33] // bA | ligation: b + A <-> bA
            -1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[15] // BA | ligation: B + A <-> BA
            -1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[165] // aCA | ligation: aC + A <-> aCA
            -1.0 * x.[29] * x.[3] // aC + A | ligation: aC + A <-> aCA
            1.0 * x.[75] // AcA | ligation: Ac + A <-> AcA
            -1.0 * x.[14] * x.[3] // Ac + A | ligation: Ac + A <-> AcA
            1.0 * x.[159] // aBA | ligation: aB + A <-> aBA
            -1.0 * x.[28] * x.[3] // aB + A | ligation: aB + A <-> aBA
            1.0 * x.[69] // AbA | ligation: Ab + A <-> AbA
            -1.0 * x.[13] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            1.0 * x.[153] // aAA | ligation: aA + A <-> aAA
            -1.0 * x.[27] * x.[3] // aA + A | ligation: aA + A <-> aAA
            1.0 * x.[63] // AaA | ligation: Aa + A <-> AaA
            -1.0 * x.[12] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            1.0 * x.[183] // acA | ligation: ac + A <-> acA
            -1.0 * x.[32] * x.[3] // ac + A | ligation: ac + A <-> acA
            1.0 * x.[57] // ACA | ligation: AC + A <-> ACA
            -1.0 * x.[11] * x.[3] // AC + A | ligation: AC + A <-> ACA
            1.0 * x.[177] // abA | ligation: ab + A <-> abA
            -1.0 * x.[31] * x.[3] // ab + A | ligation: ab + A <-> abA
            1.0 * x.[51] // ABA | ligation: AB + A <-> ABA
            -1.0 * x.[10] * x.[3] // AB + A | ligation: AB + A <-> ABA
            1.0 * x.[171] // aaA | ligation: aa + A <-> aaA
            -1.0 * x.[30] * x.[3] // aa + A | ligation: aa + A <-> aaA
            1.0 * x.[45] // AAA | ligation: AA + A <-> AAA
            -1.0 * x.[9] * x.[3] // AA + A | ligation: AA + A <-> AAA
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
            1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
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
            1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
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
            1.0 * x.[27] // aA | ligation: a + A <-> aA
            -1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
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
            1.0 * x.[11] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
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
            1.0 * x.[10] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
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
            1.0 * x.[9] // AA | ligation: A + A <-> AA
            1.0 * x.[9] // AA | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
            -1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 4 - B
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[98] * x.[190] // BCc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            -2214.00742039413 * x.[4] * x.[26] * x.[190] // B + Cc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            56.7694210357472 * x.[98] * x.[103] // BCc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            -56.7694210357472 * x.[4] * x.[26] * x.[103] // B + Cc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            2214.00742039413 * x.[97] * x.[190] // BCb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            -2214.00742039413 * x.[4] * x.[25] * x.[190] // B + Cb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            56.7694210357472 * x.[97] * x.[103] // BCb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            -56.7694210357472 * x.[4] * x.[25] * x.[103] // B + Cb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            2214.00742039413 * x.[96] * x.[190] // BCa + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            -2214.00742039413 * x.[4] * x.[24] * x.[190] // B + Ca + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            56.7694210357472 * x.[96] * x.[103] // BCa + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            -56.7694210357472 * x.[4] * x.[24] * x.[103] // B + Ca + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            -2214.00742039413 * x.[4] * x.[23] * x.[190] // B + CC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            -56.7694210357472 * x.[4] * x.[23] * x.[103] // B + CC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            2214.00742039413 * x.[94] * x.[190] // BCB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            -2214.00742039413 * x.[4] * x.[22] * x.[190] // B + CB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            56.7694210357472 * x.[94] * x.[103] // BCB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            -56.7694210357472 * x.[4] * x.[22] * x.[103] // B + CB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            2214.00742039413 * x.[93] * x.[190] // BCA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            -2214.00742039413 * x.[4] * x.[21] * x.[190] // B + CA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            56.7694210357472 * x.[93] * x.[103] // BCA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            -56.7694210357472 * x.[4] * x.[21] * x.[103] // B + CA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            2214.00742039413 * x.[17] * x.[190] // BC + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            -2214.00742039413 * x.[4] * x.[5] * x.[190] // B + C + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            56.7694210357472 * x.[17] * x.[103] // BC + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            -56.7694210357472 * x.[4] * x.[5] * x.[103] // B + C + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            60.8364303415014 * x.[118] * x.[121] // CAB + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            -60.8364303415014 * x.[21] * x.[4] * x.[121] // CA + B + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            2372.62078331855 * x.[118] * x.[244] // CAB + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            -2372.62078331855 * x.[21] * x.[4] * x.[244] // CA + B + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            60.8364303415014 * x.[82] * x.[121] // BAB + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            -60.8364303415014 * x.[15] * x.[4] * x.[121] // BA + B + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            2372.62078331855 * x.[82] * x.[244] // BAB + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            -2372.62078331855 * x.[15] * x.[4] * x.[244] // BA + B + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            -60.8364303415014 * x.[9] * x.[4] * x.[121] // AA + B + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            -2372.62078331855 * x.[9] * x.[4] * x.[244] // AA + B + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            60.8364303415014 * x.[10] * x.[121] // AB + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            -60.8364303415014 * x.[3] * x.[4] * x.[121] // A + B + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            2372.62078331855 * x.[10] * x.[244] // AB + caB | catalytic ligation: A + B + caB <-> AB + caB
            -2372.62078331855 * x.[3] * x.[4] * x.[244] // A + B + caB | catalytic ligation: A + B + caB <-> AB + caB
            60.8364303415014 * x.[94] * x.[121] // BCB + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            -60.8364303415014 * x.[17] * x.[4] * x.[121] // BC + B + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            2372.62078331855 * x.[94] * x.[244] // BCB + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            -2372.62078331855 * x.[17] * x.[4] * x.[244] // BC + B + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            60.8364303415014 * x.[58] * x.[121] // ACB + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            -60.8364303415014 * x.[11] * x.[4] * x.[121] // AC + B + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            2372.62078331855 * x.[58] * x.[244] // ACB + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            -2372.62078331855 * x.[11] * x.[4] * x.[244] // AC + B + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            60.8364303415014 * x.[22] * x.[121] // CB + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            -60.8364303415014 * x.[5] * x.[4] * x.[121] // C + B + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            2372.62078331855 * x.[22] * x.[244] // CB + caB | catalytic ligation: C + B + caB <-> CB + caB
            -2372.62078331855 * x.[5] * x.[4] * x.[244] // C + B + caB | catalytic ligation: C + B + caB <-> CB + caB
            60.8364303415013 * x.[130] * x.[121] // CCB + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            -60.8364303415013 * x.[23] * x.[4] * x.[121] // CC + B + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            2372.62078331855 * x.[130] * x.[244] // CCB + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            -2372.62078331855 * x.[23] * x.[4] * x.[244] // CC + B + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            1.0 * x.[238] // cCB | ligation: cC + B <-> cCB
            -1.0 * x.[41] * x.[4] // cC + B | ligation: cC + B <-> cCB
            1.0 * x.[148] // CcB | ligation: Cc + B <-> CcB
            -1.0 * x.[26] * x.[4] // Cc + B | ligation: Cc + B <-> CcB
            1.0 * x.[232] // cBB | ligation: cB + B <-> cBB
            -1.0 * x.[40] * x.[4] // cB + B | ligation: cB + B <-> cBB
            1.0 * x.[142] // CbB | ligation: Cb + B <-> CbB
            -1.0 * x.[25] * x.[4] // Cb + B | ligation: Cb + B <-> CbB
            1.0 * x.[226] // cAB | ligation: cA + B <-> cAB
            -1.0 * x.[39] * x.[4] // cA + B | ligation: cA + B <-> cAB
            1.0 * x.[136] // CaB | ligation: Ca + B <-> CaB
            -1.0 * x.[24] * x.[4] // Ca + B | ligation: Ca + B <-> CaB
            1.0 * x.[256] // ccB | ligation: cc + B <-> ccB
            -1.0 * x.[44] * x.[4] // cc + B | ligation: cc + B <-> ccB
            1.0 * x.[130] // CCB | ligation: CC + B <-> CCB
            -1.0 * x.[23] * x.[4] // CC + B | ligation: CC + B <-> CCB
            1.0 * x.[250] // cbB | ligation: cb + B <-> cbB
            -1.0 * x.[43] * x.[4] // cb + B | ligation: cb + B <-> cbB
            1.0 * x.[124] // CBB | ligation: CB + B <-> CBB
            -1.0 * x.[22] * x.[4] // CB + B | ligation: CB + B <-> CBB
            1.0 * x.[244] // caB | ligation: ca + B <-> caB
            -1.0 * x.[42] * x.[4] // ca + B | ligation: ca + B <-> caB
            1.0 * x.[118] // CAB | ligation: CA + B <-> CAB
            -1.0 * x.[21] * x.[4] // CA + B | ligation: CA + B <-> CAB
            1.0 * x.[40] // cB | ligation: c + B <-> cB
            -1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[22] // CB | ligation: C + B <-> CB
            -1.0 * x.[5] * x.[4] // C + B | ligation: C + B <-> CB
            1.0 * x.[202] // bCB | ligation: bC + B <-> bCB
            -1.0 * x.[35] * x.[4] // bC + B | ligation: bC + B <-> bCB
            1.0 * x.[112] // BcB | ligation: Bc + B <-> BcB
            -1.0 * x.[20] * x.[4] // Bc + B | ligation: Bc + B <-> BcB
            1.0 * x.[196] // bBB | ligation: bB + B <-> bBB
            -1.0 * x.[34] * x.[4] // bB + B | ligation: bB + B <-> bBB
            1.0 * x.[106] // BbB | ligation: Bb + B <-> BbB
            -1.0 * x.[19] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            1.0 * x.[190] // bAB | ligation: bA + B <-> bAB
            -1.0 * x.[33] * x.[4] // bA + B | ligation: bA + B <-> bAB
            1.0 * x.[100] // BaB | ligation: Ba + B <-> BaB
            -1.0 * x.[18] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            1.0 * x.[220] // bcB | ligation: bc + B <-> bcB
            -1.0 * x.[38] * x.[4] // bc + B | ligation: bc + B <-> bcB
            1.0 * x.[94] // BCB | ligation: BC + B <-> BCB
            -1.0 * x.[17] * x.[4] // BC + B | ligation: BC + B <-> BCB
            1.0 * x.[214] // bbB | ligation: bb + B <-> bbB
            -1.0 * x.[37] * x.[4] // bb + B | ligation: bb + B <-> bbB
            1.0 * x.[88] // BBB | ligation: BB + B <-> BBB
            -1.0 * x.[16] * x.[4] // BB + B | ligation: BB + B <-> BBB
            1.0 * x.[208] // baB | ligation: ba + B <-> baB
            -1.0 * x.[36] * x.[4] // ba + B | ligation: ba + B <-> baB
            1.0 * x.[82] // BAB | ligation: BA + B <-> BAB
            -1.0 * x.[15] * x.[4] // BA + B | ligation: BA + B <-> BAB
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
            1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
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
            1.0 * x.[34] // bB | ligation: b + B <-> bB
            -1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
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
            1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
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
            1.0 * x.[17] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
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
            1.0 * x.[16] // BB | ligation: B + B <-> BB
            1.0 * x.[16] // BB | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
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
            1.0 * x.[15] // BA | ligation: B + A <-> BA
            -1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[166] // aCB | ligation: aC + B <-> aCB
            -1.0 * x.[29] * x.[4] // aC + B | ligation: aC + B <-> aCB
            1.0 * x.[76] // AcB | ligation: Ac + B <-> AcB
            -1.0 * x.[14] * x.[4] // Ac + B | ligation: Ac + B <-> AcB
            1.0 * x.[160] // aBB | ligation: aB + B <-> aBB
            -1.0 * x.[28] * x.[4] // aB + B | ligation: aB + B <-> aBB
            1.0 * x.[70] // AbB | ligation: Ab + B <-> AbB
            -1.0 * x.[13] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            1.0 * x.[154] // aAB | ligation: aA + B <-> aAB
            -1.0 * x.[27] * x.[4] // aA + B | ligation: aA + B <-> aAB
            1.0 * x.[64] // AaB | ligation: Aa + B <-> AaB
            -1.0 * x.[12] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            1.0 * x.[184] // acB | ligation: ac + B <-> acB
            -1.0 * x.[32] * x.[4] // ac + B | ligation: ac + B <-> acB
            1.0 * x.[58] // ACB | ligation: AC + B <-> ACB
            -1.0 * x.[11] * x.[4] // AC + B | ligation: AC + B <-> ACB
            1.0 * x.[178] // abB | ligation: ab + B <-> abB
            -1.0 * x.[31] * x.[4] // ab + B | ligation: ab + B <-> abB
            1.0 * x.[52] // ABB | ligation: AB + B <-> ABB
            -1.0 * x.[10] * x.[4] // AB + B | ligation: AB + B <-> ABB
            1.0 * x.[172] // aaB | ligation: aa + B <-> aaB
            -1.0 * x.[30] * x.[4] // aa + B | ligation: aa + B <-> aaB
            1.0 * x.[46] // AAB | ligation: AA + B <-> AAB
            -1.0 * x.[9] * x.[4] // AA + B | ligation: AA + B <-> AAB
            1.0 * x.[28] // aB | ligation: a + B <-> aB
            -1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[10] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 5 - C
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            -2214.00742039413 * x.[23] * x.[5] * x.[190] // CC + C + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            -56.7694210357472 * x.[23] * x.[5] * x.[103] // CC + C + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            -2214.00742039413 * x.[17] * x.[5] * x.[190] // BC + C + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            -56.7694210357472 * x.[17] * x.[5] * x.[103] // BC + C + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            2214.00742039413 * x.[59] * x.[190] // ACC + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            -2214.00742039413 * x.[11] * x.[5] * x.[190] // AC + C + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            56.7694210357472 * x.[59] * x.[103] // ACC + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            -56.7694210357472 * x.[11] * x.[5] * x.[103] // AC + C + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            2214.00742039413 * x.[134] * x.[190] // CCc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            -2214.00742039413 * x.[5] * x.[26] * x.[190] // C + Cc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            56.7694210357472 * x.[134] * x.[103] // CCc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            -56.7694210357472 * x.[5] * x.[26] * x.[103] // C + Cc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            2214.00742039413 * x.[133] * x.[190] // CCb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            -2214.00742039413 * x.[5] * x.[25] * x.[190] // C + Cb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            56.7694210357472 * x.[133] * x.[103] // CCb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            -56.7694210357472 * x.[5] * x.[25] * x.[103] // C + Cb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            2214.00742039413 * x.[132] * x.[190] // CCa + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            -2214.00742039413 * x.[5] * x.[24] * x.[190] // C + Ca + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            56.7694210357472 * x.[132] * x.[103] // CCa + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            -56.7694210357472 * x.[5] * x.[24] * x.[103] // C + Ca + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            -2214.00742039413 * x.[5] * x.[23] * x.[190] // C + CC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            -56.7694210357472 * x.[5] * x.[23] * x.[103] // C + CC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            2214.00742039413 * x.[130] * x.[190] // CCB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            -2214.00742039413 * x.[5] * x.[22] * x.[190] // C + CB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            56.7694210357472 * x.[130] * x.[103] // CCB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            -56.7694210357472 * x.[5] * x.[22] * x.[103] // C + CB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            2214.00742039413 * x.[129] * x.[190] // CCA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            -2214.00742039413 * x.[5] * x.[21] * x.[190] // C + CA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            56.7694210357472 * x.[129] * x.[103] // CCA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            -56.7694210357472 * x.[5] * x.[21] * x.[103] // C + CA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            2214.00742039413 * x.[23] * x.[190] // CC + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            2214.00742039413 * x.[23] * x.[190] // CC + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            -2214.00742039413 * x.[5] * x.[5] * x.[190] // C + C + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            -2214.00742039413 * x.[5] * x.[5] * x.[190] // C + C + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            56.7694210357472 * x.[23] * x.[103] // CC + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            56.7694210357472 * x.[23] * x.[103] // CC + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            -56.7694210357472 * x.[5] * x.[5] * x.[103] // C + C + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            -56.7694210357472 * x.[5] * x.[5] * x.[103] // C + C + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            2214.00742039413 * x.[125] * x.[190] // CBC + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            -2214.00742039413 * x.[22] * x.[5] * x.[190] // CB + C + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            56.7694210357472 * x.[125] * x.[103] // CBC + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            -56.7694210357472 * x.[22] * x.[5] * x.[103] // CB + C + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            2214.00742039413 * x.[89] * x.[190] // BBC + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            -2214.00742039413 * x.[16] * x.[5] * x.[190] // BB + C + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            56.7694210357472 * x.[89] * x.[103] // BBC + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            -56.7694210357472 * x.[16] * x.[5] * x.[103] // BB + C + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            2214.00742039413 * x.[53] * x.[190] // ABC + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            -2214.00742039413 * x.[10] * x.[5] * x.[190] // AB + C + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            56.7694210357472 * x.[53] * x.[103] // ABC + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            -56.7694210357472 * x.[10] * x.[5] * x.[103] // AB + C + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            2214.00742039413 * x.[17] * x.[190] // BC + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            -2214.00742039413 * x.[4] * x.[5] * x.[190] // B + C + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            56.7694210357472 * x.[17] * x.[103] // BC + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            -56.7694210357472 * x.[4] * x.[5] * x.[103] // B + C + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            60.8364303415014 * x.[128] * x.[121] // CBc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            -60.8364303415014 * x.[5] * x.[20] * x.[121] // C + Bc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            2372.62078331855 * x.[128] * x.[244] // CBc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            -2372.62078331855 * x.[5] * x.[20] * x.[244] // C + Bc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            60.8364303415014 * x.[127] * x.[121] // CBb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            -60.8364303415014 * x.[5] * x.[19] * x.[121] // C + Bb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            2372.62078331855 * x.[127] * x.[244] // CBb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            -2372.62078331855 * x.[5] * x.[19] * x.[244] // C + Bb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            60.8364303415014 * x.[126] * x.[121] // CBa + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            -60.8364303415014 * x.[5] * x.[18] * x.[121] // C + Ba + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            2372.62078331855 * x.[126] * x.[244] // CBa + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            -2372.62078331855 * x.[5] * x.[18] * x.[244] // C + Ba + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            60.8364303415014 * x.[125] * x.[121] // CBC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            -60.8364303415014 * x.[5] * x.[17] * x.[121] // C + BC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            2372.62078331855 * x.[125] * x.[244] // CBC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            -2372.62078331855 * x.[5] * x.[17] * x.[244] // C + BC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            60.8364303415014 * x.[124] * x.[121] // CBB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            -60.8364303415014 * x.[5] * x.[16] * x.[121] // C + BB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            2372.62078331855 * x.[124] * x.[244] // CBB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            -2372.62078331855 * x.[5] * x.[16] * x.[244] // C + BB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            60.8364303415014 * x.[123] * x.[121] // CBA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            -60.8364303415014 * x.[5] * x.[15] * x.[121] // C + BA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            2372.62078331855 * x.[123] * x.[244] // CBA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            -2372.62078331855 * x.[5] * x.[15] * x.[244] // C + BA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            60.8364303415014 * x.[22] * x.[121] // CB + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            -60.8364303415014 * x.[5] * x.[4] * x.[121] // C + B + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            2372.62078331855 * x.[22] * x.[244] // CB + caB | catalytic ligation: C + B + caB <-> CB + caB
            -2372.62078331855 * x.[5] * x.[4] * x.[244] // C + B + caB | catalytic ligation: C + B + caB <-> CB + caB
            1.0 * x.[239] // cCC | ligation: cC + C <-> cCC
            -1.0 * x.[41] * x.[5] // cC + C | ligation: cC + C <-> cCC
            1.0 * x.[149] // CcC | ligation: Cc + C <-> CcC
            -1.0 * x.[26] * x.[5] // Cc + C | ligation: Cc + C <-> CcC
            1.0 * x.[233] // cBC | ligation: cB + C <-> cBC
            -1.0 * x.[40] * x.[5] // cB + C | ligation: cB + C <-> cBC
            1.0 * x.[143] // CbC | ligation: Cb + C <-> CbC
            -1.0 * x.[25] * x.[5] // Cb + C | ligation: Cb + C <-> CbC
            1.0 * x.[227] // cAC | ligation: cA + C <-> cAC
            -1.0 * x.[39] * x.[5] // cA + C | ligation: cA + C <-> cAC
            1.0 * x.[137] // CaC | ligation: Ca + C <-> CaC
            -1.0 * x.[24] * x.[5] // Ca + C | ligation: Ca + C <-> CaC
            1.0 * x.[257] // ccC | ligation: cc + C <-> ccC
            -1.0 * x.[44] * x.[5] // cc + C | ligation: cc + C <-> ccC
            1.0 * x.[131] // CCC | ligation: CC + C <-> CCC
            -1.0 * x.[23] * x.[5] // CC + C | ligation: CC + C <-> CCC
            1.0 * x.[251] // cbC | ligation: cb + C <-> cbC
            -1.0 * x.[43] * x.[5] // cb + C | ligation: cb + C <-> cbC
            1.0 * x.[125] // CBC | ligation: CB + C <-> CBC
            -1.0 * x.[22] * x.[5] // CB + C | ligation: CB + C <-> CBC
            1.0 * x.[245] // caC | ligation: ca + C <-> caC
            -1.0 * x.[42] * x.[5] // ca + C | ligation: ca + C <-> caC
            1.0 * x.[119] // CAC | ligation: CA + C <-> CAC
            -1.0 * x.[21] * x.[5] // CA + C | ligation: CA + C <-> CAC
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
            1.0 * x.[41] // cC | ligation: c + C <-> cC
            -1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
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
            1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
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
            1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
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
            1.0 * x.[23] // CC | ligation: C + C <-> CC
            1.0 * x.[23] // CC | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
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
            1.0 * x.[22] // CB | ligation: C + B <-> CB
            -1.0 * x.[5] * x.[4] // C + B | ligation: C + B <-> CB
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
            1.0 * x.[21] // CA | ligation: C + A <-> CA
            -1.0 * x.[5] * x.[3] // C + A | ligation: C + A <-> CA
            1.0 * x.[203] // bCC | ligation: bC + C <-> bCC
            -1.0 * x.[35] * x.[5] // bC + C | ligation: bC + C <-> bCC
            1.0 * x.[113] // BcC | ligation: Bc + C <-> BcC
            -1.0 * x.[20] * x.[5] // Bc + C | ligation: Bc + C <-> BcC
            1.0 * x.[197] // bBC | ligation: bB + C <-> bBC
            -1.0 * x.[34] * x.[5] // bB + C | ligation: bB + C <-> bBC
            1.0 * x.[107] // BbC | ligation: Bb + C <-> BbC
            -1.0 * x.[19] * x.[5] // Bb + C | ligation: Bb + C <-> BbC
            1.0 * x.[191] // bAC | ligation: bA + C <-> bAC
            -1.0 * x.[33] * x.[5] // bA + C | ligation: bA + C <-> bAC
            1.0 * x.[101] // BaC | ligation: Ba + C <-> BaC
            -1.0 * x.[18] * x.[5] // Ba + C | ligation: Ba + C <-> BaC
            1.0 * x.[221] // bcC | ligation: bc + C <-> bcC
            -1.0 * x.[38] * x.[5] // bc + C | ligation: bc + C <-> bcC
            1.0 * x.[95] // BCC | ligation: BC + C <-> BCC
            -1.0 * x.[17] * x.[5] // BC + C | ligation: BC + C <-> BCC
            1.0 * x.[215] // bbC | ligation: bb + C <-> bbC
            -1.0 * x.[37] * x.[5] // bb + C | ligation: bb + C <-> bbC
            1.0 * x.[89] // BBC | ligation: BB + C <-> BBC
            -1.0 * x.[16] * x.[5] // BB + C | ligation: BB + C <-> BBC
            1.0 * x.[209] // baC | ligation: ba + C <-> baC
            -1.0 * x.[36] * x.[5] // ba + C | ligation: ba + C <-> baC
            1.0 * x.[83] // BAC | ligation: BA + C <-> BAC
            -1.0 * x.[15] * x.[5] // BA + C | ligation: BA + C <-> BAC
            1.0 * x.[35] // bC | ligation: b + C <-> bC
            -1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[17] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[167] // aCC | ligation: aC + C <-> aCC
            -1.0 * x.[29] * x.[5] // aC + C | ligation: aC + C <-> aCC
            1.0 * x.[77] // AcC | ligation: Ac + C <-> AcC
            -1.0 * x.[14] * x.[5] // Ac + C | ligation: Ac + C <-> AcC
            1.0 * x.[161] // aBC | ligation: aB + C <-> aBC
            -1.0 * x.[28] * x.[5] // aB + C | ligation: aB + C <-> aBC
            1.0 * x.[71] // AbC | ligation: Ab + C <-> AbC
            -1.0 * x.[13] * x.[5] // Ab + C | ligation: Ab + C <-> AbC
            1.0 * x.[155] // aAC | ligation: aA + C <-> aAC
            -1.0 * x.[27] * x.[5] // aA + C | ligation: aA + C <-> aAC
            1.0 * x.[65] // AaC | ligation: Aa + C <-> AaC
            -1.0 * x.[12] * x.[5] // Aa + C | ligation: Aa + C <-> AaC
            1.0 * x.[185] // acC | ligation: ac + C <-> acC
            -1.0 * x.[32] * x.[5] // ac + C | ligation: ac + C <-> acC
            1.0 * x.[59] // ACC | ligation: AC + C <-> ACC
            -1.0 * x.[11] * x.[5] // AC + C | ligation: AC + C <-> ACC
            1.0 * x.[179] // abC | ligation: ab + C <-> abC
            -1.0 * x.[31] * x.[5] // ab + C | ligation: ab + C <-> abC
            1.0 * x.[53] // ABC | ligation: AB + C <-> ABC
            -1.0 * x.[10] * x.[5] // AB + C | ligation: AB + C <-> ABC
            1.0 * x.[173] // aaC | ligation: aa + C <-> aaC
            -1.0 * x.[30] * x.[5] // aa + C | ligation: aa + C <-> aaC
            1.0 * x.[47] // AAC | ligation: AA + C <-> AAC
            -1.0 * x.[9] * x.[5] // AA + C | ligation: AA + C <-> AAC
            1.0 * x.[29] // aC | ligation: a + C <-> aC
            -1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[11] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
        |]
        |> Array.sum


    // 6 - a
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[246] * x.[103] // caa + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            -2214.00742039413 * x.[42] * x.[6] * x.[103] // ca + a + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            56.7694210357472 * x.[246] * x.[190] // caa + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            -56.7694210357472 * x.[42] * x.[6] * x.[190] // ca + a + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            2214.00742039413 * x.[210] * x.[103] // baa + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            -2214.00742039413 * x.[36] * x.[6] * x.[103] // ba + a + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            56.7694210357472 * x.[210] * x.[190] // baa + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            -56.7694210357472 * x.[36] * x.[6] * x.[190] // ba + a + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            2214.00742039413 * x.[173] * x.[103] // aaC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            -2214.00742039413 * x.[6] * x.[29] * x.[103] // a + aC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            56.7694210357472 * x.[173] * x.[190] // aaC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            -56.7694210357472 * x.[6] * x.[29] * x.[190] // a + aC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            2214.00742039413 * x.[172] * x.[103] // aaB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            -2214.00742039413 * x.[6] * x.[28] * x.[103] // a + aB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            56.7694210357472 * x.[172] * x.[190] // aaB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            -56.7694210357472 * x.[6] * x.[28] * x.[190] // a + aB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            2214.00742039413 * x.[171] * x.[103] // aaA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            -2214.00742039413 * x.[6] * x.[27] * x.[103] // a + aA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            56.7694210357472 * x.[171] * x.[190] // aaA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            -56.7694210357472 * x.[6] * x.[27] * x.[190] // a + aA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            2214.00742039413 * x.[176] * x.[103] // aac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            -2214.00742039413 * x.[6] * x.[32] * x.[103] // a + ac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            56.7694210357472 * x.[176] * x.[190] // aac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            -56.7694210357472 * x.[6] * x.[32] * x.[190] // a + ac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            2214.00742039413 * x.[175] * x.[103] // aab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            -2214.00742039413 * x.[6] * x.[31] * x.[103] // a + ab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            56.7694210357472 * x.[175] * x.[190] // aab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            -56.7694210357472 * x.[6] * x.[31] * x.[190] // a + ab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            -2214.00742039413 * x.[6] * x.[30] * x.[103] // a + aa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            56.7694210357472 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            -56.7694210357472 * x.[6] * x.[30] * x.[190] // a + aa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            2214.00742039413 * x.[30] * x.[103] // aa + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            2214.00742039413 * x.[30] * x.[103] // aa + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            -2214.00742039413 * x.[6] * x.[6] * x.[103] // a + a + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            -2214.00742039413 * x.[6] * x.[6] * x.[103] // a + a + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            56.7694210357472 * x.[30] * x.[190] // aa + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            56.7694210357472 * x.[30] * x.[190] // aa + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            -56.7694210357472 * x.[6] * x.[6] * x.[190] // a + a + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            -56.7694210357472 * x.[6] * x.[6] * x.[190] // a + a + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            -2214.00742039413 * x.[30] * x.[6] * x.[103] // aa + a + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            56.769421035747 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            -56.769421035747 * x.[30] * x.[6] * x.[190] // aa + a + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            60.8364303415014 * x.[246] * x.[244] // caa + caB | catalytic ligation: ca + a + caB <-> caa + caB
            -60.8364303415014 * x.[42] * x.[6] * x.[244] // ca + a + caB | catalytic ligation: ca + a + caB <-> caa + caB
            2372.62078331855 * x.[246] * x.[121] // caa + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            -2372.62078331855 * x.[42] * x.[6] * x.[121] // ca + a + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            60.8364303415014 * x.[210] * x.[244] // baa + caB | catalytic ligation: ba + a + caB <-> baa + caB
            -60.8364303415014 * x.[36] * x.[6] * x.[244] // ba + a + caB | catalytic ligation: ba + a + caB <-> baa + caB
            2372.62078331855 * x.[210] * x.[121] // baa + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            -2372.62078331855 * x.[36] * x.[6] * x.[121] // ba + a + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            -60.8364303415014 * x.[30] * x.[6] * x.[244] // aa + a + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            -2372.62078331855 * x.[30] * x.[6] * x.[121] // aa + a + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            60.8364303415014 * x.[173] * x.[244] // aaC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            -60.8364303415014 * x.[6] * x.[29] * x.[244] // a + aC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            2372.62078331855 * x.[173] * x.[121] // aaC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            -2372.62078331855 * x.[6] * x.[29] * x.[121] // a + aC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            60.8364303415014 * x.[172] * x.[244] // aaB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            -60.8364303415014 * x.[6] * x.[28] * x.[244] // a + aB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            2372.62078331855 * x.[172] * x.[121] // aaB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            -2372.62078331855 * x.[6] * x.[28] * x.[121] // a + aB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            60.8364303415014 * x.[171] * x.[244] // aaA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            -60.8364303415014 * x.[6] * x.[27] * x.[244] // a + aA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            2372.62078331855 * x.[171] * x.[121] // aaA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            -2372.62078331855 * x.[6] * x.[27] * x.[121] // a + aA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            60.8364303415014 * x.[176] * x.[244] // aac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            -60.8364303415014 * x.[6] * x.[32] * x.[244] // a + ac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            2372.62078331855 * x.[176] * x.[121] // aac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            -2372.62078331855 * x.[6] * x.[32] * x.[121] // a + ac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            -60.8364303415014 * x.[6] * x.[31] * x.[244] // a + ab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            -2372.62078331855 * x.[6] * x.[31] * x.[121] // a + ab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            -60.8364303415014 * x.[6] * x.[30] * x.[244] // a + aa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            -2372.62078331855 * x.[6] * x.[30] * x.[121] // a + aa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            60.8364303415014 * x.[30] * x.[244] // aa + caB | catalytic ligation: a + a + caB <-> aa + caB
            60.8364303415014 * x.[30] * x.[244] // aa + caB | catalytic ligation: a + a + caB <-> aa + caB
            -60.8364303415014 * x.[6] * x.[6] * x.[244] // a + a + caB | catalytic ligation: a + a + caB <-> aa + caB
            -60.8364303415014 * x.[6] * x.[6] * x.[244] // a + a + caB | catalytic ligation: a + a + caB <-> aa + caB
            2372.62078331855 * x.[30] * x.[121] // aa + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            2372.62078331855 * x.[30] * x.[121] // aa + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            -2372.62078331855 * x.[6] * x.[6] * x.[121] // a + a + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            -2372.62078331855 * x.[6] * x.[6] * x.[121] // a + a + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            60.8364303415014 * x.[179] * x.[244] // abC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            -60.8364303415014 * x.[6] * x.[35] * x.[244] // a + bC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            2372.62078331855 * x.[179] * x.[121] // abC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            -2372.62078331855 * x.[6] * x.[35] * x.[121] // a + bC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            60.8364303415014 * x.[178] * x.[244] // abB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            -60.8364303415014 * x.[6] * x.[34] * x.[244] // a + bB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            2372.62078331855 * x.[178] * x.[121] // abB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            -2372.62078331855 * x.[6] * x.[34] * x.[121] // a + bB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            60.8364303415014 * x.[177] * x.[244] // abA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            -60.8364303415014 * x.[6] * x.[33] * x.[244] // a + bA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            2372.62078331855 * x.[177] * x.[121] // abA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            -2372.62078331855 * x.[6] * x.[33] * x.[121] // a + bA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            60.8364303415014 * x.[182] * x.[244] // abc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            -60.8364303415014 * x.[6] * x.[38] * x.[244] // a + bc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            2372.62078331855 * x.[182] * x.[121] // abc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            -2372.62078331855 * x.[6] * x.[38] * x.[121] // a + bc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            60.8364303415014 * x.[181] * x.[244] // abb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            -60.8364303415014 * x.[6] * x.[37] * x.[244] // a + bb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            2372.62078331855 * x.[181] * x.[121] // abb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            -2372.62078331855 * x.[6] * x.[37] * x.[121] // a + bb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            60.8364303415014 * x.[180] * x.[244] // aba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            -60.8364303415014 * x.[6] * x.[36] * x.[244] // a + ba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            2372.62078331855 * x.[180] * x.[121] // aba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            -2372.62078331855 * x.[6] * x.[36] * x.[121] // a + ba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            60.8364303415014 * x.[31] * x.[244] // ab + caB | catalytic ligation: a + b + caB <-> ab + caB
            -60.8364303415014 * x.[6] * x.[7] * x.[244] // a + b + caB | catalytic ligation: a + b + caB <-> ab + caB
            2372.62078331855 * x.[31] * x.[121] // ab + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            -2372.62078331855 * x.[6] * x.[7] * x.[121] // a + b + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            1.0 * x.[150] // Cca | ligation: Cc + a <-> Cca
            -1.0 * x.[26] * x.[6] // Cc + a | ligation: Cc + a <-> Cca
            1.0 * x.[240] // cCa | ligation: cC + a <-> cCa
            -1.0 * x.[41] * x.[6] // cC + a | ligation: cC + a <-> cCa
            1.0 * x.[144] // Cba | ligation: Cb + a <-> Cba
            -1.0 * x.[25] * x.[6] // Cb + a | ligation: Cb + a <-> Cba
            1.0 * x.[234] // cBa | ligation: cB + a <-> cBa
            -1.0 * x.[40] * x.[6] // cB + a | ligation: cB + a <-> cBa
            1.0 * x.[138] // Caa | ligation: Ca + a <-> Caa
            -1.0 * x.[24] * x.[6] // Ca + a | ligation: Ca + a <-> Caa
            1.0 * x.[228] // cAa | ligation: cA + a <-> cAa
            -1.0 * x.[39] * x.[6] // cA + a | ligation: cA + a <-> cAa
            1.0 * x.[132] // CCa | ligation: CC + a <-> CCa
            -1.0 * x.[23] * x.[6] // CC + a | ligation: CC + a <-> CCa
            1.0 * x.[258] // cca | ligation: cc + a <-> cca
            -1.0 * x.[44] * x.[6] // cc + a | ligation: cc + a <-> cca
            1.0 * x.[126] // CBa | ligation: CB + a <-> CBa
            -1.0 * x.[22] * x.[6] // CB + a | ligation: CB + a <-> CBa
            1.0 * x.[252] // cba | ligation: cb + a <-> cba
            -1.0 * x.[43] * x.[6] // cb + a | ligation: cb + a <-> cba
            1.0 * x.[120] // CAa | ligation: CA + a <-> CAa
            -1.0 * x.[21] * x.[6] // CA + a | ligation: CA + a <-> CAa
            1.0 * x.[246] // caa | ligation: ca + a <-> caa
            -1.0 * x.[42] * x.[6] // ca + a | ligation: ca + a <-> caa
            1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
            1.0 * x.[42] // ca | ligation: c + a <-> ca
            -1.0 * x.[8] * x.[6] // c + a | ligation: c + a <-> ca
            1.0 * x.[114] // Bca | ligation: Bc + a <-> Bca
            -1.0 * x.[20] * x.[6] // Bc + a | ligation: Bc + a <-> Bca
            1.0 * x.[204] // bCa | ligation: bC + a <-> bCa
            -1.0 * x.[35] * x.[6] // bC + a | ligation: bC + a <-> bCa
            1.0 * x.[108] // Bba | ligation: Bb + a <-> Bba
            -1.0 * x.[19] * x.[6] // Bb + a | ligation: Bb + a <-> Bba
            1.0 * x.[198] // bBa | ligation: bB + a <-> bBa
            -1.0 * x.[34] * x.[6] // bB + a | ligation: bB + a <-> bBa
            1.0 * x.[102] // Baa | ligation: Ba + a <-> Baa
            -1.0 * x.[18] * x.[6] // Ba + a | ligation: Ba + a <-> Baa
            1.0 * x.[192] // bAa | ligation: bA + a <-> bAa
            -1.0 * x.[33] * x.[6] // bA + a | ligation: bA + a <-> bAa
            1.0 * x.[96] // BCa | ligation: BC + a <-> BCa
            -1.0 * x.[17] * x.[6] // BC + a | ligation: BC + a <-> BCa
            1.0 * x.[222] // bca | ligation: bc + a <-> bca
            -1.0 * x.[38] * x.[6] // bc + a | ligation: bc + a <-> bca
            1.0 * x.[90] // BBa | ligation: BB + a <-> BBa
            -1.0 * x.[16] * x.[6] // BB + a | ligation: BB + a <-> BBa
            1.0 * x.[216] // bba | ligation: bb + a <-> bba
            -1.0 * x.[37] * x.[6] // bb + a | ligation: bb + a <-> bba
            1.0 * x.[84] // BAa | ligation: BA + a <-> BAa
            -1.0 * x.[15] * x.[6] // BA + a | ligation: BA + a <-> BAa
            1.0 * x.[210] // baa | ligation: ba + a <-> baa
            -1.0 * x.[36] * x.[6] // ba + a | ligation: ba + a <-> baa
            1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
            1.0 * x.[36] // ba | ligation: b + a <-> ba
            -1.0 * x.[7] * x.[6] // b + a | ligation: b + a <-> ba
            1.0 * x.[78] // Aca | ligation: Ac + a <-> Aca
            -1.0 * x.[14] * x.[6] // Ac + a | ligation: Ac + a <-> Aca
            1.0 * x.[168] // aCa | ligation: aC + a <-> aCa
            -1.0 * x.[29] * x.[6] // aC + a | ligation: aC + a <-> aCa
            1.0 * x.[72] // Aba | ligation: Ab + a <-> Aba
            -1.0 * x.[13] * x.[6] // Ab + a | ligation: Ab + a <-> Aba
            1.0 * x.[162] // aBa | ligation: aB + a <-> aBa
            -1.0 * x.[28] * x.[6] // aB + a | ligation: aB + a <-> aBa
            1.0 * x.[66] // Aaa | ligation: Aa + a <-> Aaa
            -1.0 * x.[12] * x.[6] // Aa + a | ligation: Aa + a <-> Aaa
            1.0 * x.[156] // aAa | ligation: aA + a <-> aAa
            -1.0 * x.[27] * x.[6] // aA + a | ligation: aA + a <-> aAa
            1.0 * x.[60] // ACa | ligation: AC + a <-> ACa
            -1.0 * x.[11] * x.[6] // AC + a | ligation: AC + a <-> ACa
            1.0 * x.[186] // aca | ligation: ac + a <-> aca
            -1.0 * x.[32] * x.[6] // ac + a | ligation: ac + a <-> aca
            1.0 * x.[54] // ABa | ligation: AB + a <-> ABa
            -1.0 * x.[10] * x.[6] // AB + a | ligation: AB + a <-> ABa
            1.0 * x.[180] // aba | ligation: ab + a <-> aba
            -1.0 * x.[31] * x.[6] // ab + a | ligation: ab + a <-> aba
            1.0 * x.[48] // AAa | ligation: AA + a <-> AAa
            -1.0 * x.[9] * x.[6] // AA + a | ligation: AA + a <-> AAa
            1.0 * x.[174] // aaa | ligation: aa + a <-> aaa
            -1.0 * x.[30] * x.[6] // aa + a | ligation: aa + a <-> aaa
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
            1.0 * x.[29] // aC | ligation: a + C <-> aC
            -1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
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
            1.0 * x.[28] // aB | ligation: a + B <-> aB
            -1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
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
            1.0 * x.[27] // aA | ligation: a + A <-> aA
            -1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
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
            1.0 * x.[32] // ac | ligation: a + c <-> ac
            -1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
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
            1.0 * x.[31] // ab | ligation: a + b <-> ab
            -1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
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
            1.0 * x.[30] // aa | ligation: a + a <-> aa
            1.0 * x.[30] // aa | ligation: a + a <-> aa
            -1.0 * x.[6] * x.[6] // a + a | ligation: a + a <-> aa
            -1.0 * x.[6] * x.[6] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 7 - b
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[221] * x.[103] // bcC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            -2214.00742039413 * x.[7] * x.[41] * x.[103] // b + cC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            56.7694210357472 * x.[221] * x.[190] // bcC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            -56.7694210357472 * x.[7] * x.[41] * x.[190] // b + cC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            2214.00742039413 * x.[220] * x.[103] // bcB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            -2214.00742039413 * x.[7] * x.[40] * x.[103] // b + cB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            56.7694210357472 * x.[220] * x.[190] // bcB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            -56.7694210357472 * x.[7] * x.[40] * x.[190] // b + cB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            2214.00742039413 * x.[219] * x.[103] // bcA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            -2214.00742039413 * x.[7] * x.[39] * x.[103] // b + cA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            56.7694210357472 * x.[219] * x.[190] // bcA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            -56.7694210357472 * x.[7] * x.[39] * x.[190] // b + cA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            -2214.00742039413 * x.[7] * x.[44] * x.[103] // b + cc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            -56.7694210357472 * x.[7] * x.[44] * x.[190] // b + cc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            2214.00742039413 * x.[223] * x.[103] // bcb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            -2214.00742039413 * x.[7] * x.[43] * x.[103] // b + cb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            56.7694210357472 * x.[223] * x.[190] // bcb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            -56.7694210357472 * x.[7] * x.[43] * x.[190] // b + cb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            2214.00742039413 * x.[222] * x.[103] // bca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            -2214.00742039413 * x.[7] * x.[42] * x.[103] // b + ca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            56.7694210357472 * x.[222] * x.[190] // bca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            -56.7694210357472 * x.[7] * x.[42] * x.[190] // b + ca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            2214.00742039413 * x.[38] * x.[103] // bc + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            -2214.00742039413 * x.[7] * x.[8] * x.[103] // b + c + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            56.7694210357472 * x.[38] * x.[190] // bc + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            -56.7694210357472 * x.[7] * x.[8] * x.[190] // b + c + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            60.8364303415014 * x.[247] * x.[244] // cab + caB | catalytic ligation: ca + b + caB <-> cab + caB
            -60.8364303415014 * x.[42] * x.[7] * x.[244] // ca + b + caB | catalytic ligation: ca + b + caB <-> cab + caB
            2372.62078331855 * x.[247] * x.[121] // cab + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            -2372.62078331855 * x.[42] * x.[7] * x.[121] // ca + b + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            60.8364303415014 * x.[211] * x.[244] // bab + caB | catalytic ligation: ba + b + caB <-> bab + caB
            -60.8364303415014 * x.[36] * x.[7] * x.[244] // ba + b + caB | catalytic ligation: ba + b + caB <-> bab + caB
            2372.62078331855 * x.[211] * x.[121] // bab + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            -2372.62078331855 * x.[36] * x.[7] * x.[121] // ba + b + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: aa + b + caB <-> aab + caB
            -60.8364303415014 * x.[30] * x.[7] * x.[244] // aa + b + caB | catalytic ligation: aa + b + caB <-> aab + caB
            2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            -2372.62078331855 * x.[30] * x.[7] * x.[121] // aa + b + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            60.8364303415014 * x.[31] * x.[244] // ab + caB | catalytic ligation: a + b + caB <-> ab + caB
            -60.8364303415014 * x.[6] * x.[7] * x.[244] // a + b + caB | catalytic ligation: a + b + caB <-> ab + caB
            2372.62078331855 * x.[31] * x.[121] // ab + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            -2372.62078331855 * x.[6] * x.[7] * x.[121] // a + b + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            60.8364303415014 * x.[223] * x.[244] // bcb + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            -60.8364303415014 * x.[38] * x.[7] * x.[244] // bc + b + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            2372.62078331855 * x.[223] * x.[121] // bcb + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            -2372.62078331855 * x.[38] * x.[7] * x.[121] // bc + b + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            60.8364303415014 * x.[187] * x.[244] // acb + caB | catalytic ligation: ac + b + caB <-> acb + caB
            -60.8364303415014 * x.[32] * x.[7] * x.[244] // ac + b + caB | catalytic ligation: ac + b + caB <-> acb + caB
            2372.62078331855 * x.[187] * x.[121] // acb + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            -2372.62078331855 * x.[32] * x.[7] * x.[121] // ac + b + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            60.8364303415014 * x.[43] * x.[244] // cb + caB | catalytic ligation: c + b + caB <-> cb + caB
            -60.8364303415014 * x.[8] * x.[7] * x.[244] // c + b + caB | catalytic ligation: c + b + caB <-> cb + caB
            2372.62078331855 * x.[43] * x.[121] // cb + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            -2372.62078331855 * x.[8] * x.[7] * x.[121] // c + b + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            60.8364303415013 * x.[259] * x.[244] // ccb + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            -60.8364303415013 * x.[44] * x.[7] * x.[244] // cc + b + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            2372.62078331855 * x.[259] * x.[121] // ccb + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            -2372.62078331855 * x.[44] * x.[7] * x.[121] // cc + b + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            1.0 * x.[151] // Ccb | ligation: Cc + b <-> Ccb
            -1.0 * x.[26] * x.[7] // Cc + b | ligation: Cc + b <-> Ccb
            1.0 * x.[241] // cCb | ligation: cC + b <-> cCb
            -1.0 * x.[41] * x.[7] // cC + b | ligation: cC + b <-> cCb
            1.0 * x.[145] // Cbb | ligation: Cb + b <-> Cbb
            -1.0 * x.[25] * x.[7] // Cb + b | ligation: Cb + b <-> Cbb
            1.0 * x.[235] // cBb | ligation: cB + b <-> cBb
            -1.0 * x.[40] * x.[7] // cB + b | ligation: cB + b <-> cBb
            1.0 * x.[139] // Cab | ligation: Ca + b <-> Cab
            -1.0 * x.[24] * x.[7] // Ca + b | ligation: Ca + b <-> Cab
            1.0 * x.[229] // cAb | ligation: cA + b <-> cAb
            -1.0 * x.[39] * x.[7] // cA + b | ligation: cA + b <-> cAb
            1.0 * x.[133] // CCb | ligation: CC + b <-> CCb
            -1.0 * x.[23] * x.[7] // CC + b | ligation: CC + b <-> CCb
            1.0 * x.[259] // ccb | ligation: cc + b <-> ccb
            -1.0 * x.[44] * x.[7] // cc + b | ligation: cc + b <-> ccb
            1.0 * x.[127] // CBb | ligation: CB + b <-> CBb
            -1.0 * x.[22] * x.[7] // CB + b | ligation: CB + b <-> CBb
            1.0 * x.[253] // cbb | ligation: cb + b <-> cbb
            -1.0 * x.[43] * x.[7] // cb + b | ligation: cb + b <-> cbb
            1.0 * x.[121] // CAb | ligation: CA + b <-> CAb
            -1.0 * x.[21] * x.[7] // CA + b | ligation: CA + b <-> CAb
            1.0 * x.[247] // cab | ligation: ca + b <-> cab
            -1.0 * x.[42] * x.[7] // ca + b | ligation: ca + b <-> cab
            1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
            1.0 * x.[43] // cb | ligation: c + b <-> cb
            -1.0 * x.[8] * x.[7] // c + b | ligation: c + b <-> cb
            1.0 * x.[115] // Bcb | ligation: Bc + b <-> Bcb
            -1.0 * x.[20] * x.[7] // Bc + b | ligation: Bc + b <-> Bcb
            1.0 * x.[205] // bCb | ligation: bC + b <-> bCb
            -1.0 * x.[35] * x.[7] // bC + b | ligation: bC + b <-> bCb
            1.0 * x.[109] // Bbb | ligation: Bb + b <-> Bbb
            -1.0 * x.[19] * x.[7] // Bb + b | ligation: Bb + b <-> Bbb
            1.0 * x.[199] // bBb | ligation: bB + b <-> bBb
            -1.0 * x.[34] * x.[7] // bB + b | ligation: bB + b <-> bBb
            1.0 * x.[103] // Bab | ligation: Ba + b <-> Bab
            -1.0 * x.[18] * x.[7] // Ba + b | ligation: Ba + b <-> Bab
            1.0 * x.[193] // bAb | ligation: bA + b <-> bAb
            -1.0 * x.[33] * x.[7] // bA + b | ligation: bA + b <-> bAb
            1.0 * x.[97] // BCb | ligation: BC + b <-> BCb
            -1.0 * x.[17] * x.[7] // BC + b | ligation: BC + b <-> BCb
            1.0 * x.[223] // bcb | ligation: bc + b <-> bcb
            -1.0 * x.[38] * x.[7] // bc + b | ligation: bc + b <-> bcb
            1.0 * x.[91] // BBb | ligation: BB + b <-> BBb
            -1.0 * x.[16] * x.[7] // BB + b | ligation: BB + b <-> BBb
            1.0 * x.[217] // bbb | ligation: bb + b <-> bbb
            -1.0 * x.[37] * x.[7] // bb + b | ligation: bb + b <-> bbb
            1.0 * x.[85] // BAb | ligation: BA + b <-> BAb
            -1.0 * x.[15] * x.[7] // BA + b | ligation: BA + b <-> BAb
            1.0 * x.[211] // bab | ligation: ba + b <-> bab
            -1.0 * x.[36] * x.[7] // ba + b | ligation: ba + b <-> bab
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
            1.0 * x.[35] // bC | ligation: b + C <-> bC
            -1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
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
            1.0 * x.[34] // bB | ligation: b + B <-> bB
            -1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
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
            1.0 * x.[33] // bA | ligation: b + A <-> bA
            -1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
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
            1.0 * x.[38] // bc | ligation: b + c <-> bc
            -1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
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
            1.0 * x.[37] // bb | ligation: b + b <-> bb
            1.0 * x.[37] // bb | ligation: b + b <-> bb
            -1.0 * x.[7] * x.[7] // b + b | ligation: b + b <-> bb
            -1.0 * x.[7] * x.[7] // b + b | ligation: b + b <-> bb
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
            1.0 * x.[36] // ba | ligation: b + a <-> ba
            -1.0 * x.[7] * x.[6] // b + a | ligation: b + a <-> ba
            1.0 * x.[79] // Acb | ligation: Ac + b <-> Acb
            -1.0 * x.[14] * x.[7] // Ac + b | ligation: Ac + b <-> Acb
            1.0 * x.[169] // aCb | ligation: aC + b <-> aCb
            -1.0 * x.[29] * x.[7] // aC + b | ligation: aC + b <-> aCb
            1.0 * x.[73] // Abb | ligation: Ab + b <-> Abb
            -1.0 * x.[13] * x.[7] // Ab + b | ligation: Ab + b <-> Abb
            1.0 * x.[163] // aBb | ligation: aB + b <-> aBb
            -1.0 * x.[28] * x.[7] // aB + b | ligation: aB + b <-> aBb
            1.0 * x.[67] // Aab | ligation: Aa + b <-> Aab
            -1.0 * x.[12] * x.[7] // Aa + b | ligation: Aa + b <-> Aab
            1.0 * x.[157] // aAb | ligation: aA + b <-> aAb
            -1.0 * x.[27] * x.[7] // aA + b | ligation: aA + b <-> aAb
            1.0 * x.[61] // ACb | ligation: AC + b <-> ACb
            -1.0 * x.[11] * x.[7] // AC + b | ligation: AC + b <-> ACb
            1.0 * x.[187] // acb | ligation: ac + b <-> acb
            -1.0 * x.[32] * x.[7] // ac + b | ligation: ac + b <-> acb
            1.0 * x.[55] // ABb | ligation: AB + b <-> ABb
            -1.0 * x.[10] * x.[7] // AB + b | ligation: AB + b <-> ABb
            1.0 * x.[181] // abb | ligation: ab + b <-> abb
            -1.0 * x.[31] * x.[7] // ab + b | ligation: ab + b <-> abb
            1.0 * x.[49] // AAb | ligation: AA + b <-> AAb
            -1.0 * x.[9] * x.[7] // AA + b | ligation: AA + b <-> AAb
            1.0 * x.[175] // aab | ligation: aa + b <-> aab
            -1.0 * x.[30] * x.[7] // aa + b | ligation: aa + b <-> aab
            1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
            1.0 * x.[31] // ab | ligation: a + b <-> ab
            -1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 8 - c
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            -2214.00742039413 * x.[44] * x.[8] * x.[103] // cc + c + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            -56.7694210357472 * x.[44] * x.[8] * x.[190] // cc + c + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            -2214.00742039413 * x.[38] * x.[8] * x.[103] // bc + c + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            -56.7694210357472 * x.[38] * x.[8] * x.[190] // bc + c + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            2214.00742039413 * x.[188] * x.[103] // acc + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            -2214.00742039413 * x.[32] * x.[8] * x.[103] // ac + c + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            56.7694210357472 * x.[188] * x.[190] // acc + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            -56.7694210357472 * x.[32] * x.[8] * x.[190] // ac + c + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            2214.00742039413 * x.[257] * x.[103] // ccC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            -2214.00742039413 * x.[8] * x.[41] * x.[103] // c + cC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            56.7694210357472 * x.[257] * x.[190] // ccC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            -56.7694210357472 * x.[8] * x.[41] * x.[190] // c + cC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            2214.00742039413 * x.[256] * x.[103] // ccB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            -2214.00742039413 * x.[8] * x.[40] * x.[103] // c + cB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            56.7694210357472 * x.[256] * x.[190] // ccB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            -56.7694210357472 * x.[8] * x.[40] * x.[190] // c + cB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            2214.00742039413 * x.[255] * x.[103] // ccA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            -2214.00742039413 * x.[8] * x.[39] * x.[103] // c + cA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            56.7694210357472 * x.[255] * x.[190] // ccA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            -56.7694210357472 * x.[8] * x.[39] * x.[190] // c + cA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            -2214.00742039413 * x.[8] * x.[44] * x.[103] // c + cc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            -56.7694210357472 * x.[8] * x.[44] * x.[190] // c + cc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            2214.00742039413 * x.[259] * x.[103] // ccb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            -2214.00742039413 * x.[8] * x.[43] * x.[103] // c + cb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            56.7694210357472 * x.[259] * x.[190] // ccb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            -56.7694210357472 * x.[8] * x.[43] * x.[190] // c + cb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            2214.00742039413 * x.[258] * x.[103] // cca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            -2214.00742039413 * x.[8] * x.[42] * x.[103] // c + ca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            56.7694210357472 * x.[258] * x.[190] // cca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            -56.7694210357472 * x.[8] * x.[42] * x.[190] // c + ca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            2214.00742039413 * x.[44] * x.[103] // cc + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            2214.00742039413 * x.[44] * x.[103] // cc + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            -2214.00742039413 * x.[8] * x.[8] * x.[103] // c + c + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            -2214.00742039413 * x.[8] * x.[8] * x.[103] // c + c + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            56.7694210357472 * x.[44] * x.[190] // cc + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            56.7694210357472 * x.[44] * x.[190] // cc + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            -56.7694210357472 * x.[8] * x.[8] * x.[190] // c + c + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            -56.7694210357472 * x.[8] * x.[8] * x.[190] // c + c + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            2214.00742039413 * x.[254] * x.[103] // cbc + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            -2214.00742039413 * x.[43] * x.[8] * x.[103] // cb + c + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            56.7694210357472 * x.[254] * x.[190] // cbc + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            -56.7694210357472 * x.[43] * x.[8] * x.[190] // cb + c + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            2214.00742039413 * x.[218] * x.[103] // bbc + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            -2214.00742039413 * x.[37] * x.[8] * x.[103] // bb + c + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            56.7694210357472 * x.[218] * x.[190] // bbc + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            -56.7694210357472 * x.[37] * x.[8] * x.[190] // bb + c + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            2214.00742039413 * x.[182] * x.[103] // abc + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            -2214.00742039413 * x.[31] * x.[8] * x.[103] // ab + c + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            56.7694210357472 * x.[182] * x.[190] // abc + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            -56.7694210357472 * x.[31] * x.[8] * x.[190] // ab + c + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            2214.00742039413 * x.[38] * x.[103] // bc + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            -2214.00742039413 * x.[7] * x.[8] * x.[103] // b + c + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            56.7694210357472 * x.[38] * x.[190] // bc + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            -56.7694210357472 * x.[7] * x.[8] * x.[190] // b + c + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            60.8364303415014 * x.[251] * x.[244] // cbC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            -60.8364303415014 * x.[8] * x.[35] * x.[244] // c + bC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            2372.62078331855 * x.[251] * x.[121] // cbC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            -2372.62078331855 * x.[8] * x.[35] * x.[121] // c + bC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            60.8364303415014 * x.[250] * x.[244] // cbB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            -60.8364303415014 * x.[8] * x.[34] * x.[244] // c + bB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            2372.62078331855 * x.[250] * x.[121] // cbB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            -2372.62078331855 * x.[8] * x.[34] * x.[121] // c + bB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            60.8364303415014 * x.[249] * x.[244] // cbA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            -60.8364303415014 * x.[8] * x.[33] * x.[244] // c + bA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            2372.62078331855 * x.[249] * x.[121] // cbA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            -2372.62078331855 * x.[8] * x.[33] * x.[121] // c + bA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            60.8364303415014 * x.[254] * x.[244] // cbc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            -60.8364303415014 * x.[8] * x.[38] * x.[244] // c + bc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            2372.62078331855 * x.[254] * x.[121] // cbc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            -2372.62078331855 * x.[8] * x.[38] * x.[121] // c + bc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            60.8364303415014 * x.[253] * x.[244] // cbb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            -60.8364303415014 * x.[8] * x.[37] * x.[244] // c + bb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            2372.62078331855 * x.[253] * x.[121] // cbb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            -2372.62078331855 * x.[8] * x.[37] * x.[121] // c + bb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            60.8364303415014 * x.[252] * x.[244] // cba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            -60.8364303415014 * x.[8] * x.[36] * x.[244] // c + ba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            2372.62078331855 * x.[252] * x.[121] // cba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            -2372.62078331855 * x.[8] * x.[36] * x.[121] // c + ba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            60.8364303415014 * x.[43] * x.[244] // cb + caB | catalytic ligation: c + b + caB <-> cb + caB
            -60.8364303415014 * x.[8] * x.[7] * x.[244] // c + b + caB | catalytic ligation: c + b + caB <-> cb + caB
            2372.62078331855 * x.[43] * x.[121] // cb + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            -2372.62078331855 * x.[8] * x.[7] * x.[121] // c + b + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            1.0 * x.[152] // Ccc | ligation: Cc + c <-> Ccc
            -1.0 * x.[26] * x.[8] // Cc + c | ligation: Cc + c <-> Ccc
            1.0 * x.[242] // cCc | ligation: cC + c <-> cCc
            -1.0 * x.[41] * x.[8] // cC + c | ligation: cC + c <-> cCc
            1.0 * x.[146] // Cbc | ligation: Cb + c <-> Cbc
            -1.0 * x.[25] * x.[8] // Cb + c | ligation: Cb + c <-> Cbc
            1.0 * x.[236] // cBc | ligation: cB + c <-> cBc
            -1.0 * x.[40] * x.[8] // cB + c | ligation: cB + c <-> cBc
            1.0 * x.[140] // Cac | ligation: Ca + c <-> Cac
            -1.0 * x.[24] * x.[8] // Ca + c | ligation: Ca + c <-> Cac
            1.0 * x.[230] // cAc | ligation: cA + c <-> cAc
            -1.0 * x.[39] * x.[8] // cA + c | ligation: cA + c <-> cAc
            1.0 * x.[134] // CCc | ligation: CC + c <-> CCc
            -1.0 * x.[23] * x.[8] // CC + c | ligation: CC + c <-> CCc
            1.0 * x.[260] // ccc | ligation: cc + c <-> ccc
            -1.0 * x.[44] * x.[8] // cc + c | ligation: cc + c <-> ccc
            1.0 * x.[128] // CBc | ligation: CB + c <-> CBc
            -1.0 * x.[22] * x.[8] // CB + c | ligation: CB + c <-> CBc
            1.0 * x.[254] // cbc | ligation: cb + c <-> cbc
            -1.0 * x.[43] * x.[8] // cb + c | ligation: cb + c <-> cbc
            1.0 * x.[122] // CAc | ligation: CA + c <-> CAc
            -1.0 * x.[21] * x.[8] // CA + c | ligation: CA + c <-> CAc
            1.0 * x.[248] // cac | ligation: ca + c <-> cac
            -1.0 * x.[42] * x.[8] // ca + c | ligation: ca + c <-> cac
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
            1.0 * x.[41] // cC | ligation: c + C <-> cC
            -1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
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
            1.0 * x.[40] // cB | ligation: c + B <-> cB
            -1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
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
            1.0 * x.[39] // cA | ligation: c + A <-> cA
            -1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
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
            1.0 * x.[44] // cc | ligation: c + c <-> cc
            1.0 * x.[44] // cc | ligation: c + c <-> cc
            -1.0 * x.[8] * x.[8] // c + c | ligation: c + c <-> cc
            -1.0 * x.[8] * x.[8] // c + c | ligation: c + c <-> cc
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
            1.0 * x.[43] // cb | ligation: c + b <-> cb
            -1.0 * x.[8] * x.[7] // c + b | ligation: c + b <-> cb
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
            1.0 * x.[42] // ca | ligation: c + a <-> ca
            -1.0 * x.[8] * x.[6] // c + a | ligation: c + a <-> ca
            1.0 * x.[116] // Bcc | ligation: Bc + c <-> Bcc
            -1.0 * x.[20] * x.[8] // Bc + c | ligation: Bc + c <-> Bcc
            1.0 * x.[206] // bCc | ligation: bC + c <-> bCc
            -1.0 * x.[35] * x.[8] // bC + c | ligation: bC + c <-> bCc
            1.0 * x.[110] // Bbc | ligation: Bb + c <-> Bbc
            -1.0 * x.[19] * x.[8] // Bb + c | ligation: Bb + c <-> Bbc
            1.0 * x.[200] // bBc | ligation: bB + c <-> bBc
            -1.0 * x.[34] * x.[8] // bB + c | ligation: bB + c <-> bBc
            1.0 * x.[104] // Bac | ligation: Ba + c <-> Bac
            -1.0 * x.[18] * x.[8] // Ba + c | ligation: Ba + c <-> Bac
            1.0 * x.[194] // bAc | ligation: bA + c <-> bAc
            -1.0 * x.[33] * x.[8] // bA + c | ligation: bA + c <-> bAc
            1.0 * x.[98] // BCc | ligation: BC + c <-> BCc
            -1.0 * x.[17] * x.[8] // BC + c | ligation: BC + c <-> BCc
            1.0 * x.[224] // bcc | ligation: bc + c <-> bcc
            -1.0 * x.[38] * x.[8] // bc + c | ligation: bc + c <-> bcc
            1.0 * x.[92] // BBc | ligation: BB + c <-> BBc
            -1.0 * x.[16] * x.[8] // BB + c | ligation: BB + c <-> BBc
            1.0 * x.[218] // bbc | ligation: bb + c <-> bbc
            -1.0 * x.[37] * x.[8] // bb + c | ligation: bb + c <-> bbc
            1.0 * x.[86] // BAc | ligation: BA + c <-> BAc
            -1.0 * x.[15] * x.[8] // BA + c | ligation: BA + c <-> BAc
            1.0 * x.[212] // bac | ligation: ba + c <-> bac
            -1.0 * x.[36] * x.[8] // ba + c | ligation: ba + c <-> bac
            1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
            1.0 * x.[38] // bc | ligation: b + c <-> bc
            -1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
            1.0 * x.[80] // Acc | ligation: Ac + c <-> Acc
            -1.0 * x.[14] * x.[8] // Ac + c | ligation: Ac + c <-> Acc
            1.0 * x.[170] // aCc | ligation: aC + c <-> aCc
            -1.0 * x.[29] * x.[8] // aC + c | ligation: aC + c <-> aCc
            1.0 * x.[74] // Abc | ligation: Ab + c <-> Abc
            -1.0 * x.[13] * x.[8] // Ab + c | ligation: Ab + c <-> Abc
            1.0 * x.[164] // aBc | ligation: aB + c <-> aBc
            -1.0 * x.[28] * x.[8] // aB + c | ligation: aB + c <-> aBc
            1.0 * x.[68] // Aac | ligation: Aa + c <-> Aac
            -1.0 * x.[12] * x.[8] // Aa + c | ligation: Aa + c <-> Aac
            1.0 * x.[158] // aAc | ligation: aA + c <-> aAc
            -1.0 * x.[27] * x.[8] // aA + c | ligation: aA + c <-> aAc
            1.0 * x.[62] // ACc | ligation: AC + c <-> ACc
            -1.0 * x.[11] * x.[8] // AC + c | ligation: AC + c <-> ACc
            1.0 * x.[188] // acc | ligation: ac + c <-> acc
            -1.0 * x.[32] * x.[8] // ac + c | ligation: ac + c <-> acc
            1.0 * x.[56] // ABc | ligation: AB + c <-> ABc
            -1.0 * x.[10] * x.[8] // AB + c | ligation: AB + c <-> ABc
            1.0 * x.[182] // abc | ligation: ab + c <-> abc
            -1.0 * x.[31] * x.[8] // ab + c | ligation: ab + c <-> abc
            1.0 * x.[50] // AAc | ligation: AA + c <-> AAc
            -1.0 * x.[9] * x.[8] // AA + c | ligation: AA + c <-> AAc
            1.0 * x.[176] // aac | ligation: aa + c <-> aac
            -1.0 * x.[30] * x.[8] // aa + c | ligation: aa + c <-> aac
            1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
            1.0 * x.[32] // ac | ligation: a + c <-> ac
            -1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
        |]
        |> Array.sum


    // 9 - AA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            -2214.00742039413 * x.[3] * x.[9] * x.[190] // A + AA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            56.7694210357472 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            -56.7694210357472 * x.[3] * x.[9] * x.[103] // A + AA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            -2214.00742039413 * x.[9] * x.[190] // AA + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            2214.00742039413 * x.[3] * x.[3] * x.[190] // A + A + bAB | catalytic ligation: A + A + bAB <-> AA + bAB
            -56.7694210357472 * x.[9] * x.[103] // AA + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            56.7694210357472 * x.[3] * x.[3] * x.[103] // A + A + Bab | catalytic ligation: A + A + Bab <-> AA + Bab
            2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            -2214.00742039413 * x.[9] * x.[3] * x.[190] // AA + A + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            56.769421035747 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            -56.769421035747 * x.[9] * x.[3] * x.[103] // AA + A + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            -60.8364303415014 * x.[9] * x.[3] * x.[121] // AA + A + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            -2372.62078331855 * x.[9] * x.[3] * x.[244] // AA + A + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            -60.8364303415014 * x.[3] * x.[9] * x.[121] // A + AA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            -2372.62078331855 * x.[3] * x.[9] * x.[244] // A + AA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            -60.8364303415014 * x.[9] * x.[121] // AA + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            60.8364303415014 * x.[3] * x.[3] * x.[121] // A + A + CAb | catalytic ligation: A + A + CAb <-> AA + CAb
            -2372.62078331855 * x.[9] * x.[244] // AA + caB | catalytic ligation: A + A + caB <-> AA + caB
            2372.62078331855 * x.[3] * x.[3] * x.[244] // A + A + caB | catalytic ligation: A + A + caB <-> AA + caB
            60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            -60.8364303415014 * x.[9] * x.[4] * x.[121] // AA + B + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            -2372.62078331855 * x.[9] * x.[4] * x.[244] // AA + B + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            1.0 * x.[225] // cAA | ligation: c + AA <-> cAA
            -1.0 * x.[8] * x.[9] // c + AA | ligation: c + AA <-> cAA
            1.0 * x.[117] // CAA | ligation: C + AA <-> CAA
            -1.0 * x.[5] * x.[9] // C + AA | ligation: C + AA <-> CAA
            1.0 * x.[189] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[7] * x.[9] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[81] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[9] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[50] // AAc | ligation: AA + c <-> AAc
            -1.0 * x.[9] * x.[8] // AA + c | ligation: AA + c <-> AAc
            1.0 * x.[49] // AAb | ligation: AA + b <-> AAb
            -1.0 * x.[9] * x.[7] // AA + b | ligation: AA + b <-> AAb
            1.0 * x.[48] // AAa | ligation: AA + a <-> AAa
            -1.0 * x.[9] * x.[6] // AA + a | ligation: AA + a <-> AAa
            1.0 * x.[47] // AAC | ligation: AA + C <-> AAC
            -1.0 * x.[9] * x.[5] // AA + C | ligation: AA + C <-> AAC
            1.0 * x.[46] // AAB | ligation: AA + B <-> AAB
            -1.0 * x.[9] * x.[4] // AA + B | ligation: AA + B <-> AAB
            1.0 * x.[45] // AAA | ligation: AA + A <-> AAA
            -1.0 * x.[9] * x.[3] // AA + A | ligation: AA + A <-> AAA
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
            2214.00742039413 * x.[46] * x.[190] // AAB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            -2214.00742039413 * x.[3] * x.[10] * x.[190] // A + AB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            56.7694210357472 * x.[46] * x.[103] // AAB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            -56.7694210357472 * x.[3] * x.[10] * x.[103] // A + AB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            2214.00742039413 * x.[53] * x.[190] // ABC + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            -2214.00742039413 * x.[10] * x.[5] * x.[190] // AB + C + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            56.7694210357472 * x.[53] * x.[103] // ABC + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            -56.7694210357472 * x.[10] * x.[5] * x.[103] // AB + C + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            -60.8364303415014 * x.[3] * x.[10] * x.[121] // A + AB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            -2372.62078331855 * x.[3] * x.[10] * x.[244] // A + AB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            -60.8364303415014 * x.[10] * x.[121] // AB + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            60.8364303415014 * x.[3] * x.[4] * x.[121] // A + B + CAb | catalytic ligation: A + B + CAb <-> AB + CAb
            -2372.62078331855 * x.[10] * x.[244] // AB + caB | catalytic ligation: A + B + caB <-> AB + caB
            2372.62078331855 * x.[3] * x.[4] * x.[244] // A + B + caB | catalytic ligation: A + B + caB <-> AB + caB
            1.0 * x.[226] // cAB | ligation: c + AB <-> cAB
            -1.0 * x.[8] * x.[10] // c + AB | ligation: c + AB <-> cAB
            1.0 * x.[118] // CAB | ligation: C + AB <-> CAB
            -1.0 * x.[5] * x.[10] // C + AB | ligation: C + AB <-> CAB
            1.0 * x.[190] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[7] * x.[10] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[82] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[10] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[56] // ABc | ligation: AB + c <-> ABc
            -1.0 * x.[10] * x.[8] // AB + c | ligation: AB + c <-> ABc
            1.0 * x.[55] // ABb | ligation: AB + b <-> ABb
            -1.0 * x.[10] * x.[7] // AB + b | ligation: AB + b <-> ABb
            1.0 * x.[54] // ABa | ligation: AB + a <-> ABa
            -1.0 * x.[10] * x.[6] // AB + a | ligation: AB + a <-> ABa
            1.0 * x.[53] // ABC | ligation: AB + C <-> ABC
            -1.0 * x.[10] * x.[5] // AB + C | ligation: AB + C <-> ABC
            1.0 * x.[52] // ABB | ligation: AB + B <-> ABB
            -1.0 * x.[10] * x.[4] // AB + B | ligation: AB + B <-> ABB
            1.0 * x.[51] // ABA | ligation: AB + A <-> ABA
            -1.0 * x.[10] * x.[3] // AB + A | ligation: AB + A <-> ABA
            1.0 * x.[154] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[6] * x.[10] // a + AB | ligation: a + AB <-> aAB
            -1.0 * x.[10] // AB | ligation: A + B <-> AB
            1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[46] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[10] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 11 - AC
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[47] * x.[190] // AAC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            -2214.00742039413 * x.[3] * x.[11] * x.[190] // A + AC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            56.7694210357472 * x.[47] * x.[103] // AAC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            -56.7694210357472 * x.[3] * x.[11] * x.[103] // A + AC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            2214.00742039413 * x.[59] * x.[190] // ACC + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            -2214.00742039413 * x.[11] * x.[5] * x.[190] // AC + C + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            56.7694210357472 * x.[59] * x.[103] // ACC + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            -56.7694210357472 * x.[11] * x.[5] * x.[103] // AC + C + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            60.8364303415014 * x.[47] * x.[121] // AAC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            -60.8364303415014 * x.[3] * x.[11] * x.[121] // A + AC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            2372.62078331855 * x.[47] * x.[244] // AAC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            -2372.62078331855 * x.[3] * x.[11] * x.[244] // A + AC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            60.8364303415014 * x.[58] * x.[121] // ACB + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            -60.8364303415014 * x.[11] * x.[4] * x.[121] // AC + B + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            2372.62078331855 * x.[58] * x.[244] // ACB + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            -2372.62078331855 * x.[11] * x.[4] * x.[244] // AC + B + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            1.0 * x.[227] // cAC | ligation: c + AC <-> cAC
            -1.0 * x.[8] * x.[11] // c + AC | ligation: c + AC <-> cAC
            1.0 * x.[119] // CAC | ligation: C + AC <-> CAC
            -1.0 * x.[5] * x.[11] // C + AC | ligation: C + AC <-> CAC
            1.0 * x.[191] // bAC | ligation: b + AC <-> bAC
            -1.0 * x.[7] * x.[11] // b + AC | ligation: b + AC <-> bAC
            1.0 * x.[83] // BAC | ligation: B + AC <-> BAC
            -1.0 * x.[4] * x.[11] // B + AC | ligation: B + AC <-> BAC
            1.0 * x.[62] // ACc | ligation: AC + c <-> ACc
            -1.0 * x.[11] * x.[8] // AC + c | ligation: AC + c <-> ACc
            1.0 * x.[61] // ACb | ligation: AC + b <-> ACb
            -1.0 * x.[11] * x.[7] // AC + b | ligation: AC + b <-> ACb
            1.0 * x.[60] // ACa | ligation: AC + a <-> ACa
            -1.0 * x.[11] * x.[6] // AC + a | ligation: AC + a <-> ACa
            1.0 * x.[59] // ACC | ligation: AC + C <-> ACC
            -1.0 * x.[11] * x.[5] // AC + C | ligation: AC + C <-> ACC
            1.0 * x.[58] // ACB | ligation: AC + B <-> ACB
            -1.0 * x.[11] * x.[4] // AC + B | ligation: AC + B <-> ACB
            1.0 * x.[57] // ACA | ligation: AC + A <-> ACA
            -1.0 * x.[11] * x.[3] // AC + A | ligation: AC + A <-> ACA
            1.0 * x.[155] // aAC | ligation: a + AC <-> aAC
            -1.0 * x.[6] * x.[11] // a + AC | ligation: a + AC <-> aAC
            -1.0 * x.[11] // AC | ligation: A + C <-> AC
            1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
            1.0 * x.[47] // AAC | ligation: A + AC <-> AAC
            -1.0 * x.[3] * x.[11] // A + AC | ligation: A + AC <-> AAC
        |]
        |> Array.sum


    // 12 - Aa
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[48] * x.[190] // AAa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            -2214.00742039413 * x.[3] * x.[12] * x.[190] // A + Aa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            56.7694210357472 * x.[48] * x.[103] // AAa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            -56.7694210357472 * x.[3] * x.[12] * x.[103] // A + Aa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            60.8364303415014 * x.[48] * x.[121] // AAa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            -60.8364303415014 * x.[3] * x.[12] * x.[121] // A + Aa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            2372.62078331855 * x.[48] * x.[244] // AAa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            -2372.62078331855 * x.[3] * x.[12] * x.[244] // A + Aa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            1.0 * x.[228] // cAa | ligation: c + Aa <-> cAa
            -1.0 * x.[8] * x.[12] // c + Aa | ligation: c + Aa <-> cAa
            1.0 * x.[120] // CAa | ligation: C + Aa <-> CAa
            -1.0 * x.[5] * x.[12] // C + Aa | ligation: C + Aa <-> CAa
            1.0 * x.[192] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[7] * x.[12] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[84] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[12] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[68] // Aac | ligation: Aa + c <-> Aac
            -1.0 * x.[12] * x.[8] // Aa + c | ligation: Aa + c <-> Aac
            1.0 * x.[67] // Aab | ligation: Aa + b <-> Aab
            -1.0 * x.[12] * x.[7] // Aa + b | ligation: Aa + b <-> Aab
            1.0 * x.[66] // Aaa | ligation: Aa + a <-> Aaa
            -1.0 * x.[12] * x.[6] // Aa + a | ligation: Aa + a <-> Aaa
            1.0 * x.[65] // AaC | ligation: Aa + C <-> AaC
            -1.0 * x.[12] * x.[5] // Aa + C | ligation: Aa + C <-> AaC
            1.0 * x.[64] // AaB | ligation: Aa + B <-> AaB
            -1.0 * x.[12] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            1.0 * x.[63] // AaA | ligation: Aa + A <-> AaA
            -1.0 * x.[12] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            1.0 * x.[156] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[6] * x.[12] // a + Aa | ligation: a + Aa <-> aAa
            -1.0 * x.[12] // Aa | ligation: A + a <-> Aa
            1.0 * x.[3] * x.[6] // A + a | ligation: A + a <-> Aa
            1.0 * x.[48] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[12] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 13 - Ab
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[49] * x.[190] // AAb + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            -2214.00742039413 * x.[3] * x.[13] * x.[190] // A + Ab + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            56.7694210357472 * x.[49] * x.[103] // AAb + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            -56.7694210357472 * x.[3] * x.[13] * x.[103] // A + Ab + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            60.8364303415014 * x.[49] * x.[121] // AAb + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            -60.8364303415014 * x.[3] * x.[13] * x.[121] // A + Ab + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            2372.62078331855 * x.[49] * x.[244] // AAb + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            -2372.62078331855 * x.[3] * x.[13] * x.[244] // A + Ab + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            1.0 * x.[229] // cAb | ligation: c + Ab <-> cAb
            -1.0 * x.[8] * x.[13] // c + Ab | ligation: c + Ab <-> cAb
            1.0 * x.[121] // CAb | ligation: C + Ab <-> CAb
            -1.0 * x.[5] * x.[13] // C + Ab | ligation: C + Ab <-> CAb
            1.0 * x.[193] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[7] * x.[13] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[85] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[13] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[74] // Abc | ligation: Ab + c <-> Abc
            -1.0 * x.[13] * x.[8] // Ab + c | ligation: Ab + c <-> Abc
            1.0 * x.[73] // Abb | ligation: Ab + b <-> Abb
            -1.0 * x.[13] * x.[7] // Ab + b | ligation: Ab + b <-> Abb
            1.0 * x.[72] // Aba | ligation: Ab + a <-> Aba
            -1.0 * x.[13] * x.[6] // Ab + a | ligation: Ab + a <-> Aba
            1.0 * x.[71] // AbC | ligation: Ab + C <-> AbC
            -1.0 * x.[13] * x.[5] // Ab + C | ligation: Ab + C <-> AbC
            1.0 * x.[70] // AbB | ligation: Ab + B <-> AbB
            -1.0 * x.[13] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            1.0 * x.[69] // AbA | ligation: Ab + A <-> AbA
            -1.0 * x.[13] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            -1.0 * x.[13] // Ab | ligation: A + b <-> Ab
            1.0 * x.[3] * x.[7] // A + b | ligation: A + b <-> Ab
            1.0 * x.[157] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[6] * x.[13] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[49] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[13] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 14 - Ac
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[50] * x.[190] // AAc + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            -2214.00742039413 * x.[3] * x.[14] * x.[190] // A + Ac + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            56.7694210357472 * x.[50] * x.[103] // AAc + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            -56.7694210357472 * x.[3] * x.[14] * x.[103] // A + Ac + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            60.8364303415014 * x.[50] * x.[121] // AAc + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            -60.8364303415014 * x.[3] * x.[14] * x.[121] // A + Ac + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            2372.62078331855 * x.[50] * x.[244] // AAc + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            -2372.62078331855 * x.[3] * x.[14] * x.[244] // A + Ac + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            1.0 * x.[230] // cAc | ligation: c + Ac <-> cAc
            -1.0 * x.[8] * x.[14] // c + Ac | ligation: c + Ac <-> cAc
            1.0 * x.[122] // CAc | ligation: C + Ac <-> CAc
            -1.0 * x.[5] * x.[14] // C + Ac | ligation: C + Ac <-> CAc
            1.0 * x.[194] // bAc | ligation: b + Ac <-> bAc
            -1.0 * x.[7] * x.[14] // b + Ac | ligation: b + Ac <-> bAc
            1.0 * x.[86] // BAc | ligation: B + Ac <-> BAc
            -1.0 * x.[4] * x.[14] // B + Ac | ligation: B + Ac <-> BAc
            1.0 * x.[80] // Acc | ligation: Ac + c <-> Acc
            -1.0 * x.[14] * x.[8] // Ac + c | ligation: Ac + c <-> Acc
            1.0 * x.[79] // Acb | ligation: Ac + b <-> Acb
            -1.0 * x.[14] * x.[7] // Ac + b | ligation: Ac + b <-> Acb
            1.0 * x.[78] // Aca | ligation: Ac + a <-> Aca
            -1.0 * x.[14] * x.[6] // Ac + a | ligation: Ac + a <-> Aca
            1.0 * x.[77] // AcC | ligation: Ac + C <-> AcC
            -1.0 * x.[14] * x.[5] // Ac + C | ligation: Ac + C <-> AcC
            1.0 * x.[76] // AcB | ligation: Ac + B <-> AcB
            -1.0 * x.[14] * x.[4] // Ac + B | ligation: Ac + B <-> AcB
            1.0 * x.[75] // AcA | ligation: Ac + A <-> AcA
            -1.0 * x.[14] * x.[3] // Ac + A | ligation: Ac + A <-> AcA
            -1.0 * x.[14] // Ac | ligation: A + c <-> Ac
            1.0 * x.[3] * x.[8] // A + c | ligation: A + c <-> Ac
            1.0 * x.[158] // aAc | ligation: a + Ac <-> aAc
            -1.0 * x.[6] * x.[14] // a + Ac | ligation: a + Ac <-> aAc
            1.0 * x.[50] // AAc | ligation: A + Ac <-> AAc
            -1.0 * x.[3] * x.[14] // A + Ac | ligation: A + Ac <-> AAc
        |]
        |> Array.sum


    // 15 - BA
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[81] * x.[190] // BAA + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            -2214.00742039413 * x.[15] * x.[3] * x.[190] // BA + A + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            56.7694210357472 * x.[81] * x.[103] // BAA + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            -56.7694210357472 * x.[15] * x.[3] * x.[103] // BA + A + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            60.8364303415014 * x.[81] * x.[121] // BAA + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            -60.8364303415014 * x.[15] * x.[3] * x.[121] // BA + A + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            2372.62078331855 * x.[81] * x.[244] // BAA + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            -2372.62078331855 * x.[15] * x.[3] * x.[244] // BA + A + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            60.8364303415014 * x.[82] * x.[121] // BAB + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            -60.8364303415014 * x.[15] * x.[4] * x.[121] // BA + B + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            2372.62078331855 * x.[82] * x.[244] // BAB + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            -2372.62078331855 * x.[15] * x.[4] * x.[244] // BA + B + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            60.8364303415014 * x.[51] * x.[121] // ABA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            -60.8364303415014 * x.[3] * x.[15] * x.[121] // A + BA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            2372.62078331855 * x.[51] * x.[244] // ABA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            -2372.62078331855 * x.[3] * x.[15] * x.[244] // A + BA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            60.8364303415014 * x.[123] * x.[121] // CBA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            -60.8364303415014 * x.[5] * x.[15] * x.[121] // C + BA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            2372.62078331855 * x.[123] * x.[244] // CBA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            -2372.62078331855 * x.[5] * x.[15] * x.[244] // C + BA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            1.0 * x.[231] // cBA | ligation: c + BA <-> cBA
            -1.0 * x.[8] * x.[15] // c + BA | ligation: c + BA <-> cBA
            1.0 * x.[123] // CBA | ligation: C + BA <-> CBA
            -1.0 * x.[5] * x.[15] // C + BA | ligation: C + BA <-> CBA
            1.0 * x.[86] // BAc | ligation: BA + c <-> BAc
            -1.0 * x.[15] * x.[8] // BA + c | ligation: BA + c <-> BAc
            1.0 * x.[85] // BAb | ligation: BA + b <-> BAb
            -1.0 * x.[15] * x.[7] // BA + b | ligation: BA + b <-> BAb
            1.0 * x.[84] // BAa | ligation: BA + a <-> BAa
            -1.0 * x.[15] * x.[6] // BA + a | ligation: BA + a <-> BAa
            1.0 * x.[83] // BAC | ligation: BA + C <-> BAC
            -1.0 * x.[15] * x.[5] // BA + C | ligation: BA + C <-> BAC
            1.0 * x.[82] // BAB | ligation: BA + B <-> BAB
            -1.0 * x.[15] * x.[4] // BA + B | ligation: BA + B <-> BAB
            1.0 * x.[81] // BAA | ligation: BA + A <-> BAA
            -1.0 * x.[15] * x.[3] // BA + A | ligation: BA + A <-> BAA
            1.0 * x.[195] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[7] * x.[15] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[87] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[15] // B + BA | ligation: B + BA <-> BBA
            -1.0 * x.[15] // BA | ligation: B + A <-> BA
            1.0 * x.[4] * x.[3] // B + A | ligation: B + A <-> BA
            1.0 * x.[159] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[6] * x.[15] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[51] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[15] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 16 - BB
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[89] * x.[190] // BBC + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            -2214.00742039413 * x.[16] * x.[5] * x.[190] // BB + C + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            56.7694210357472 * x.[89] * x.[103] // BBC + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            -56.7694210357472 * x.[16] * x.[5] * x.[103] // BB + C + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            60.8364303415014 * x.[52] * x.[121] // ABB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            -60.8364303415014 * x.[3] * x.[16] * x.[121] // A + BB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            2372.62078331855 * x.[52] * x.[244] // ABB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            -2372.62078331855 * x.[3] * x.[16] * x.[244] // A + BB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            60.8364303415014 * x.[124] * x.[121] // CBB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            -60.8364303415014 * x.[5] * x.[16] * x.[121] // C + BB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            2372.62078331855 * x.[124] * x.[244] // CBB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            -2372.62078331855 * x.[5] * x.[16] * x.[244] // C + BB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            1.0 * x.[232] // cBB | ligation: c + BB <-> cBB
            -1.0 * x.[8] * x.[16] // c + BB | ligation: c + BB <-> cBB
            1.0 * x.[124] // CBB | ligation: C + BB <-> CBB
            -1.0 * x.[5] * x.[16] // C + BB | ligation: C + BB <-> CBB
            1.0 * x.[92] // BBc | ligation: BB + c <-> BBc
            -1.0 * x.[16] * x.[8] // BB + c | ligation: BB + c <-> BBc
            1.0 * x.[91] // BBb | ligation: BB + b <-> BBb
            -1.0 * x.[16] * x.[7] // BB + b | ligation: BB + b <-> BBb
            1.0 * x.[90] // BBa | ligation: BB + a <-> BBa
            -1.0 * x.[16] * x.[6] // BB + a | ligation: BB + a <-> BBa
            1.0 * x.[89] // BBC | ligation: BB + C <-> BBC
            -1.0 * x.[16] * x.[5] // BB + C | ligation: BB + C <-> BBC
            1.0 * x.[88] // BBB | ligation: BB + B <-> BBB
            -1.0 * x.[16] * x.[4] // BB + B | ligation: BB + B <-> BBB
            1.0 * x.[87] // BBA | ligation: BB + A <-> BBA
            -1.0 * x.[16] * x.[3] // BB + A | ligation: BB + A <-> BBA
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
            2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            -2214.00742039413 * x.[17] * x.[5] * x.[190] // BC + C + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            -56.7694210357472 * x.[17] * x.[5] * x.[103] // BC + C + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            -2214.00742039413 * x.[17] * x.[190] // BC + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            2214.00742039413 * x.[4] * x.[5] * x.[190] // B + C + bAB | catalytic ligation: B + C + bAB <-> BC + bAB
            -56.7694210357472 * x.[17] * x.[103] // BC + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            56.7694210357472 * x.[4] * x.[5] * x.[103] // B + C + Bab | catalytic ligation: B + C + Bab <-> BC + Bab
            60.8364303415014 * x.[53] * x.[121] // ABC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            -60.8364303415014 * x.[3] * x.[17] * x.[121] // A + BC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            2372.62078331855 * x.[53] * x.[244] // ABC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            -2372.62078331855 * x.[3] * x.[17] * x.[244] // A + BC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            60.8364303415014 * x.[94] * x.[121] // BCB + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            -60.8364303415014 * x.[17] * x.[4] * x.[121] // BC + B + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            2372.62078331855 * x.[94] * x.[244] // BCB + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            -2372.62078331855 * x.[17] * x.[4] * x.[244] // BC + B + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            60.8364303415014 * x.[125] * x.[121] // CBC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            -60.8364303415014 * x.[5] * x.[17] * x.[121] // C + BC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            2372.62078331855 * x.[125] * x.[244] // CBC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            -2372.62078331855 * x.[5] * x.[17] * x.[244] // C + BC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            1.0 * x.[233] // cBC | ligation: c + BC <-> cBC
            -1.0 * x.[8] * x.[17] // c + BC | ligation: c + BC <-> cBC
            1.0 * x.[125] // CBC | ligation: C + BC <-> CBC
            -1.0 * x.[5] * x.[17] // C + BC | ligation: C + BC <-> CBC
            1.0 * x.[98] // BCc | ligation: BC + c <-> BCc
            -1.0 * x.[17] * x.[8] // BC + c | ligation: BC + c <-> BCc
            1.0 * x.[97] // BCb | ligation: BC + b <-> BCb
            -1.0 * x.[17] * x.[7] // BC + b | ligation: BC + b <-> BCb
            1.0 * x.[96] // BCa | ligation: BC + a <-> BCa
            -1.0 * x.[17] * x.[6] // BC + a | ligation: BC + a <-> BCa
            1.0 * x.[95] // BCC | ligation: BC + C <-> BCC
            -1.0 * x.[17] * x.[5] // BC + C | ligation: BC + C <-> BCC
            1.0 * x.[94] // BCB | ligation: BC + B <-> BCB
            -1.0 * x.[17] * x.[4] // BC + B | ligation: BC + B <-> BCB
            1.0 * x.[93] // BCA | ligation: BC + A <-> BCA
            -1.0 * x.[17] * x.[3] // BC + A | ligation: BC + A <-> BCA
            1.0 * x.[197] // bBC | ligation: b + BC <-> bBC
            -1.0 * x.[7] * x.[17] // b + BC | ligation: b + BC <-> bBC
            -1.0 * x.[17] // BC | ligation: B + C <-> BC
            1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[89] // BBC | ligation: B + BC <-> BBC
            -1.0 * x.[4] * x.[17] // B + BC | ligation: B + BC <-> BBC
            1.0 * x.[161] // aBC | ligation: a + BC <-> aBC
            -1.0 * x.[6] * x.[17] // a + BC | ligation: a + BC <-> aBC
            1.0 * x.[53] // ABC | ligation: A + BC <-> ABC
            -1.0 * x.[3] * x.[17] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 18 - Ba
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[54] * x.[121] // ABa + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            -60.8364303415014 * x.[3] * x.[18] * x.[121] // A + Ba + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            2372.62078331855 * x.[54] * x.[244] // ABa + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            -2372.62078331855 * x.[3] * x.[18] * x.[244] // A + Ba + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            60.8364303415014 * x.[126] * x.[121] // CBa + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            -60.8364303415014 * x.[5] * x.[18] * x.[121] // C + Ba + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            2372.62078331855 * x.[126] * x.[244] // CBa + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            -2372.62078331855 * x.[5] * x.[18] * x.[244] // C + Ba + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            1.0 * x.[234] // cBa | ligation: c + Ba <-> cBa
            -1.0 * x.[8] * x.[18] // c + Ba | ligation: c + Ba <-> cBa
            1.0 * x.[126] // CBa | ligation: C + Ba <-> CBa
            -1.0 * x.[5] * x.[18] // C + Ba | ligation: C + Ba <-> CBa
            1.0 * x.[104] // Bac | ligation: Ba + c <-> Bac
            -1.0 * x.[18] * x.[8] // Ba + c | ligation: Ba + c <-> Bac
            1.0 * x.[103] // Bab | ligation: Ba + b <-> Bab
            -1.0 * x.[18] * x.[7] // Ba + b | ligation: Ba + b <-> Bab
            1.0 * x.[102] // Baa | ligation: Ba + a <-> Baa
            -1.0 * x.[18] * x.[6] // Ba + a | ligation: Ba + a <-> Baa
            1.0 * x.[101] // BaC | ligation: Ba + C <-> BaC
            -1.0 * x.[18] * x.[5] // Ba + C | ligation: Ba + C <-> BaC
            1.0 * x.[100] // BaB | ligation: Ba + B <-> BaB
            -1.0 * x.[18] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            1.0 * x.[99] // BaA | ligation: Ba + A <-> BaA
            -1.0 * x.[18] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            1.0 * x.[198] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[7] * x.[18] // b + Ba | ligation: b + Ba <-> bBa
            -1.0 * x.[18] // Ba | ligation: B + a <-> Ba
            1.0 * x.[4] * x.[6] // B + a | ligation: B + a <-> Ba
            1.0 * x.[90] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[18] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[162] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[6] * x.[18] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[54] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[18] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 19 - Bb
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[55] * x.[121] // ABb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            -60.8364303415014 * x.[3] * x.[19] * x.[121] // A + Bb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            2372.62078331855 * x.[55] * x.[244] // ABb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            -2372.62078331855 * x.[3] * x.[19] * x.[244] // A + Bb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            60.8364303415014 * x.[127] * x.[121] // CBb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            -60.8364303415014 * x.[5] * x.[19] * x.[121] // C + Bb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            2372.62078331855 * x.[127] * x.[244] // CBb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            -2372.62078331855 * x.[5] * x.[19] * x.[244] // C + Bb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            1.0 * x.[235] // cBb | ligation: c + Bb <-> cBb
            -1.0 * x.[8] * x.[19] // c + Bb | ligation: c + Bb <-> cBb
            1.0 * x.[127] // CBb | ligation: C + Bb <-> CBb
            -1.0 * x.[5] * x.[19] // C + Bb | ligation: C + Bb <-> CBb
            1.0 * x.[110] // Bbc | ligation: Bb + c <-> Bbc
            -1.0 * x.[19] * x.[8] // Bb + c | ligation: Bb + c <-> Bbc
            1.0 * x.[109] // Bbb | ligation: Bb + b <-> Bbb
            -1.0 * x.[19] * x.[7] // Bb + b | ligation: Bb + b <-> Bbb
            1.0 * x.[108] // Bba | ligation: Bb + a <-> Bba
            -1.0 * x.[19] * x.[6] // Bb + a | ligation: Bb + a <-> Bba
            1.0 * x.[107] // BbC | ligation: Bb + C <-> BbC
            -1.0 * x.[19] * x.[5] // Bb + C | ligation: Bb + C <-> BbC
            1.0 * x.[106] // BbB | ligation: Bb + B <-> BbB
            -1.0 * x.[19] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            1.0 * x.[105] // BbA | ligation: Bb + A <-> BbA
            -1.0 * x.[19] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            1.0 * x.[199] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[7] * x.[19] // b + Bb | ligation: b + Bb <-> bBb
            -1.0 * x.[19] // Bb | ligation: B + b <-> Bb
            1.0 * x.[4] * x.[7] // B + b | ligation: B + b <-> Bb
            1.0 * x.[91] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[19] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[163] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[6] * x.[19] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[55] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[19] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 20 - Bc
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[56] * x.[121] // ABc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            -60.8364303415014 * x.[3] * x.[20] * x.[121] // A + Bc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            2372.62078331855 * x.[56] * x.[244] // ABc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            -2372.62078331855 * x.[3] * x.[20] * x.[244] // A + Bc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            60.8364303415014 * x.[128] * x.[121] // CBc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            -60.8364303415014 * x.[5] * x.[20] * x.[121] // C + Bc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            2372.62078331855 * x.[128] * x.[244] // CBc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            -2372.62078331855 * x.[5] * x.[20] * x.[244] // C + Bc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            1.0 * x.[236] // cBc | ligation: c + Bc <-> cBc
            -1.0 * x.[8] * x.[20] // c + Bc | ligation: c + Bc <-> cBc
            1.0 * x.[128] // CBc | ligation: C + Bc <-> CBc
            -1.0 * x.[5] * x.[20] // C + Bc | ligation: C + Bc <-> CBc
            1.0 * x.[116] // Bcc | ligation: Bc + c <-> Bcc
            -1.0 * x.[20] * x.[8] // Bc + c | ligation: Bc + c <-> Bcc
            1.0 * x.[115] // Bcb | ligation: Bc + b <-> Bcb
            -1.0 * x.[20] * x.[7] // Bc + b | ligation: Bc + b <-> Bcb
            1.0 * x.[114] // Bca | ligation: Bc + a <-> Bca
            -1.0 * x.[20] * x.[6] // Bc + a | ligation: Bc + a <-> Bca
            1.0 * x.[113] // BcC | ligation: Bc + C <-> BcC
            -1.0 * x.[20] * x.[5] // Bc + C | ligation: Bc + C <-> BcC
            1.0 * x.[112] // BcB | ligation: Bc + B <-> BcB
            -1.0 * x.[20] * x.[4] // Bc + B | ligation: Bc + B <-> BcB
            1.0 * x.[111] // BcA | ligation: Bc + A <-> BcA
            -1.0 * x.[20] * x.[3] // Bc + A | ligation: Bc + A <-> BcA
            -1.0 * x.[20] // Bc | ligation: B + c <-> Bc
            1.0 * x.[4] * x.[8] // B + c | ligation: B + c <-> Bc
            1.0 * x.[200] // bBc | ligation: b + Bc <-> bBc
            -1.0 * x.[7] * x.[20] // b + Bc | ligation: b + Bc <-> bBc
            1.0 * x.[92] // BBc | ligation: B + Bc <-> BBc
            -1.0 * x.[4] * x.[20] // B + Bc | ligation: B + Bc <-> BBc
            1.0 * x.[164] // aBc | ligation: a + Bc <-> aBc
            -1.0 * x.[6] * x.[20] // a + Bc | ligation: a + Bc <-> aBc
            1.0 * x.[56] // ABc | ligation: A + Bc <-> ABc
            -1.0 * x.[3] * x.[20] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 21 - CA
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[117] * x.[190] // CAA + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            -2214.00742039413 * x.[21] * x.[3] * x.[190] // CA + A + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            56.7694210357472 * x.[117] * x.[103] // CAA + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            -56.7694210357472 * x.[21] * x.[3] * x.[103] // CA + A + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            2214.00742039413 * x.[129] * x.[190] // CCA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            -2214.00742039413 * x.[5] * x.[21] * x.[190] // C + CA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            56.7694210357472 * x.[129] * x.[103] // CCA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            -56.7694210357472 * x.[5] * x.[21] * x.[103] // C + CA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            2214.00742039413 * x.[93] * x.[190] // BCA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            -2214.00742039413 * x.[4] * x.[21] * x.[190] // B + CA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            56.7694210357472 * x.[93] * x.[103] // BCA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            -56.7694210357472 * x.[4] * x.[21] * x.[103] // B + CA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            60.8364303415014 * x.[117] * x.[121] // CAA + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            -60.8364303415014 * x.[21] * x.[3] * x.[121] // CA + A + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            2372.62078331855 * x.[117] * x.[244] // CAA + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            -2372.62078331855 * x.[21] * x.[3] * x.[244] // CA + A + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            60.8364303415014 * x.[118] * x.[121] // CAB + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            -60.8364303415014 * x.[21] * x.[4] * x.[121] // CA + B + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            2372.62078331855 * x.[118] * x.[244] // CAB + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            -2372.62078331855 * x.[21] * x.[4] * x.[244] // CA + B + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            1.0 * x.[122] // CAc | ligation: CA + c <-> CAc
            -1.0 * x.[21] * x.[8] // CA + c | ligation: CA + c <-> CAc
            1.0 * x.[121] // CAb | ligation: CA + b <-> CAb
            -1.0 * x.[21] * x.[7] // CA + b | ligation: CA + b <-> CAb
            1.0 * x.[120] // CAa | ligation: CA + a <-> CAa
            -1.0 * x.[21] * x.[6] // CA + a | ligation: CA + a <-> CAa
            1.0 * x.[119] // CAC | ligation: CA + C <-> CAC
            -1.0 * x.[21] * x.[5] // CA + C | ligation: CA + C <-> CAC
            1.0 * x.[118] // CAB | ligation: CA + B <-> CAB
            -1.0 * x.[21] * x.[4] // CA + B | ligation: CA + B <-> CAB
            1.0 * x.[117] // CAA | ligation: CA + A <-> CAA
            -1.0 * x.[21] * x.[3] // CA + A | ligation: CA + A <-> CAA
            1.0 * x.[237] // cCA | ligation: c + CA <-> cCA
            -1.0 * x.[8] * x.[21] // c + CA | ligation: c + CA <-> cCA
            1.0 * x.[129] // CCA | ligation: C + CA <-> CCA
            -1.0 * x.[5] * x.[21] // C + CA | ligation: C + CA <-> CCA
            -1.0 * x.[21] // CA | ligation: C + A <-> CA
            1.0 * x.[5] * x.[3] // C + A | ligation: C + A <-> CA
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
            2214.00742039413 * x.[130] * x.[190] // CCB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            -2214.00742039413 * x.[5] * x.[22] * x.[190] // C + CB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            56.7694210357472 * x.[130] * x.[103] // CCB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            -56.7694210357472 * x.[5] * x.[22] * x.[103] // C + CB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            2214.00742039413 * x.[125] * x.[190] // CBC + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            -2214.00742039413 * x.[22] * x.[5] * x.[190] // CB + C + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            56.7694210357472 * x.[125] * x.[103] // CBC + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            -56.7694210357472 * x.[22] * x.[5] * x.[103] // CB + C + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            2214.00742039413 * x.[94] * x.[190] // BCB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            -2214.00742039413 * x.[4] * x.[22] * x.[190] // B + CB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            56.7694210357472 * x.[94] * x.[103] // BCB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            -56.7694210357472 * x.[4] * x.[22] * x.[103] // B + CB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            -60.8364303415014 * x.[22] * x.[121] // CB + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            60.8364303415014 * x.[5] * x.[4] * x.[121] // C + B + CAb | catalytic ligation: C + B + CAb <-> CB + CAb
            -2372.62078331855 * x.[22] * x.[244] // CB + caB | catalytic ligation: C + B + caB <-> CB + caB
            2372.62078331855 * x.[5] * x.[4] * x.[244] // C + B + caB | catalytic ligation: C + B + caB <-> CB + caB
            1.0 * x.[128] // CBc | ligation: CB + c <-> CBc
            -1.0 * x.[22] * x.[8] // CB + c | ligation: CB + c <-> CBc
            1.0 * x.[127] // CBb | ligation: CB + b <-> CBb
            -1.0 * x.[22] * x.[7] // CB + b | ligation: CB + b <-> CBb
            1.0 * x.[126] // CBa | ligation: CB + a <-> CBa
            -1.0 * x.[22] * x.[6] // CB + a | ligation: CB + a <-> CBa
            1.0 * x.[125] // CBC | ligation: CB + C <-> CBC
            -1.0 * x.[22] * x.[5] // CB + C | ligation: CB + C <-> CBC
            1.0 * x.[124] // CBB | ligation: CB + B <-> CBB
            -1.0 * x.[22] * x.[4] // CB + B | ligation: CB + B <-> CBB
            1.0 * x.[123] // CBA | ligation: CB + A <-> CBA
            -1.0 * x.[22] * x.[3] // CB + A | ligation: CB + A <-> CBA
            1.0 * x.[238] // cCB | ligation: c + CB <-> cCB
            -1.0 * x.[8] * x.[22] // c + CB | ligation: c + CB <-> cCB
            1.0 * x.[130] // CCB | ligation: C + CB <-> CCB
            -1.0 * x.[5] * x.[22] // C + CB | ligation: C + CB <-> CCB
            -1.0 * x.[22] // CB | ligation: C + B <-> CB
            1.0 * x.[5] * x.[4] // C + B | ligation: C + B <-> CB
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
            2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            -2214.00742039413 * x.[23] * x.[5] * x.[190] // CC + C + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            -56.7694210357472 * x.[23] * x.[5] * x.[103] // CC + C + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            -2214.00742039413 * x.[5] * x.[23] * x.[190] // C + CC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            -56.7694210357472 * x.[5] * x.[23] * x.[103] // C + CC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            -2214.00742039413 * x.[23] * x.[190] // CC + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            2214.00742039413 * x.[5] * x.[5] * x.[190] // C + C + bAB | catalytic ligation: C + C + bAB <-> CC + bAB
            -56.7694210357472 * x.[23] * x.[103] // CC + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            56.7694210357472 * x.[5] * x.[5] * x.[103] // C + C + Bab | catalytic ligation: C + C + Bab <-> CC + Bab
            2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            -2214.00742039413 * x.[4] * x.[23] * x.[190] // B + CC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            -56.7694210357472 * x.[4] * x.[23] * x.[103] // B + CC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            60.8364303415013 * x.[130] * x.[121] // CCB + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            -60.8364303415013 * x.[23] * x.[4] * x.[121] // CC + B + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            2372.62078331855 * x.[130] * x.[244] // CCB + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            -2372.62078331855 * x.[23] * x.[4] * x.[244] // CC + B + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            1.0 * x.[134] // CCc | ligation: CC + c <-> CCc
            -1.0 * x.[23] * x.[8] // CC + c | ligation: CC + c <-> CCc
            1.0 * x.[133] // CCb | ligation: CC + b <-> CCb
            -1.0 * x.[23] * x.[7] // CC + b | ligation: CC + b <-> CCb
            1.0 * x.[132] // CCa | ligation: CC + a <-> CCa
            -1.0 * x.[23] * x.[6] // CC + a | ligation: CC + a <-> CCa
            1.0 * x.[131] // CCC | ligation: CC + C <-> CCC
            -1.0 * x.[23] * x.[5] // CC + C | ligation: CC + C <-> CCC
            1.0 * x.[130] // CCB | ligation: CC + B <-> CCB
            -1.0 * x.[23] * x.[4] // CC + B | ligation: CC + B <-> CCB
            1.0 * x.[129] // CCA | ligation: CC + A <-> CCA
            -1.0 * x.[23] * x.[3] // CC + A | ligation: CC + A <-> CCA
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
            2214.00742039413 * x.[132] * x.[190] // CCa + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            -2214.00742039413 * x.[5] * x.[24] * x.[190] // C + Ca + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            56.7694210357472 * x.[132] * x.[103] // CCa + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            -56.7694210357472 * x.[5] * x.[24] * x.[103] // C + Ca + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            2214.00742039413 * x.[96] * x.[190] // BCa + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            -2214.00742039413 * x.[4] * x.[24] * x.[190] // B + Ca + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            56.7694210357472 * x.[96] * x.[103] // BCa + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            -56.7694210357472 * x.[4] * x.[24] * x.[103] // B + Ca + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            1.0 * x.[140] // Cac | ligation: Ca + c <-> Cac
            -1.0 * x.[24] * x.[8] // Ca + c | ligation: Ca + c <-> Cac
            1.0 * x.[139] // Cab | ligation: Ca + b <-> Cab
            -1.0 * x.[24] * x.[7] // Ca + b | ligation: Ca + b <-> Cab
            1.0 * x.[138] // Caa | ligation: Ca + a <-> Caa
            -1.0 * x.[24] * x.[6] // Ca + a | ligation: Ca + a <-> Caa
            1.0 * x.[137] // CaC | ligation: Ca + C <-> CaC
            -1.0 * x.[24] * x.[5] // Ca + C | ligation: Ca + C <-> CaC
            1.0 * x.[136] // CaB | ligation: Ca + B <-> CaB
            -1.0 * x.[24] * x.[4] // Ca + B | ligation: Ca + B <-> CaB
            1.0 * x.[135] // CaA | ligation: Ca + A <-> CaA
            -1.0 * x.[24] * x.[3] // Ca + A | ligation: Ca + A <-> CaA
            1.0 * x.[240] // cCa | ligation: c + Ca <-> cCa
            -1.0 * x.[8] * x.[24] // c + Ca | ligation: c + Ca <-> cCa
            -1.0 * x.[24] // Ca | ligation: C + a <-> Ca
            1.0 * x.[5] * x.[6] // C + a | ligation: C + a <-> Ca
            1.0 * x.[132] // CCa | ligation: C + Ca <-> CCa
            -1.0 * x.[5] * x.[24] // C + Ca | ligation: C + Ca <-> CCa
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
            2214.00742039413 * x.[133] * x.[190] // CCb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            -2214.00742039413 * x.[5] * x.[25] * x.[190] // C + Cb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            56.7694210357472 * x.[133] * x.[103] // CCb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            -56.7694210357472 * x.[5] * x.[25] * x.[103] // C + Cb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            2214.00742039413 * x.[97] * x.[190] // BCb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            -2214.00742039413 * x.[4] * x.[25] * x.[190] // B + Cb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            56.7694210357472 * x.[97] * x.[103] // BCb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            -56.7694210357472 * x.[4] * x.[25] * x.[103] // B + Cb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            1.0 * x.[146] // Cbc | ligation: Cb + c <-> Cbc
            -1.0 * x.[25] * x.[8] // Cb + c | ligation: Cb + c <-> Cbc
            1.0 * x.[145] // Cbb | ligation: Cb + b <-> Cbb
            -1.0 * x.[25] * x.[7] // Cb + b | ligation: Cb + b <-> Cbb
            1.0 * x.[144] // Cba | ligation: Cb + a <-> Cba
            -1.0 * x.[25] * x.[6] // Cb + a | ligation: Cb + a <-> Cba
            1.0 * x.[143] // CbC | ligation: Cb + C <-> CbC
            -1.0 * x.[25] * x.[5] // Cb + C | ligation: Cb + C <-> CbC
            1.0 * x.[142] // CbB | ligation: Cb + B <-> CbB
            -1.0 * x.[25] * x.[4] // Cb + B | ligation: Cb + B <-> CbB
            1.0 * x.[141] // CbA | ligation: Cb + A <-> CbA
            -1.0 * x.[25] * x.[3] // Cb + A | ligation: Cb + A <-> CbA
            1.0 * x.[241] // cCb | ligation: c + Cb <-> cCb
            -1.0 * x.[8] * x.[25] // c + Cb | ligation: c + Cb <-> cCb
            -1.0 * x.[25] // Cb | ligation: C + b <-> Cb
            1.0 * x.[5] * x.[7] // C + b | ligation: C + b <-> Cb
            1.0 * x.[133] // CCb | ligation: C + Cb <-> CCb
            -1.0 * x.[5] * x.[25] // C + Cb | ligation: C + Cb <-> CCb
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
            2214.00742039413 * x.[134] * x.[190] // CCc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            -2214.00742039413 * x.[5] * x.[26] * x.[190] // C + Cc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            56.7694210357472 * x.[134] * x.[103] // CCc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            -56.7694210357472 * x.[5] * x.[26] * x.[103] // C + Cc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            2214.00742039413 * x.[98] * x.[190] // BCc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            -2214.00742039413 * x.[4] * x.[26] * x.[190] // B + Cc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            56.7694210357472 * x.[98] * x.[103] // BCc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            -56.7694210357472 * x.[4] * x.[26] * x.[103] // B + Cc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            1.0 * x.[152] // Ccc | ligation: Cc + c <-> Ccc
            -1.0 * x.[26] * x.[8] // Cc + c | ligation: Cc + c <-> Ccc
            1.0 * x.[151] // Ccb | ligation: Cc + b <-> Ccb
            -1.0 * x.[26] * x.[7] // Cc + b | ligation: Cc + b <-> Ccb
            1.0 * x.[150] // Cca | ligation: Cc + a <-> Cca
            -1.0 * x.[26] * x.[6] // Cc + a | ligation: Cc + a <-> Cca
            1.0 * x.[149] // CcC | ligation: Cc + C <-> CcC
            -1.0 * x.[26] * x.[5] // Cc + C | ligation: Cc + C <-> CcC
            1.0 * x.[148] // CcB | ligation: Cc + B <-> CcB
            -1.0 * x.[26] * x.[4] // Cc + B | ligation: Cc + B <-> CcB
            1.0 * x.[147] // CcA | ligation: Cc + A <-> CcA
            -1.0 * x.[26] * x.[3] // Cc + A | ligation: Cc + A <-> CcA
            1.0 * x.[242] // cCc | ligation: c + Cc <-> cCc
            -1.0 * x.[8] * x.[26] // c + Cc | ligation: c + Cc <-> cCc
            -1.0 * x.[26] // Cc | ligation: C + c <-> Cc
            1.0 * x.[5] * x.[8] // C + c | ligation: C + c <-> Cc
            1.0 * x.[134] // CCc | ligation: C + Cc <-> CCc
            -1.0 * x.[5] * x.[26] // C + Cc | ligation: C + Cc <-> CCc
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
            2214.00742039413 * x.[171] * x.[103] // aaA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            -2214.00742039413 * x.[6] * x.[27] * x.[103] // a + aA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            56.7694210357472 * x.[171] * x.[190] // aaA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            -56.7694210357472 * x.[6] * x.[27] * x.[190] // a + aA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            60.8364303415014 * x.[171] * x.[244] // aaA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            -60.8364303415014 * x.[6] * x.[27] * x.[244] // a + aA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            2372.62078331855 * x.[171] * x.[121] // aaA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            -2372.62078331855 * x.[6] * x.[27] * x.[121] // a + aA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            1.0 * x.[135] // CaA | ligation: C + aA <-> CaA
            -1.0 * x.[5] * x.[27] // C + aA | ligation: C + aA <-> CaA
            1.0 * x.[243] // caA | ligation: c + aA <-> caA
            -1.0 * x.[8] * x.[27] // c + aA | ligation: c + aA <-> caA
            1.0 * x.[99] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[27] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[207] // baA | ligation: b + aA <-> baA
            -1.0 * x.[7] * x.[27] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[155] // aAC | ligation: aA + C <-> aAC
            -1.0 * x.[27] * x.[5] // aA + C | ligation: aA + C <-> aAC
            1.0 * x.[154] // aAB | ligation: aA + B <-> aAB
            -1.0 * x.[27] * x.[4] // aA + B | ligation: aA + B <-> aAB
            1.0 * x.[153] // aAA | ligation: aA + A <-> aAA
            -1.0 * x.[27] * x.[3] // aA + A | ligation: aA + A <-> aAA
            1.0 * x.[158] // aAc | ligation: aA + c <-> aAc
            -1.0 * x.[27] * x.[8] // aA + c | ligation: aA + c <-> aAc
            1.0 * x.[157] // aAb | ligation: aA + b <-> aAb
            -1.0 * x.[27] * x.[7] // aA + b | ligation: aA + b <-> aAb
            1.0 * x.[156] // aAa | ligation: aA + a <-> aAa
            -1.0 * x.[27] * x.[6] // aA + a | ligation: aA + a <-> aAa
            1.0 * x.[63] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[27] // A + aA | ligation: A + aA <-> AaA
            -1.0 * x.[27] // aA | ligation: a + A <-> aA
            1.0 * x.[6] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[171] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[6] * x.[27] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 28 - aB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[172] * x.[103] // aaB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            -2214.00742039413 * x.[6] * x.[28] * x.[103] // a + aB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            56.7694210357472 * x.[172] * x.[190] // aaB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            -56.7694210357472 * x.[6] * x.[28] * x.[190] // a + aB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            60.8364303415014 * x.[172] * x.[244] // aaB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            -60.8364303415014 * x.[6] * x.[28] * x.[244] // a + aB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            2372.62078331855 * x.[172] * x.[121] // aaB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            -2372.62078331855 * x.[6] * x.[28] * x.[121] // a + aB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            1.0 * x.[136] // CaB | ligation: C + aB <-> CaB
            -1.0 * x.[5] * x.[28] // C + aB | ligation: C + aB <-> CaB
            1.0 * x.[244] // caB | ligation: c + aB <-> caB
            -1.0 * x.[8] * x.[28] // c + aB | ligation: c + aB <-> caB
            1.0 * x.[100] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[28] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[208] // baB | ligation: b + aB <-> baB
            -1.0 * x.[7] * x.[28] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[161] // aBC | ligation: aB + C <-> aBC
            -1.0 * x.[28] * x.[5] // aB + C | ligation: aB + C <-> aBC
            1.0 * x.[160] // aBB | ligation: aB + B <-> aBB
            -1.0 * x.[28] * x.[4] // aB + B | ligation: aB + B <-> aBB
            1.0 * x.[159] // aBA | ligation: aB + A <-> aBA
            -1.0 * x.[28] * x.[3] // aB + A | ligation: aB + A <-> aBA
            1.0 * x.[164] // aBc | ligation: aB + c <-> aBc
            -1.0 * x.[28] * x.[8] // aB + c | ligation: aB + c <-> aBc
            1.0 * x.[163] // aBb | ligation: aB + b <-> aBb
            -1.0 * x.[28] * x.[7] // aB + b | ligation: aB + b <-> aBb
            1.0 * x.[162] // aBa | ligation: aB + a <-> aBa
            -1.0 * x.[28] * x.[6] // aB + a | ligation: aB + a <-> aBa
            -1.0 * x.[28] // aB | ligation: a + B <-> aB
            1.0 * x.[6] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[64] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[28] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[172] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[6] * x.[28] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 29 - aC
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[173] * x.[103] // aaC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            -2214.00742039413 * x.[6] * x.[29] * x.[103] // a + aC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            56.7694210357472 * x.[173] * x.[190] // aaC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            -56.7694210357472 * x.[6] * x.[29] * x.[190] // a + aC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            60.8364303415014 * x.[173] * x.[244] // aaC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            -60.8364303415014 * x.[6] * x.[29] * x.[244] // a + aC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            2372.62078331855 * x.[173] * x.[121] // aaC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            -2372.62078331855 * x.[6] * x.[29] * x.[121] // a + aC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            1.0 * x.[137] // CaC | ligation: C + aC <-> CaC
            -1.0 * x.[5] * x.[29] // C + aC | ligation: C + aC <-> CaC
            1.0 * x.[245] // caC | ligation: c + aC <-> caC
            -1.0 * x.[8] * x.[29] // c + aC | ligation: c + aC <-> caC
            1.0 * x.[101] // BaC | ligation: B + aC <-> BaC
            -1.0 * x.[4] * x.[29] // B + aC | ligation: B + aC <-> BaC
            1.0 * x.[209] // baC | ligation: b + aC <-> baC
            -1.0 * x.[7] * x.[29] // b + aC | ligation: b + aC <-> baC
            1.0 * x.[167] // aCC | ligation: aC + C <-> aCC
            -1.0 * x.[29] * x.[5] // aC + C | ligation: aC + C <-> aCC
            1.0 * x.[166] // aCB | ligation: aC + B <-> aCB
            -1.0 * x.[29] * x.[4] // aC + B | ligation: aC + B <-> aCB
            1.0 * x.[165] // aCA | ligation: aC + A <-> aCA
            -1.0 * x.[29] * x.[3] // aC + A | ligation: aC + A <-> aCA
            1.0 * x.[170] // aCc | ligation: aC + c <-> aCc
            -1.0 * x.[29] * x.[8] // aC + c | ligation: aC + c <-> aCc
            1.0 * x.[169] // aCb | ligation: aC + b <-> aCb
            -1.0 * x.[29] * x.[7] // aC + b | ligation: aC + b <-> aCb
            1.0 * x.[168] // aCa | ligation: aC + a <-> aCa
            -1.0 * x.[29] * x.[6] // aC + a | ligation: aC + a <-> aCa
            -1.0 * x.[29] // aC | ligation: a + C <-> aC
            1.0 * x.[6] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[65] // AaC | ligation: A + aC <-> AaC
            -1.0 * x.[3] * x.[29] // A + aC | ligation: A + aC <-> AaC
            1.0 * x.[173] // aaC | ligation: a + aC <-> aaC
            -1.0 * x.[6] * x.[29] // a + aC | ligation: a + aC <-> aaC
        |]
        |> Array.sum


    // 30 - aa
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            -2214.00742039413 * x.[6] * x.[30] * x.[103] // a + aa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            56.7694210357472 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            -56.7694210357472 * x.[6] * x.[30] * x.[190] // a + aa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            -2214.00742039413 * x.[30] * x.[103] // aa + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            2214.00742039413 * x.[6] * x.[6] * x.[103] // a + a + Bab | catalytic ligation: a + a + Bab <-> aa + Bab
            -56.7694210357472 * x.[30] * x.[190] // aa + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            56.7694210357472 * x.[6] * x.[6] * x.[190] // a + a + bAB | catalytic ligation: a + a + bAB <-> aa + bAB
            2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            -2214.00742039413 * x.[30] * x.[6] * x.[103] // aa + a + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            56.769421035747 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            -56.769421035747 * x.[30] * x.[6] * x.[190] // aa + a + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            -60.8364303415014 * x.[30] * x.[6] * x.[244] // aa + a + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            -2372.62078331855 * x.[30] * x.[6] * x.[121] // aa + a + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            -60.8364303415014 * x.[6] * x.[30] * x.[244] // a + aa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            -2372.62078331855 * x.[6] * x.[30] * x.[121] // a + aa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            -60.8364303415014 * x.[30] * x.[244] // aa + caB | catalytic ligation: a + a + caB <-> aa + caB
            60.8364303415014 * x.[6] * x.[6] * x.[244] // a + a + caB | catalytic ligation: a + a + caB <-> aa + caB
            -2372.62078331855 * x.[30] * x.[121] // aa + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            2372.62078331855 * x.[6] * x.[6] * x.[121] // a + a + CAb | catalytic ligation: a + a + CAb <-> aa + CAb
            60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: aa + b + caB <-> aab + caB
            -60.8364303415014 * x.[30] * x.[7] * x.[244] // aa + b + caB | catalytic ligation: aa + b + caB <-> aab + caB
            2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            -2372.62078331855 * x.[30] * x.[7] * x.[121] // aa + b + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            1.0 * x.[138] // Caa | ligation: C + aa <-> Caa
            -1.0 * x.[5] * x.[30] // C + aa | ligation: C + aa <-> Caa
            1.0 * x.[246] // caa | ligation: c + aa <-> caa
            -1.0 * x.[8] * x.[30] // c + aa | ligation: c + aa <-> caa
            1.0 * x.[102] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[30] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[210] // baa | ligation: b + aa <-> baa
            -1.0 * x.[7] * x.[30] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[173] // aaC | ligation: aa + C <-> aaC
            -1.0 * x.[30] * x.[5] // aa + C | ligation: aa + C <-> aaC
            1.0 * x.[172] // aaB | ligation: aa + B <-> aaB
            -1.0 * x.[30] * x.[4] // aa + B | ligation: aa + B <-> aaB
            1.0 * x.[171] // aaA | ligation: aa + A <-> aaA
            -1.0 * x.[30] * x.[3] // aa + A | ligation: aa + A <-> aaA
            1.0 * x.[176] // aac | ligation: aa + c <-> aac
            -1.0 * x.[30] * x.[8] // aa + c | ligation: aa + c <-> aac
            1.0 * x.[175] // aab | ligation: aa + b <-> aab
            -1.0 * x.[30] * x.[7] // aa + b | ligation: aa + b <-> aab
            1.0 * x.[174] // aaa | ligation: aa + a <-> aaa
            -1.0 * x.[30] * x.[6] // aa + a | ligation: aa + a <-> aaa
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
            2214.00742039413 * x.[175] * x.[103] // aab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            -2214.00742039413 * x.[6] * x.[31] * x.[103] // a + ab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            56.7694210357472 * x.[175] * x.[190] // aab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            -56.7694210357472 * x.[6] * x.[31] * x.[190] // a + ab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            2214.00742039413 * x.[182] * x.[103] // abc + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            -2214.00742039413 * x.[31] * x.[8] * x.[103] // ab + c + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            56.7694210357472 * x.[182] * x.[190] // abc + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            -56.7694210357472 * x.[31] * x.[8] * x.[190] // ab + c + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            -60.8364303415014 * x.[6] * x.[31] * x.[244] // a + ab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            -2372.62078331855 * x.[6] * x.[31] * x.[121] // a + ab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            -60.8364303415014 * x.[31] * x.[244] // ab + caB | catalytic ligation: a + b + caB <-> ab + caB
            60.8364303415014 * x.[6] * x.[7] * x.[244] // a + b + caB | catalytic ligation: a + b + caB <-> ab + caB
            -2372.62078331855 * x.[31] * x.[121] // ab + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            2372.62078331855 * x.[6] * x.[7] * x.[121] // a + b + CAb | catalytic ligation: a + b + CAb <-> ab + CAb
            1.0 * x.[139] // Cab | ligation: C + ab <-> Cab
            -1.0 * x.[5] * x.[31] // C + ab | ligation: C + ab <-> Cab
            1.0 * x.[247] // cab | ligation: c + ab <-> cab
            -1.0 * x.[8] * x.[31] // c + ab | ligation: c + ab <-> cab
            1.0 * x.[103] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[31] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[211] // bab | ligation: b + ab <-> bab
            -1.0 * x.[7] * x.[31] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[179] // abC | ligation: ab + C <-> abC
            -1.0 * x.[31] * x.[5] // ab + C | ligation: ab + C <-> abC
            1.0 * x.[178] // abB | ligation: ab + B <-> abB
            -1.0 * x.[31] * x.[4] // ab + B | ligation: ab + B <-> abB
            1.0 * x.[177] // abA | ligation: ab + A <-> abA
            -1.0 * x.[31] * x.[3] // ab + A | ligation: ab + A <-> abA
            1.0 * x.[182] // abc | ligation: ab + c <-> abc
            -1.0 * x.[31] * x.[8] // ab + c | ligation: ab + c <-> abc
            1.0 * x.[181] // abb | ligation: ab + b <-> abb
            -1.0 * x.[31] * x.[7] // ab + b | ligation: ab + b <-> abb
            1.0 * x.[180] // aba | ligation: ab + a <-> aba
            -1.0 * x.[31] * x.[6] // ab + a | ligation: ab + a <-> aba
            1.0 * x.[67] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[31] // A + ab | ligation: A + ab <-> Aab
            -1.0 * x.[31] // ab | ligation: a + b <-> ab
            1.0 * x.[6] * x.[7] // a + b | ligation: a + b <-> ab
            1.0 * x.[175] // aab | ligation: a + ab <-> aab
            -1.0 * x.[6] * x.[31] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 32 - ac
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[176] * x.[103] // aac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            -2214.00742039413 * x.[6] * x.[32] * x.[103] // a + ac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            56.7694210357472 * x.[176] * x.[190] // aac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            -56.7694210357472 * x.[6] * x.[32] * x.[190] // a + ac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            2214.00742039413 * x.[188] * x.[103] // acc + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            -2214.00742039413 * x.[32] * x.[8] * x.[103] // ac + c + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            56.7694210357472 * x.[188] * x.[190] // acc + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            -56.7694210357472 * x.[32] * x.[8] * x.[190] // ac + c + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            60.8364303415014 * x.[176] * x.[244] // aac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            -60.8364303415014 * x.[6] * x.[32] * x.[244] // a + ac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            2372.62078331855 * x.[176] * x.[121] // aac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            -2372.62078331855 * x.[6] * x.[32] * x.[121] // a + ac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            60.8364303415014 * x.[187] * x.[244] // acb + caB | catalytic ligation: ac + b + caB <-> acb + caB
            -60.8364303415014 * x.[32] * x.[7] * x.[244] // ac + b + caB | catalytic ligation: ac + b + caB <-> acb + caB
            2372.62078331855 * x.[187] * x.[121] // acb + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            -2372.62078331855 * x.[32] * x.[7] * x.[121] // ac + b + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            1.0 * x.[140] // Cac | ligation: C + ac <-> Cac
            -1.0 * x.[5] * x.[32] // C + ac | ligation: C + ac <-> Cac
            1.0 * x.[248] // cac | ligation: c + ac <-> cac
            -1.0 * x.[8] * x.[32] // c + ac | ligation: c + ac <-> cac
            1.0 * x.[104] // Bac | ligation: B + ac <-> Bac
            -1.0 * x.[4] * x.[32] // B + ac | ligation: B + ac <-> Bac
            1.0 * x.[212] // bac | ligation: b + ac <-> bac
            -1.0 * x.[7] * x.[32] // b + ac | ligation: b + ac <-> bac
            1.0 * x.[185] // acC | ligation: ac + C <-> acC
            -1.0 * x.[32] * x.[5] // ac + C | ligation: ac + C <-> acC
            1.0 * x.[184] // acB | ligation: ac + B <-> acB
            -1.0 * x.[32] * x.[4] // ac + B | ligation: ac + B <-> acB
            1.0 * x.[183] // acA | ligation: ac + A <-> acA
            -1.0 * x.[32] * x.[3] // ac + A | ligation: ac + A <-> acA
            1.0 * x.[188] // acc | ligation: ac + c <-> acc
            -1.0 * x.[32] * x.[8] // ac + c | ligation: ac + c <-> acc
            1.0 * x.[187] // acb | ligation: ac + b <-> acb
            -1.0 * x.[32] * x.[7] // ac + b | ligation: ac + b <-> acb
            1.0 * x.[186] // aca | ligation: ac + a <-> aca
            -1.0 * x.[32] * x.[6] // ac + a | ligation: ac + a <-> aca
            1.0 * x.[68] // Aac | ligation: A + ac <-> Aac
            -1.0 * x.[3] * x.[32] // A + ac | ligation: A + ac <-> Aac
            -1.0 * x.[32] // ac | ligation: a + c <-> ac
            1.0 * x.[6] * x.[8] // a + c | ligation: a + c <-> ac
            1.0 * x.[176] // aac | ligation: a + ac <-> aac
            -1.0 * x.[6] * x.[32] // a + ac | ligation: a + ac <-> aac
        |]
        |> Array.sum


    // 33 - bA
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[177] * x.[244] // abA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            -60.8364303415014 * x.[6] * x.[33] * x.[244] // a + bA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            2372.62078331855 * x.[177] * x.[121] // abA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            -2372.62078331855 * x.[6] * x.[33] * x.[121] // a + bA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            60.8364303415014 * x.[249] * x.[244] // cbA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            -60.8364303415014 * x.[8] * x.[33] * x.[244] // c + bA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            2372.62078331855 * x.[249] * x.[121] // cbA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            -2372.62078331855 * x.[8] * x.[33] * x.[121] // c + bA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            1.0 * x.[141] // CbA | ligation: C + bA <-> CbA
            -1.0 * x.[5] * x.[33] // C + bA | ligation: C + bA <-> CbA
            1.0 * x.[249] // cbA | ligation: c + bA <-> cbA
            -1.0 * x.[8] * x.[33] // c + bA | ligation: c + bA <-> cbA
            1.0 * x.[191] // bAC | ligation: bA + C <-> bAC
            -1.0 * x.[33] * x.[5] // bA + C | ligation: bA + C <-> bAC
            1.0 * x.[190] // bAB | ligation: bA + B <-> bAB
            -1.0 * x.[33] * x.[4] // bA + B | ligation: bA + B <-> bAB
            1.0 * x.[189] // bAA | ligation: bA + A <-> bAA
            -1.0 * x.[33] * x.[3] // bA + A | ligation: bA + A <-> bAA
            1.0 * x.[194] // bAc | ligation: bA + c <-> bAc
            -1.0 * x.[33] * x.[8] // bA + c | ligation: bA + c <-> bAc
            1.0 * x.[193] // bAb | ligation: bA + b <-> bAb
            -1.0 * x.[33] * x.[7] // bA + b | ligation: bA + b <-> bAb
            1.0 * x.[192] // bAa | ligation: bA + a <-> bAa
            -1.0 * x.[33] * x.[6] // bA + a | ligation: bA + a <-> bAa
            1.0 * x.[105] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[33] // B + bA | ligation: B + bA <-> BbA
            -1.0 * x.[33] // bA | ligation: b + A <-> bA
            1.0 * x.[7] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[213] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[7] * x.[33] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[69] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[33] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[177] // abA | ligation: a + bA <-> abA
            -1.0 * x.[6] * x.[33] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 34 - bB
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[178] * x.[244] // abB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            -60.8364303415014 * x.[6] * x.[34] * x.[244] // a + bB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            2372.62078331855 * x.[178] * x.[121] // abB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            -2372.62078331855 * x.[6] * x.[34] * x.[121] // a + bB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            60.8364303415014 * x.[250] * x.[244] // cbB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            -60.8364303415014 * x.[8] * x.[34] * x.[244] // c + bB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            2372.62078331855 * x.[250] * x.[121] // cbB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            -2372.62078331855 * x.[8] * x.[34] * x.[121] // c + bB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            1.0 * x.[142] // CbB | ligation: C + bB <-> CbB
            -1.0 * x.[5] * x.[34] // C + bB | ligation: C + bB <-> CbB
            1.0 * x.[250] // cbB | ligation: c + bB <-> cbB
            -1.0 * x.[8] * x.[34] // c + bB | ligation: c + bB <-> cbB
            1.0 * x.[197] // bBC | ligation: bB + C <-> bBC
            -1.0 * x.[34] * x.[5] // bB + C | ligation: bB + C <-> bBC
            1.0 * x.[196] // bBB | ligation: bB + B <-> bBB
            -1.0 * x.[34] * x.[4] // bB + B | ligation: bB + B <-> bBB
            1.0 * x.[195] // bBA | ligation: bB + A <-> bBA
            -1.0 * x.[34] * x.[3] // bB + A | ligation: bB + A <-> bBA
            1.0 * x.[200] // bBc | ligation: bB + c <-> bBc
            -1.0 * x.[34] * x.[8] // bB + c | ligation: bB + c <-> bBc
            1.0 * x.[199] // bBb | ligation: bB + b <-> bBb
            -1.0 * x.[34] * x.[7] // bB + b | ligation: bB + b <-> bBb
            1.0 * x.[198] // bBa | ligation: bB + a <-> bBa
            -1.0 * x.[34] * x.[6] // bB + a | ligation: bB + a <-> bBa
            1.0 * x.[106] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[34] // B + bB | ligation: B + bB <-> BbB
            -1.0 * x.[34] // bB | ligation: b + B <-> bB
            1.0 * x.[7] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[214] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[7] * x.[34] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[70] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[34] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[178] // abB | ligation: a + bB <-> abB
            -1.0 * x.[6] * x.[34] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 35 - bC
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            60.8364303415014 * x.[179] * x.[244] // abC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            -60.8364303415014 * x.[6] * x.[35] * x.[244] // a + bC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            2372.62078331855 * x.[179] * x.[121] // abC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            -2372.62078331855 * x.[6] * x.[35] * x.[121] // a + bC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            60.8364303415014 * x.[251] * x.[244] // cbC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            -60.8364303415014 * x.[8] * x.[35] * x.[244] // c + bC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            2372.62078331855 * x.[251] * x.[121] // cbC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            -2372.62078331855 * x.[8] * x.[35] * x.[121] // c + bC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            1.0 * x.[143] // CbC | ligation: C + bC <-> CbC
            -1.0 * x.[5] * x.[35] // C + bC | ligation: C + bC <-> CbC
            1.0 * x.[251] // cbC | ligation: c + bC <-> cbC
            -1.0 * x.[8] * x.[35] // c + bC | ligation: c + bC <-> cbC
            1.0 * x.[203] // bCC | ligation: bC + C <-> bCC
            -1.0 * x.[35] * x.[5] // bC + C | ligation: bC + C <-> bCC
            1.0 * x.[202] // bCB | ligation: bC + B <-> bCB
            -1.0 * x.[35] * x.[4] // bC + B | ligation: bC + B <-> bCB
            1.0 * x.[201] // bCA | ligation: bC + A <-> bCA
            -1.0 * x.[35] * x.[3] // bC + A | ligation: bC + A <-> bCA
            1.0 * x.[206] // bCc | ligation: bC + c <-> bCc
            -1.0 * x.[35] * x.[8] // bC + c | ligation: bC + c <-> bCc
            1.0 * x.[205] // bCb | ligation: bC + b <-> bCb
            -1.0 * x.[35] * x.[7] // bC + b | ligation: bC + b <-> bCb
            1.0 * x.[204] // bCa | ligation: bC + a <-> bCa
            -1.0 * x.[35] * x.[6] // bC + a | ligation: bC + a <-> bCa
            -1.0 * x.[35] // bC | ligation: b + C <-> bC
            1.0 * x.[7] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[107] // BbC | ligation: B + bC <-> BbC
            -1.0 * x.[4] * x.[35] // B + bC | ligation: B + bC <-> BbC
            1.0 * x.[215] // bbC | ligation: b + bC <-> bbC
            -1.0 * x.[7] * x.[35] // b + bC | ligation: b + bC <-> bbC
            1.0 * x.[71] // AbC | ligation: A + bC <-> AbC
            -1.0 * x.[3] * x.[35] // A + bC | ligation: A + bC <-> AbC
            1.0 * x.[179] // abC | ligation: a + bC <-> abC
            -1.0 * x.[6] * x.[35] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 36 - ba
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[210] * x.[103] // baa + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            -2214.00742039413 * x.[36] * x.[6] * x.[103] // ba + a + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            56.7694210357472 * x.[210] * x.[190] // baa + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            -56.7694210357472 * x.[36] * x.[6] * x.[190] // ba + a + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            60.8364303415014 * x.[210] * x.[244] // baa + caB | catalytic ligation: ba + a + caB <-> baa + caB
            -60.8364303415014 * x.[36] * x.[6] * x.[244] // ba + a + caB | catalytic ligation: ba + a + caB <-> baa + caB
            2372.62078331855 * x.[210] * x.[121] // baa + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            -2372.62078331855 * x.[36] * x.[6] * x.[121] // ba + a + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            60.8364303415014 * x.[211] * x.[244] // bab + caB | catalytic ligation: ba + b + caB <-> bab + caB
            -60.8364303415014 * x.[36] * x.[7] * x.[244] // ba + b + caB | catalytic ligation: ba + b + caB <-> bab + caB
            2372.62078331855 * x.[211] * x.[121] // bab + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            -2372.62078331855 * x.[36] * x.[7] * x.[121] // ba + b + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            60.8364303415014 * x.[180] * x.[244] // aba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            -60.8364303415014 * x.[6] * x.[36] * x.[244] // a + ba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            2372.62078331855 * x.[180] * x.[121] // aba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            -2372.62078331855 * x.[6] * x.[36] * x.[121] // a + ba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            60.8364303415014 * x.[252] * x.[244] // cba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            -60.8364303415014 * x.[8] * x.[36] * x.[244] // c + ba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            2372.62078331855 * x.[252] * x.[121] // cba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            -2372.62078331855 * x.[8] * x.[36] * x.[121] // c + ba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            1.0 * x.[144] // Cba | ligation: C + ba <-> Cba
            -1.0 * x.[5] * x.[36] // C + ba | ligation: C + ba <-> Cba
            1.0 * x.[252] // cba | ligation: c + ba <-> cba
            -1.0 * x.[8] * x.[36] // c + ba | ligation: c + ba <-> cba
            1.0 * x.[209] // baC | ligation: ba + C <-> baC
            -1.0 * x.[36] * x.[5] // ba + C | ligation: ba + C <-> baC
            1.0 * x.[208] // baB | ligation: ba + B <-> baB
            -1.0 * x.[36] * x.[4] // ba + B | ligation: ba + B <-> baB
            1.0 * x.[207] // baA | ligation: ba + A <-> baA
            -1.0 * x.[36] * x.[3] // ba + A | ligation: ba + A <-> baA
            1.0 * x.[212] // bac | ligation: ba + c <-> bac
            -1.0 * x.[36] * x.[8] // ba + c | ligation: ba + c <-> bac
            1.0 * x.[211] // bab | ligation: ba + b <-> bab
            -1.0 * x.[36] * x.[7] // ba + b | ligation: ba + b <-> bab
            1.0 * x.[210] // baa | ligation: ba + a <-> baa
            -1.0 * x.[36] * x.[6] // ba + a | ligation: ba + a <-> baa
            1.0 * x.[108] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[36] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[216] // bba | ligation: b + ba <-> bba
            -1.0 * x.[7] * x.[36] // b + ba | ligation: b + ba <-> bba
            -1.0 * x.[36] // ba | ligation: b + a <-> ba
            1.0 * x.[7] * x.[6] // b + a | ligation: b + a <-> ba
            1.0 * x.[72] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[36] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[180] // aba | ligation: a + ba <-> aba
            -1.0 * x.[6] * x.[36] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 37 - bb
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[218] * x.[103] // bbc + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            -2214.00742039413 * x.[37] * x.[8] * x.[103] // bb + c + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            56.7694210357472 * x.[218] * x.[190] // bbc + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            -56.7694210357472 * x.[37] * x.[8] * x.[190] // bb + c + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            60.8364303415014 * x.[181] * x.[244] // abb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            -60.8364303415014 * x.[6] * x.[37] * x.[244] // a + bb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            2372.62078331855 * x.[181] * x.[121] // abb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            -2372.62078331855 * x.[6] * x.[37] * x.[121] // a + bb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            60.8364303415014 * x.[253] * x.[244] // cbb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            -60.8364303415014 * x.[8] * x.[37] * x.[244] // c + bb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            2372.62078331855 * x.[253] * x.[121] // cbb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            -2372.62078331855 * x.[8] * x.[37] * x.[121] // c + bb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            1.0 * x.[145] // Cbb | ligation: C + bb <-> Cbb
            -1.0 * x.[5] * x.[37] // C + bb | ligation: C + bb <-> Cbb
            1.0 * x.[253] // cbb | ligation: c + bb <-> cbb
            -1.0 * x.[8] * x.[37] // c + bb | ligation: c + bb <-> cbb
            1.0 * x.[215] // bbC | ligation: bb + C <-> bbC
            -1.0 * x.[37] * x.[5] // bb + C | ligation: bb + C <-> bbC
            1.0 * x.[214] // bbB | ligation: bb + B <-> bbB
            -1.0 * x.[37] * x.[4] // bb + B | ligation: bb + B <-> bbB
            1.0 * x.[213] // bbA | ligation: bb + A <-> bbA
            -1.0 * x.[37] * x.[3] // bb + A | ligation: bb + A <-> bbA
            1.0 * x.[218] // bbc | ligation: bb + c <-> bbc
            -1.0 * x.[37] * x.[8] // bb + c | ligation: bb + c <-> bbc
            1.0 * x.[217] // bbb | ligation: bb + b <-> bbb
            -1.0 * x.[37] * x.[7] // bb + b | ligation: bb + b <-> bbb
            1.0 * x.[216] // bba | ligation: bb + a <-> bba
            -1.0 * x.[37] * x.[6] // bb + a | ligation: bb + a <-> bba
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
            2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            -2214.00742039413 * x.[38] * x.[8] * x.[103] // bc + c + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            -56.7694210357472 * x.[38] * x.[8] * x.[190] // bc + c + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            -2214.00742039413 * x.[38] * x.[103] // bc + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            2214.00742039413 * x.[7] * x.[8] * x.[103] // b + c + Bab | catalytic ligation: b + c + Bab <-> bc + Bab
            -56.7694210357472 * x.[38] * x.[190] // bc + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            56.7694210357472 * x.[7] * x.[8] * x.[190] // b + c + bAB | catalytic ligation: b + c + bAB <-> bc + bAB
            60.8364303415014 * x.[182] * x.[244] // abc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            -60.8364303415014 * x.[6] * x.[38] * x.[244] // a + bc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            2372.62078331855 * x.[182] * x.[121] // abc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            -2372.62078331855 * x.[6] * x.[38] * x.[121] // a + bc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            60.8364303415014 * x.[223] * x.[244] // bcb + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            -60.8364303415014 * x.[38] * x.[7] * x.[244] // bc + b + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            2372.62078331855 * x.[223] * x.[121] // bcb + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            -2372.62078331855 * x.[38] * x.[7] * x.[121] // bc + b + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            60.8364303415014 * x.[254] * x.[244] // cbc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            -60.8364303415014 * x.[8] * x.[38] * x.[244] // c + bc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            2372.62078331855 * x.[254] * x.[121] // cbc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            -2372.62078331855 * x.[8] * x.[38] * x.[121] // c + bc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            1.0 * x.[146] // Cbc | ligation: C + bc <-> Cbc
            -1.0 * x.[5] * x.[38] // C + bc | ligation: C + bc <-> Cbc
            1.0 * x.[254] // cbc | ligation: c + bc <-> cbc
            -1.0 * x.[8] * x.[38] // c + bc | ligation: c + bc <-> cbc
            1.0 * x.[221] // bcC | ligation: bc + C <-> bcC
            -1.0 * x.[38] * x.[5] // bc + C | ligation: bc + C <-> bcC
            1.0 * x.[220] // bcB | ligation: bc + B <-> bcB
            -1.0 * x.[38] * x.[4] // bc + B | ligation: bc + B <-> bcB
            1.0 * x.[219] // bcA | ligation: bc + A <-> bcA
            -1.0 * x.[38] * x.[3] // bc + A | ligation: bc + A <-> bcA
            1.0 * x.[224] // bcc | ligation: bc + c <-> bcc
            -1.0 * x.[38] * x.[8] // bc + c | ligation: bc + c <-> bcc
            1.0 * x.[223] // bcb | ligation: bc + b <-> bcb
            -1.0 * x.[38] * x.[7] // bc + b | ligation: bc + b <-> bcb
            1.0 * x.[222] // bca | ligation: bc + a <-> bca
            -1.0 * x.[38] * x.[6] // bc + a | ligation: bc + a <-> bca
            1.0 * x.[110] // Bbc | ligation: B + bc <-> Bbc
            -1.0 * x.[4] * x.[38] // B + bc | ligation: B + bc <-> Bbc
            -1.0 * x.[38] // bc | ligation: b + c <-> bc
            1.0 * x.[7] * x.[8] // b + c | ligation: b + c <-> bc
            1.0 * x.[218] // bbc | ligation: b + bc <-> bbc
            -1.0 * x.[7] * x.[38] // b + bc | ligation: b + bc <-> bbc
            1.0 * x.[74] // Abc | ligation: A + bc <-> Abc
            -1.0 * x.[3] * x.[38] // A + bc | ligation: A + bc <-> Abc
            1.0 * x.[182] // abc | ligation: a + bc <-> abc
            -1.0 * x.[6] * x.[38] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 39 - cA
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            2214.00742039413 * x.[255] * x.[103] // ccA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            -2214.00742039413 * x.[8] * x.[39] * x.[103] // c + cA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            56.7694210357472 * x.[255] * x.[190] // ccA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            -56.7694210357472 * x.[8] * x.[39] * x.[190] // c + cA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            2214.00742039413 * x.[219] * x.[103] // bcA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            -2214.00742039413 * x.[7] * x.[39] * x.[103] // b + cA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            56.7694210357472 * x.[219] * x.[190] // bcA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            -56.7694210357472 * x.[7] * x.[39] * x.[190] // b + cA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            1.0 * x.[227] // cAC | ligation: cA + C <-> cAC
            -1.0 * x.[39] * x.[5] // cA + C | ligation: cA + C <-> cAC
            1.0 * x.[226] // cAB | ligation: cA + B <-> cAB
            -1.0 * x.[39] * x.[4] // cA + B | ligation: cA + B <-> cAB
            1.0 * x.[225] // cAA | ligation: cA + A <-> cAA
            -1.0 * x.[39] * x.[3] // cA + A | ligation: cA + A <-> cAA
            1.0 * x.[230] // cAc | ligation: cA + c <-> cAc
            -1.0 * x.[39] * x.[8] // cA + c | ligation: cA + c <-> cAc
            1.0 * x.[229] // cAb | ligation: cA + b <-> cAb
            -1.0 * x.[39] * x.[7] // cA + b | ligation: cA + b <-> cAb
            1.0 * x.[228] // cAa | ligation: cA + a <-> cAa
            -1.0 * x.[39] * x.[6] // cA + a | ligation: cA + a <-> cAa
            1.0 * x.[147] // CcA | ligation: C + cA <-> CcA
            -1.0 * x.[5] * x.[39] // C + cA | ligation: C + cA <-> CcA
            -1.0 * x.[39] // cA | ligation: c + A <-> cA
            1.0 * x.[8] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[255] // ccA | ligation: c + cA <-> ccA
            -1.0 * x.[8] * x.[39] // c + cA | ligation: c + cA <-> ccA
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
            2214.00742039413 * x.[256] * x.[103] // ccB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            -2214.00742039413 * x.[8] * x.[40] * x.[103] // c + cB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            56.7694210357472 * x.[256] * x.[190] // ccB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            -56.7694210357472 * x.[8] * x.[40] * x.[190] // c + cB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            2214.00742039413 * x.[220] * x.[103] // bcB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            -2214.00742039413 * x.[7] * x.[40] * x.[103] // b + cB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            56.7694210357472 * x.[220] * x.[190] // bcB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            -56.7694210357472 * x.[7] * x.[40] * x.[190] // b + cB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            1.0 * x.[233] // cBC | ligation: cB + C <-> cBC
            -1.0 * x.[40] * x.[5] // cB + C | ligation: cB + C <-> cBC
            1.0 * x.[232] // cBB | ligation: cB + B <-> cBB
            -1.0 * x.[40] * x.[4] // cB + B | ligation: cB + B <-> cBB
            1.0 * x.[231] // cBA | ligation: cB + A <-> cBA
            -1.0 * x.[40] * x.[3] // cB + A | ligation: cB + A <-> cBA
            1.0 * x.[236] // cBc | ligation: cB + c <-> cBc
            -1.0 * x.[40] * x.[8] // cB + c | ligation: cB + c <-> cBc
            1.0 * x.[235] // cBb | ligation: cB + b <-> cBb
            -1.0 * x.[40] * x.[7] // cB + b | ligation: cB + b <-> cBb
            1.0 * x.[234] // cBa | ligation: cB + a <-> cBa
            -1.0 * x.[40] * x.[6] // cB + a | ligation: cB + a <-> cBa
            1.0 * x.[148] // CcB | ligation: C + cB <-> CcB
            -1.0 * x.[5] * x.[40] // C + cB | ligation: C + cB <-> CcB
            -1.0 * x.[40] // cB | ligation: c + B <-> cB
            1.0 * x.[8] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[256] // ccB | ligation: c + cB <-> ccB
            -1.0 * x.[8] * x.[40] // c + cB | ligation: c + cB <-> ccB
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
            2214.00742039413 * x.[257] * x.[103] // ccC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            -2214.00742039413 * x.[8] * x.[41] * x.[103] // c + cC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            56.7694210357472 * x.[257] * x.[190] // ccC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            -56.7694210357472 * x.[8] * x.[41] * x.[190] // c + cC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            2214.00742039413 * x.[221] * x.[103] // bcC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            -2214.00742039413 * x.[7] * x.[41] * x.[103] // b + cC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            56.7694210357472 * x.[221] * x.[190] // bcC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            -56.7694210357472 * x.[7] * x.[41] * x.[190] // b + cC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            1.0 * x.[239] // cCC | ligation: cC + C <-> cCC
            -1.0 * x.[41] * x.[5] // cC + C | ligation: cC + C <-> cCC
            1.0 * x.[238] // cCB | ligation: cC + B <-> cCB
            -1.0 * x.[41] * x.[4] // cC + B | ligation: cC + B <-> cCB
            1.0 * x.[237] // cCA | ligation: cC + A <-> cCA
            -1.0 * x.[41] * x.[3] // cC + A | ligation: cC + A <-> cCA
            1.0 * x.[242] // cCc | ligation: cC + c <-> cCc
            -1.0 * x.[41] * x.[8] // cC + c | ligation: cC + c <-> cCc
            1.0 * x.[241] // cCb | ligation: cC + b <-> cCb
            -1.0 * x.[41] * x.[7] // cC + b | ligation: cC + b <-> cCb
            1.0 * x.[240] // cCa | ligation: cC + a <-> cCa
            -1.0 * x.[41] * x.[6] // cC + a | ligation: cC + a <-> cCa
            1.0 * x.[149] // CcC | ligation: C + cC <-> CcC
            -1.0 * x.[5] * x.[41] // C + cC | ligation: C + cC <-> CcC
            -1.0 * x.[41] // cC | ligation: c + C <-> cC
            1.0 * x.[8] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[257] // ccC | ligation: c + cC <-> ccC
            -1.0 * x.[8] * x.[41] // c + cC | ligation: c + cC <-> ccC
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
            2214.00742039413 * x.[246] * x.[103] // caa + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            -2214.00742039413 * x.[42] * x.[6] * x.[103] // ca + a + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            56.7694210357472 * x.[246] * x.[190] // caa + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            -56.7694210357472 * x.[42] * x.[6] * x.[190] // ca + a + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            2214.00742039413 * x.[258] * x.[103] // cca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            -2214.00742039413 * x.[8] * x.[42] * x.[103] // c + ca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            56.7694210357472 * x.[258] * x.[190] // cca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            -56.7694210357472 * x.[8] * x.[42] * x.[190] // c + ca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            2214.00742039413 * x.[222] * x.[103] // bca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            -2214.00742039413 * x.[7] * x.[42] * x.[103] // b + ca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            56.7694210357472 * x.[222] * x.[190] // bca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            -56.7694210357472 * x.[7] * x.[42] * x.[190] // b + ca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            60.8364303415014 * x.[246] * x.[244] // caa + caB | catalytic ligation: ca + a + caB <-> caa + caB
            -60.8364303415014 * x.[42] * x.[6] * x.[244] // ca + a + caB | catalytic ligation: ca + a + caB <-> caa + caB
            2372.62078331855 * x.[246] * x.[121] // caa + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            -2372.62078331855 * x.[42] * x.[6] * x.[121] // ca + a + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            60.8364303415014 * x.[247] * x.[244] // cab + caB | catalytic ligation: ca + b + caB <-> cab + caB
            -60.8364303415014 * x.[42] * x.[7] * x.[244] // ca + b + caB | catalytic ligation: ca + b + caB <-> cab + caB
            2372.62078331855 * x.[247] * x.[121] // cab + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            -2372.62078331855 * x.[42] * x.[7] * x.[121] // ca + b + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            1.0 * x.[245] // caC | ligation: ca + C <-> caC
            -1.0 * x.[42] * x.[5] // ca + C | ligation: ca + C <-> caC
            1.0 * x.[244] // caB | ligation: ca + B <-> caB
            -1.0 * x.[42] * x.[4] // ca + B | ligation: ca + B <-> caB
            1.0 * x.[243] // caA | ligation: ca + A <-> caA
            -1.0 * x.[42] * x.[3] // ca + A | ligation: ca + A <-> caA
            1.0 * x.[248] // cac | ligation: ca + c <-> cac
            -1.0 * x.[42] * x.[8] // ca + c | ligation: ca + c <-> cac
            1.0 * x.[247] // cab | ligation: ca + b <-> cab
            -1.0 * x.[42] * x.[7] // ca + b | ligation: ca + b <-> cab
            1.0 * x.[246] // caa | ligation: ca + a <-> caa
            -1.0 * x.[42] * x.[6] // ca + a | ligation: ca + a <-> caa
            1.0 * x.[150] // Cca | ligation: C + ca <-> Cca
            -1.0 * x.[5] * x.[42] // C + ca | ligation: C + ca <-> Cca
            1.0 * x.[258] // cca | ligation: c + ca <-> cca
            -1.0 * x.[8] * x.[42] // c + ca | ligation: c + ca <-> cca
            -1.0 * x.[42] // ca | ligation: c + a <-> ca
            1.0 * x.[8] * x.[6] // c + a | ligation: c + a <-> ca
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
            2214.00742039413 * x.[259] * x.[103] // ccb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            -2214.00742039413 * x.[8] * x.[43] * x.[103] // c + cb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            56.7694210357472 * x.[259] * x.[190] // ccb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            -56.7694210357472 * x.[8] * x.[43] * x.[190] // c + cb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            2214.00742039413 * x.[254] * x.[103] // cbc + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            -2214.00742039413 * x.[43] * x.[8] * x.[103] // cb + c + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            56.7694210357472 * x.[254] * x.[190] // cbc + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            -56.7694210357472 * x.[43] * x.[8] * x.[190] // cb + c + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            2214.00742039413 * x.[223] * x.[103] // bcb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            -2214.00742039413 * x.[7] * x.[43] * x.[103] // b + cb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            56.7694210357472 * x.[223] * x.[190] // bcb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            -56.7694210357472 * x.[7] * x.[43] * x.[190] // b + cb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            -60.8364303415014 * x.[43] * x.[244] // cb + caB | catalytic ligation: c + b + caB <-> cb + caB
            60.8364303415014 * x.[8] * x.[7] * x.[244] // c + b + caB | catalytic ligation: c + b + caB <-> cb + caB
            -2372.62078331855 * x.[43] * x.[121] // cb + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            2372.62078331855 * x.[8] * x.[7] * x.[121] // c + b + CAb | catalytic ligation: c + b + CAb <-> cb + CAb
            1.0 * x.[251] // cbC | ligation: cb + C <-> cbC
            -1.0 * x.[43] * x.[5] // cb + C | ligation: cb + C <-> cbC
            1.0 * x.[250] // cbB | ligation: cb + B <-> cbB
            -1.0 * x.[43] * x.[4] // cb + B | ligation: cb + B <-> cbB
            1.0 * x.[249] // cbA | ligation: cb + A <-> cbA
            -1.0 * x.[43] * x.[3] // cb + A | ligation: cb + A <-> cbA
            1.0 * x.[254] // cbc | ligation: cb + c <-> cbc
            -1.0 * x.[43] * x.[8] // cb + c | ligation: cb + c <-> cbc
            1.0 * x.[253] // cbb | ligation: cb + b <-> cbb
            -1.0 * x.[43] * x.[7] // cb + b | ligation: cb + b <-> cbb
            1.0 * x.[252] // cba | ligation: cb + a <-> cba
            -1.0 * x.[43] * x.[6] // cb + a | ligation: cb + a <-> cba
            1.0 * x.[151] // Ccb | ligation: C + cb <-> Ccb
            -1.0 * x.[5] * x.[43] // C + cb | ligation: C + cb <-> Ccb
            1.0 * x.[259] // ccb | ligation: c + cb <-> ccb
            -1.0 * x.[8] * x.[43] // c + cb | ligation: c + cb <-> ccb
            -1.0 * x.[43] // cb | ligation: c + b <-> cb
            1.0 * x.[8] * x.[7] // c + b | ligation: c + b <-> cb
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
            2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            -2214.00742039413 * x.[44] * x.[8] * x.[103] // cc + c + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            -56.7694210357472 * x.[44] * x.[8] * x.[190] // cc + c + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            -2214.00742039413 * x.[8] * x.[44] * x.[103] // c + cc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            -56.7694210357472 * x.[8] * x.[44] * x.[190] // c + cc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            -2214.00742039413 * x.[44] * x.[103] // cc + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            2214.00742039413 * x.[8] * x.[8] * x.[103] // c + c + Bab | catalytic ligation: c + c + Bab <-> cc + Bab
            -56.7694210357472 * x.[44] * x.[190] // cc + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            56.7694210357472 * x.[8] * x.[8] * x.[190] // c + c + bAB | catalytic ligation: c + c + bAB <-> cc + bAB
            2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            -2214.00742039413 * x.[7] * x.[44] * x.[103] // b + cc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            -56.7694210357472 * x.[7] * x.[44] * x.[190] // b + cc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            60.8364303415013 * x.[259] * x.[244] // ccb + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            -60.8364303415013 * x.[44] * x.[7] * x.[244] // cc + b + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            2372.62078331855 * x.[259] * x.[121] // ccb + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            -2372.62078331855 * x.[44] * x.[7] * x.[121] // cc + b + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            1.0 * x.[257] // ccC | ligation: cc + C <-> ccC
            -1.0 * x.[44] * x.[5] // cc + C | ligation: cc + C <-> ccC
            1.0 * x.[256] // ccB | ligation: cc + B <-> ccB
            -1.0 * x.[44] * x.[4] // cc + B | ligation: cc + B <-> ccB
            1.0 * x.[255] // ccA | ligation: cc + A <-> ccA
            -1.0 * x.[44] * x.[3] // cc + A | ligation: cc + A <-> ccA
            1.0 * x.[260] // ccc | ligation: cc + c <-> ccc
            -1.0 * x.[44] * x.[8] // cc + c | ligation: cc + c <-> ccc
            1.0 * x.[259] // ccb | ligation: cc + b <-> ccb
            -1.0 * x.[44] * x.[7] // cc + b | ligation: cc + b <-> ccb
            1.0 * x.[258] // cca | ligation: cc + a <-> cca
            -1.0 * x.[44] * x.[6] // cc + a | ligation: cc + a <-> cca
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
            -2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            2214.00742039413 * x.[3] * x.[9] * x.[190] // A + AA + bAB | catalytic ligation: A + AA + bAB <-> AAA + bAB
            -56.7694210357472 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            56.7694210357472 * x.[3] * x.[9] * x.[103] // A + AA + Bab | catalytic ligation: A + AA + Bab <-> AAA + Bab
            -2214.00742039413 * x.[45] * x.[190] // AAA + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            2214.00742039413 * x.[9] * x.[3] * x.[190] // AA + A + bAB | catalytic ligation: AA + A + bAB <-> AAA + bAB
            -56.769421035747 * x.[45] * x.[103] // AAA + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            56.769421035747 * x.[9] * x.[3] * x.[103] // AA + A + Bab | catalytic ligation: AA + A + Bab <-> AAA + Bab
            -60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            60.8364303415014 * x.[9] * x.[3] * x.[121] // AA + A + CAb | catalytic ligation: AA + A + CAb <-> AAA + CAb
            -2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            2372.62078331855 * x.[9] * x.[3] * x.[244] // AA + A + caB | catalytic ligation: AA + A + caB <-> AAA + caB
            -60.8364303415014 * x.[45] * x.[121] // AAA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            60.8364303415014 * x.[3] * x.[9] * x.[121] // A + AA + CAb | catalytic ligation: A + AA + CAb <-> AAA + CAb
            -2372.62078331855 * x.[45] * x.[244] // AAA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            2372.62078331855 * x.[3] * x.[9] * x.[244] // A + AA + caB | catalytic ligation: A + AA + caB <-> AAA + caB
            -1.0 * x.[45] // AAA | ligation: AA + A <-> AAA
            1.0 * x.[9] * x.[3] // AA + A | ligation: AA + A <-> AAA
            -1.0 * x.[45] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[9] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 46 - AAB
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[46] * x.[190] // AAB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            2214.00742039413 * x.[3] * x.[10] * x.[190] // A + AB + bAB | catalytic ligation: A + AB + bAB <-> AAB + bAB
            -56.7694210357472 * x.[46] * x.[103] // AAB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            56.7694210357472 * x.[3] * x.[10] * x.[103] // A + AB + Bab | catalytic ligation: A + AB + Bab <-> AAB + Bab
            -60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            60.8364303415014 * x.[3] * x.[10] * x.[121] // A + AB + CAb | catalytic ligation: A + AB + CAb <-> AAB + CAb
            -2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            2372.62078331855 * x.[3] * x.[10] * x.[244] // A + AB + caB | catalytic ligation: A + AB + caB <-> AAB + caB
            -60.8364303415014 * x.[46] * x.[121] // AAB + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            60.8364303415014 * x.[9] * x.[4] * x.[121] // AA + B + CAb | catalytic ligation: AA + B + CAb <-> AAB + CAb
            -2372.62078331855 * x.[46] * x.[244] // AAB + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            2372.62078331855 * x.[9] * x.[4] * x.[244] // AA + B + caB | catalytic ligation: AA + B + caB <-> AAB + caB
            -1.0 * x.[46] // AAB | ligation: AA + B <-> AAB
            1.0 * x.[9] * x.[4] // AA + B | ligation: AA + B <-> AAB
            -1.0 * x.[46] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[10] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 47 - AAC
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[47] * x.[190] // AAC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            2214.00742039413 * x.[3] * x.[11] * x.[190] // A + AC + bAB | catalytic ligation: A + AC + bAB <-> AAC + bAB
            -56.7694210357472 * x.[47] * x.[103] // AAC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            56.7694210357472 * x.[3] * x.[11] * x.[103] // A + AC + Bab | catalytic ligation: A + AC + Bab <-> AAC + Bab
            -60.8364303415014 * x.[47] * x.[121] // AAC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            60.8364303415014 * x.[3] * x.[11] * x.[121] // A + AC + CAb | catalytic ligation: A + AC + CAb <-> AAC + CAb
            -2372.62078331855 * x.[47] * x.[244] // AAC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            2372.62078331855 * x.[3] * x.[11] * x.[244] // A + AC + caB | catalytic ligation: A + AC + caB <-> AAC + caB
            -1.0 * x.[47] // AAC | ligation: AA + C <-> AAC
            1.0 * x.[9] * x.[5] // AA + C | ligation: AA + C <-> AAC
            -1.0 * x.[47] // AAC | ligation: A + AC <-> AAC
            1.0 * x.[3] * x.[11] // A + AC | ligation: A + AC <-> AAC
        |]
        |> Array.sum


    // 48 - AAa
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[48] * x.[190] // AAa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            2214.00742039413 * x.[3] * x.[12] * x.[190] // A + Aa + bAB | catalytic ligation: A + Aa + bAB <-> AAa + bAB
            -56.7694210357472 * x.[48] * x.[103] // AAa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            56.7694210357472 * x.[3] * x.[12] * x.[103] // A + Aa + Bab | catalytic ligation: A + Aa + Bab <-> AAa + Bab
            -60.8364303415014 * x.[48] * x.[121] // AAa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            60.8364303415014 * x.[3] * x.[12] * x.[121] // A + Aa + CAb | catalytic ligation: A + Aa + CAb <-> AAa + CAb
            -2372.62078331855 * x.[48] * x.[244] // AAa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            2372.62078331855 * x.[3] * x.[12] * x.[244] // A + Aa + caB | catalytic ligation: A + Aa + caB <-> AAa + caB
            -1.0 * x.[48] // AAa | ligation: AA + a <-> AAa
            1.0 * x.[9] * x.[6] // AA + a | ligation: AA + a <-> AAa
            -1.0 * x.[48] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[12] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 49 - AAb
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[49] * x.[190] // AAb + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            2214.00742039413 * x.[3] * x.[13] * x.[190] // A + Ab + bAB | catalytic ligation: A + Ab + bAB <-> AAb + bAB
            -56.7694210357472 * x.[49] * x.[103] // AAb + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            56.7694210357472 * x.[3] * x.[13] * x.[103] // A + Ab + Bab | catalytic ligation: A + Ab + Bab <-> AAb + Bab
            -60.8364303415014 * x.[49] * x.[121] // AAb + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            60.8364303415014 * x.[3] * x.[13] * x.[121] // A + Ab + CAb | catalytic ligation: A + Ab + CAb <-> AAb + CAb
            -2372.62078331855 * x.[49] * x.[244] // AAb + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            2372.62078331855 * x.[3] * x.[13] * x.[244] // A + Ab + caB | catalytic ligation: A + Ab + caB <-> AAb + caB
            -1.0 * x.[49] // AAb | ligation: AA + b <-> AAb
            1.0 * x.[9] * x.[7] // AA + b | ligation: AA + b <-> AAb
            -1.0 * x.[49] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[13] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 50 - AAc
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[50] * x.[190] // AAc + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            2214.00742039413 * x.[3] * x.[14] * x.[190] // A + Ac + bAB | catalytic ligation: A + Ac + bAB <-> AAc + bAB
            -56.7694210357472 * x.[50] * x.[103] // AAc + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            56.7694210357472 * x.[3] * x.[14] * x.[103] // A + Ac + Bab | catalytic ligation: A + Ac + Bab <-> AAc + Bab
            -60.8364303415014 * x.[50] * x.[121] // AAc + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            60.8364303415014 * x.[3] * x.[14] * x.[121] // A + Ac + CAb | catalytic ligation: A + Ac + CAb <-> AAc + CAb
            -2372.62078331855 * x.[50] * x.[244] // AAc + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            2372.62078331855 * x.[3] * x.[14] * x.[244] // A + Ac + caB | catalytic ligation: A + Ac + caB <-> AAc + caB
            -1.0 * x.[50] // AAc | ligation: AA + c <-> AAc
            1.0 * x.[9] * x.[8] // AA + c | ligation: AA + c <-> AAc
            -1.0 * x.[50] // AAc | ligation: A + Ac <-> AAc
            1.0 * x.[3] * x.[14] // A + Ac | ligation: A + Ac <-> AAc
        |]
        |> Array.sum


    // 51 - ABA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[51] * x.[121] // ABA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            60.8364303415014 * x.[3] * x.[15] * x.[121] // A + BA + CAb | catalytic ligation: A + BA + CAb <-> ABA + CAb
            -2372.62078331855 * x.[51] * x.[244] // ABA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            2372.62078331855 * x.[3] * x.[15] * x.[244] // A + BA + caB | catalytic ligation: A + BA + caB <-> ABA + caB
            -1.0 * x.[51] // ABA | ligation: AB + A <-> ABA
            1.0 * x.[10] * x.[3] // AB + A | ligation: AB + A <-> ABA
            -1.0 * x.[51] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[15] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 52 - ABB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[52] * x.[121] // ABB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            60.8364303415014 * x.[3] * x.[16] * x.[121] // A + BB + CAb | catalytic ligation: A + BB + CAb <-> ABB + CAb
            -2372.62078331855 * x.[52] * x.[244] // ABB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            2372.62078331855 * x.[3] * x.[16] * x.[244] // A + BB + caB | catalytic ligation: A + BB + caB <-> ABB + caB
            -1.0 * x.[52] // ABB | ligation: AB + B <-> ABB
            1.0 * x.[10] * x.[4] // AB + B | ligation: AB + B <-> ABB
            -1.0 * x.[52] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[16] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 53 - ABC
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[53] * x.[190] // ABC + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            2214.00742039413 * x.[10] * x.[5] * x.[190] // AB + C + bAB | catalytic ligation: AB + C + bAB <-> ABC + bAB
            -56.7694210357472 * x.[53] * x.[103] // ABC + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            56.7694210357472 * x.[10] * x.[5] * x.[103] // AB + C + Bab | catalytic ligation: AB + C + Bab <-> ABC + Bab
            -60.8364303415014 * x.[53] * x.[121] // ABC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            60.8364303415014 * x.[3] * x.[17] * x.[121] // A + BC + CAb | catalytic ligation: A + BC + CAb <-> ABC + CAb
            -2372.62078331855 * x.[53] * x.[244] // ABC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            2372.62078331855 * x.[3] * x.[17] * x.[244] // A + BC + caB | catalytic ligation: A + BC + caB <-> ABC + caB
            -1.0 * x.[53] // ABC | ligation: AB + C <-> ABC
            1.0 * x.[10] * x.[5] // AB + C | ligation: AB + C <-> ABC
            -1.0 * x.[53] // ABC | ligation: A + BC <-> ABC
            1.0 * x.[3] * x.[17] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 54 - ABa
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[54] * x.[121] // ABa + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            60.8364303415014 * x.[3] * x.[18] * x.[121] // A + Ba + CAb | catalytic ligation: A + Ba + CAb <-> ABa + CAb
            -2372.62078331855 * x.[54] * x.[244] // ABa + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            2372.62078331855 * x.[3] * x.[18] * x.[244] // A + Ba + caB | catalytic ligation: A + Ba + caB <-> ABa + caB
            -1.0 * x.[54] // ABa | ligation: AB + a <-> ABa
            1.0 * x.[10] * x.[6] // AB + a | ligation: AB + a <-> ABa
            -1.0 * x.[54] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[18] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 55 - ABb
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[55] * x.[121] // ABb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            60.8364303415014 * x.[3] * x.[19] * x.[121] // A + Bb + CAb | catalytic ligation: A + Bb + CAb <-> ABb + CAb
            -2372.62078331855 * x.[55] * x.[244] // ABb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            2372.62078331855 * x.[3] * x.[19] * x.[244] // A + Bb + caB | catalytic ligation: A + Bb + caB <-> ABb + caB
            -1.0 * x.[55] // ABb | ligation: AB + b <-> ABb
            1.0 * x.[10] * x.[7] // AB + b | ligation: AB + b <-> ABb
            -1.0 * x.[55] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[19] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 56 - ABc
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[56] * x.[121] // ABc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            60.8364303415014 * x.[3] * x.[20] * x.[121] // A + Bc + CAb | catalytic ligation: A + Bc + CAb <-> ABc + CAb
            -2372.62078331855 * x.[56] * x.[244] // ABc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            2372.62078331855 * x.[3] * x.[20] * x.[244] // A + Bc + caB | catalytic ligation: A + Bc + caB <-> ABc + caB
            -1.0 * x.[56] // ABc | ligation: AB + c <-> ABc
            1.0 * x.[10] * x.[8] // AB + c | ligation: AB + c <-> ABc
            -1.0 * x.[56] // ABc | ligation: A + Bc <-> ABc
            1.0 * x.[3] * x.[20] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 57 - ACA
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[57] // ACA | ligation: AC + A <-> ACA
            1.0 * x.[11] * x.[3] // AC + A | ligation: AC + A <-> ACA
            -1.0 * x.[57] // ACA | ligation: A + CA <-> ACA
            1.0 * x.[3] * x.[21] // A + CA | ligation: A + CA <-> ACA
        |]
        |> Array.sum


    // 58 - ACB
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[58] * x.[121] // ACB + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            60.8364303415014 * x.[11] * x.[4] * x.[121] // AC + B + CAb | catalytic ligation: AC + B + CAb <-> ACB + CAb
            -2372.62078331855 * x.[58] * x.[244] // ACB + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            2372.62078331855 * x.[11] * x.[4] * x.[244] // AC + B + caB | catalytic ligation: AC + B + caB <-> ACB + caB
            -1.0 * x.[58] // ACB | ligation: AC + B <-> ACB
            1.0 * x.[11] * x.[4] // AC + B | ligation: AC + B <-> ACB
            -1.0 * x.[58] // ACB | ligation: A + CB <-> ACB
            1.0 * x.[3] * x.[22] // A + CB | ligation: A + CB <-> ACB
        |]
        |> Array.sum


    // 59 - ACC
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[59] * x.[190] // ACC + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            2214.00742039413 * x.[11] * x.[5] * x.[190] // AC + C + bAB | catalytic ligation: AC + C + bAB <-> ACC + bAB
            -56.7694210357472 * x.[59] * x.[103] // ACC + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            56.7694210357472 * x.[11] * x.[5] * x.[103] // AC + C + Bab | catalytic ligation: AC + C + Bab <-> ACC + Bab
            -1.0 * x.[59] // ACC | ligation: AC + C <-> ACC
            1.0 * x.[11] * x.[5] // AC + C | ligation: AC + C <-> ACC
            -1.0 * x.[59] // ACC | ligation: A + CC <-> ACC
            1.0 * x.[3] * x.[23] // A + CC | ligation: A + CC <-> ACC
        |]
        |> Array.sum


    // 60 - ACa
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[60] // ACa | ligation: AC + a <-> ACa
            1.0 * x.[11] * x.[6] // AC + a | ligation: AC + a <-> ACa
            -1.0 * x.[60] // ACa | ligation: A + Ca <-> ACa
            1.0 * x.[3] * x.[24] // A + Ca | ligation: A + Ca <-> ACa
        |]
        |> Array.sum


    // 61 - ACb
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[61] // ACb | ligation: AC + b <-> ACb
            1.0 * x.[11] * x.[7] // AC + b | ligation: AC + b <-> ACb
            -1.0 * x.[61] // ACb | ligation: A + Cb <-> ACb
            1.0 * x.[3] * x.[25] // A + Cb | ligation: A + Cb <-> ACb
        |]
        |> Array.sum


    // 62 - ACc
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[62] // ACc | ligation: AC + c <-> ACc
            1.0 * x.[11] * x.[8] // AC + c | ligation: AC + c <-> ACc
            -1.0 * x.[62] // ACc | ligation: A + Cc <-> ACc
            1.0 * x.[3] * x.[26] // A + Cc | ligation: A + Cc <-> ACc
        |]
        |> Array.sum


    // 63 - AaA
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[63] // AaA | ligation: Aa + A <-> AaA
            1.0 * x.[12] * x.[3] // Aa + A | ligation: Aa + A <-> AaA
            -1.0 * x.[63] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[27] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 64 - AaB
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[64] // AaB | ligation: Aa + B <-> AaB
            1.0 * x.[12] * x.[4] // Aa + B | ligation: Aa + B <-> AaB
            -1.0 * x.[64] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[28] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 65 - AaC
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[65] // AaC | ligation: Aa + C <-> AaC
            1.0 * x.[12] * x.[5] // Aa + C | ligation: Aa + C <-> AaC
            -1.0 * x.[65] // AaC | ligation: A + aC <-> AaC
            1.0 * x.[3] * x.[29] // A + aC | ligation: A + aC <-> AaC
        |]
        |> Array.sum


    // 66 - Aaa
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[66] // Aaa | ligation: Aa + a <-> Aaa
            1.0 * x.[12] * x.[6] // Aa + a | ligation: Aa + a <-> Aaa
            -1.0 * x.[66] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[30] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 67 - Aab
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[67] // Aab | ligation: Aa + b <-> Aab
            1.0 * x.[12] * x.[7] // Aa + b | ligation: Aa + b <-> Aab
            -1.0 * x.[67] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[31] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 68 - Aac
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[68] // Aac | ligation: Aa + c <-> Aac
            1.0 * x.[12] * x.[8] // Aa + c | ligation: Aa + c <-> Aac
            -1.0 * x.[68] // Aac | ligation: A + ac <-> Aac
            1.0 * x.[3] * x.[32] // A + ac | ligation: A + ac <-> Aac
        |]
        |> Array.sum


    // 69 - AbA
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[69] // AbA | ligation: Ab + A <-> AbA
            1.0 * x.[13] * x.[3] // Ab + A | ligation: Ab + A <-> AbA
            -1.0 * x.[69] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[33] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 70 - AbB
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[70] // AbB | ligation: Ab + B <-> AbB
            1.0 * x.[13] * x.[4] // Ab + B | ligation: Ab + B <-> AbB
            -1.0 * x.[70] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[34] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 71 - AbC
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[71] // AbC | ligation: Ab + C <-> AbC
            1.0 * x.[13] * x.[5] // Ab + C | ligation: Ab + C <-> AbC
            -1.0 * x.[71] // AbC | ligation: A + bC <-> AbC
            1.0 * x.[3] * x.[35] // A + bC | ligation: A + bC <-> AbC
        |]
        |> Array.sum


    // 72 - Aba
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[72] // Aba | ligation: Ab + a <-> Aba
            1.0 * x.[13] * x.[6] // Ab + a | ligation: Ab + a <-> Aba
            -1.0 * x.[72] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[36] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 73 - Abb
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[73] // Abb | ligation: Ab + b <-> Abb
            1.0 * x.[13] * x.[7] // Ab + b | ligation: Ab + b <-> Abb
            -1.0 * x.[73] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[37] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 74 - Abc
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[74] // Abc | ligation: Ab + c <-> Abc
            1.0 * x.[13] * x.[8] // Ab + c | ligation: Ab + c <-> Abc
            -1.0 * x.[74] // Abc | ligation: A + bc <-> Abc
            1.0 * x.[3] * x.[38] // A + bc | ligation: A + bc <-> Abc
        |]
        |> Array.sum


    // 75 - AcA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[75] // AcA | ligation: Ac + A <-> AcA
            1.0 * x.[14] * x.[3] // Ac + A | ligation: Ac + A <-> AcA
            -1.0 * x.[75] // AcA | ligation: A + cA <-> AcA
            1.0 * x.[3] * x.[39] // A + cA | ligation: A + cA <-> AcA
        |]
        |> Array.sum


    // 76 - AcB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[76] // AcB | ligation: Ac + B <-> AcB
            1.0 * x.[14] * x.[4] // Ac + B | ligation: Ac + B <-> AcB
            -1.0 * x.[76] // AcB | ligation: A + cB <-> AcB
            1.0 * x.[3] * x.[40] // A + cB | ligation: A + cB <-> AcB
        |]
        |> Array.sum


    // 77 - AcC
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[77] // AcC | ligation: Ac + C <-> AcC
            1.0 * x.[14] * x.[5] // Ac + C | ligation: Ac + C <-> AcC
            -1.0 * x.[77] // AcC | ligation: A + cC <-> AcC
            1.0 * x.[3] * x.[41] // A + cC | ligation: A + cC <-> AcC
        |]
        |> Array.sum


    // 78 - Aca
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[78] // Aca | ligation: Ac + a <-> Aca
            1.0 * x.[14] * x.[6] // Ac + a | ligation: Ac + a <-> Aca
            -1.0 * x.[78] // Aca | ligation: A + ca <-> Aca
            1.0 * x.[3] * x.[42] // A + ca | ligation: A + ca <-> Aca
        |]
        |> Array.sum


    // 79 - Acb
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[79] // Acb | ligation: Ac + b <-> Acb
            1.0 * x.[14] * x.[7] // Ac + b | ligation: Ac + b <-> Acb
            -1.0 * x.[79] // Acb | ligation: A + cb <-> Acb
            1.0 * x.[3] * x.[43] // A + cb | ligation: A + cb <-> Acb
        |]
        |> Array.sum


    // 80 - Acc
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[80] // Acc | ligation: Ac + c <-> Acc
            1.0 * x.[14] * x.[8] // Ac + c | ligation: Ac + c <-> Acc
            -1.0 * x.[80] // Acc | ligation: A + cc <-> Acc
            1.0 * x.[3] * x.[44] // A + cc | ligation: A + cc <-> Acc
        |]
        |> Array.sum


    // 81 - BAA
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[81] * x.[190] // BAA + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            2214.00742039413 * x.[15] * x.[3] * x.[190] // BA + A + bAB | catalytic ligation: BA + A + bAB <-> BAA + bAB
            -56.7694210357472 * x.[81] * x.[103] // BAA + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            56.7694210357472 * x.[15] * x.[3] * x.[103] // BA + A + Bab | catalytic ligation: BA + A + Bab <-> BAA + Bab
            -60.8364303415014 * x.[81] * x.[121] // BAA + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            60.8364303415014 * x.[15] * x.[3] * x.[121] // BA + A + CAb | catalytic ligation: BA + A + CAb <-> BAA + CAb
            -2372.62078331855 * x.[81] * x.[244] // BAA + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            2372.62078331855 * x.[15] * x.[3] * x.[244] // BA + A + caB | catalytic ligation: BA + A + caB <-> BAA + caB
            -1.0 * x.[81] // BAA | ligation: BA + A <-> BAA
            1.0 * x.[15] * x.[3] // BA + A | ligation: BA + A <-> BAA
            -1.0 * x.[81] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[9] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 82 - BAB
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[82] * x.[121] // BAB + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            60.8364303415014 * x.[15] * x.[4] * x.[121] // BA + B + CAb | catalytic ligation: BA + B + CAb <-> BAB + CAb
            -2372.62078331855 * x.[82] * x.[244] // BAB + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            2372.62078331855 * x.[15] * x.[4] * x.[244] // BA + B + caB | catalytic ligation: BA + B + caB <-> BAB + caB
            -1.0 * x.[82] // BAB | ligation: BA + B <-> BAB
            1.0 * x.[15] * x.[4] // BA + B | ligation: BA + B <-> BAB
            -1.0 * x.[82] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[10] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 83 - BAC
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[83] // BAC | ligation: BA + C <-> BAC
            1.0 * x.[15] * x.[5] // BA + C | ligation: BA + C <-> BAC
            -1.0 * x.[83] // BAC | ligation: B + AC <-> BAC
            1.0 * x.[4] * x.[11] // B + AC | ligation: B + AC <-> BAC
        |]
        |> Array.sum


    // 84 - BAa
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[84] // BAa | ligation: BA + a <-> BAa
            1.0 * x.[15] * x.[6] // BA + a | ligation: BA + a <-> BAa
            -1.0 * x.[84] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[12] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 85 - BAb
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[85] // BAb | ligation: BA + b <-> BAb
            1.0 * x.[15] * x.[7] // BA + b | ligation: BA + b <-> BAb
            -1.0 * x.[85] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[13] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 86 - BAc
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[86] // BAc | ligation: BA + c <-> BAc
            1.0 * x.[15] * x.[8] // BA + c | ligation: BA + c <-> BAc
            -1.0 * x.[86] // BAc | ligation: B + Ac <-> BAc
            1.0 * x.[4] * x.[14] // B + Ac | ligation: B + Ac <-> BAc
        |]
        |> Array.sum


    // 87 - BBA
    let d87 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[87] // BBA | ligation: BB + A <-> BBA
            1.0 * x.[16] * x.[3] // BB + A | ligation: BB + A <-> BBA
            -1.0 * x.[87] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[15] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 88 - BBB
    let d88 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[88] // BBB | ligation: BB + B <-> BBB
            1.0 * x.[16] * x.[4] // BB + B | ligation: BB + B <-> BBB
            -1.0 * x.[88] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[16] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 89 - BBC
    let d89 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[89] * x.[190] // BBC + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            2214.00742039413 * x.[16] * x.[5] * x.[190] // BB + C + bAB | catalytic ligation: BB + C + bAB <-> BBC + bAB
            -56.7694210357472 * x.[89] * x.[103] // BBC + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            56.7694210357472 * x.[16] * x.[5] * x.[103] // BB + C + Bab | catalytic ligation: BB + C + Bab <-> BBC + Bab
            -1.0 * x.[89] // BBC | ligation: BB + C <-> BBC
            1.0 * x.[16] * x.[5] // BB + C | ligation: BB + C <-> BBC
            -1.0 * x.[89] // BBC | ligation: B + BC <-> BBC
            1.0 * x.[4] * x.[17] // B + BC | ligation: B + BC <-> BBC
        |]
        |> Array.sum


    // 90 - BBa
    let d90 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[90] // BBa | ligation: BB + a <-> BBa
            1.0 * x.[16] * x.[6] // BB + a | ligation: BB + a <-> BBa
            -1.0 * x.[90] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[18] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 91 - BBb
    let d91 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[91] // BBb | ligation: BB + b <-> BBb
            1.0 * x.[16] * x.[7] // BB + b | ligation: BB + b <-> BBb
            -1.0 * x.[91] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[19] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 92 - BBc
    let d92 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[92] // BBc | ligation: BB + c <-> BBc
            1.0 * x.[16] * x.[8] // BB + c | ligation: BB + c <-> BBc
            -1.0 * x.[92] // BBc | ligation: B + Bc <-> BBc
            1.0 * x.[4] * x.[20] // B + Bc | ligation: B + Bc <-> BBc
        |]
        |> Array.sum


    // 93 - BCA
    let d93 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[93] * x.[190] // BCA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            2214.00742039413 * x.[4] * x.[21] * x.[190] // B + CA + bAB | catalytic ligation: B + CA + bAB <-> BCA + bAB
            -56.7694210357472 * x.[93] * x.[103] // BCA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            56.7694210357472 * x.[4] * x.[21] * x.[103] // B + CA + Bab | catalytic ligation: B + CA + Bab <-> BCA + Bab
            -1.0 * x.[93] // BCA | ligation: BC + A <-> BCA
            1.0 * x.[17] * x.[3] // BC + A | ligation: BC + A <-> BCA
            -1.0 * x.[93] // BCA | ligation: B + CA <-> BCA
            1.0 * x.[4] * x.[21] // B + CA | ligation: B + CA <-> BCA
        |]
        |> Array.sum


    // 94 - BCB
    let d94 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[94] * x.[190] // BCB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            2214.00742039413 * x.[4] * x.[22] * x.[190] // B + CB + bAB | catalytic ligation: B + CB + bAB <-> BCB + bAB
            -56.7694210357472 * x.[94] * x.[103] // BCB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            56.7694210357472 * x.[4] * x.[22] * x.[103] // B + CB + Bab | catalytic ligation: B + CB + Bab <-> BCB + Bab
            -60.8364303415014 * x.[94] * x.[121] // BCB + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            60.8364303415014 * x.[17] * x.[4] * x.[121] // BC + B + CAb | catalytic ligation: BC + B + CAb <-> BCB + CAb
            -2372.62078331855 * x.[94] * x.[244] // BCB + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            2372.62078331855 * x.[17] * x.[4] * x.[244] // BC + B + caB | catalytic ligation: BC + B + caB <-> BCB + caB
            -1.0 * x.[94] // BCB | ligation: BC + B <-> BCB
            1.0 * x.[17] * x.[4] // BC + B | ligation: BC + B <-> BCB
            -1.0 * x.[94] // BCB | ligation: B + CB <-> BCB
            1.0 * x.[4] * x.[22] // B + CB | ligation: B + CB <-> BCB
        |]
        |> Array.sum


    // 95 - BCC
    let d95 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            2214.00742039413 * x.[17] * x.[5] * x.[190] // BC + C + bAB | catalytic ligation: BC + C + bAB <-> BCC + bAB
            -56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            56.7694210357472 * x.[17] * x.[5] * x.[103] // BC + C + Bab | catalytic ligation: BC + C + Bab <-> BCC + Bab
            -2214.00742039413 * x.[95] * x.[190] // BCC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            2214.00742039413 * x.[4] * x.[23] * x.[190] // B + CC + bAB | catalytic ligation: B + CC + bAB <-> BCC + bAB
            -56.7694210357472 * x.[95] * x.[103] // BCC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            56.7694210357472 * x.[4] * x.[23] * x.[103] // B + CC + Bab | catalytic ligation: B + CC + Bab <-> BCC + Bab
            -1.0 * x.[95] // BCC | ligation: BC + C <-> BCC
            1.0 * x.[17] * x.[5] // BC + C | ligation: BC + C <-> BCC
            -1.0 * x.[95] // BCC | ligation: B + CC <-> BCC
            1.0 * x.[4] * x.[23] // B + CC | ligation: B + CC <-> BCC
        |]
        |> Array.sum


    // 96 - BCa
    let d96 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[96] * x.[190] // BCa + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            2214.00742039413 * x.[4] * x.[24] * x.[190] // B + Ca + bAB | catalytic ligation: B + Ca + bAB <-> BCa + bAB
            -56.7694210357472 * x.[96] * x.[103] // BCa + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            56.7694210357472 * x.[4] * x.[24] * x.[103] // B + Ca + Bab | catalytic ligation: B + Ca + Bab <-> BCa + Bab
            -1.0 * x.[96] // BCa | ligation: BC + a <-> BCa
            1.0 * x.[17] * x.[6] // BC + a | ligation: BC + a <-> BCa
            -1.0 * x.[96] // BCa | ligation: B + Ca <-> BCa
            1.0 * x.[4] * x.[24] // B + Ca | ligation: B + Ca <-> BCa
        |]
        |> Array.sum


    // 97 - BCb
    let d97 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[97] * x.[190] // BCb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            2214.00742039413 * x.[4] * x.[25] * x.[190] // B + Cb + bAB | catalytic ligation: B + Cb + bAB <-> BCb + bAB
            -56.7694210357472 * x.[97] * x.[103] // BCb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            56.7694210357472 * x.[4] * x.[25] * x.[103] // B + Cb + Bab | catalytic ligation: B + Cb + Bab <-> BCb + Bab
            -1.0 * x.[97] // BCb | ligation: BC + b <-> BCb
            1.0 * x.[17] * x.[7] // BC + b | ligation: BC + b <-> BCb
            -1.0 * x.[97] // BCb | ligation: B + Cb <-> BCb
            1.0 * x.[4] * x.[25] // B + Cb | ligation: B + Cb <-> BCb
        |]
        |> Array.sum


    // 98 - BCc
    let d98 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[98] * x.[190] // BCc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            2214.00742039413 * x.[4] * x.[26] * x.[190] // B + Cc + bAB | catalytic ligation: B + Cc + bAB <-> BCc + bAB
            -56.7694210357472 * x.[98] * x.[103] // BCc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            56.7694210357472 * x.[4] * x.[26] * x.[103] // B + Cc + Bab | catalytic ligation: B + Cc + Bab <-> BCc + Bab
            -1.0 * x.[98] // BCc | ligation: BC + c <-> BCc
            1.0 * x.[17] * x.[8] // BC + c | ligation: BC + c <-> BCc
            -1.0 * x.[98] // BCc | ligation: B + Cc <-> BCc
            1.0 * x.[4] * x.[26] // B + Cc | ligation: B + Cc <-> BCc
        |]
        |> Array.sum


    // 99 - BaA
    let d99 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[99] // BaA | ligation: Ba + A <-> BaA
            1.0 * x.[18] * x.[3] // Ba + A | ligation: Ba + A <-> BaA
            -1.0 * x.[99] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[27] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 100 - BaB
    let d100 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[100] // BaB | ligation: Ba + B <-> BaB
            1.0 * x.[18] * x.[4] // Ba + B | ligation: Ba + B <-> BaB
            -1.0 * x.[100] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[28] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 101 - BaC
    let d101 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[101] // BaC | ligation: Ba + C <-> BaC
            1.0 * x.[18] * x.[5] // Ba + C | ligation: Ba + C <-> BaC
            -1.0 * x.[101] // BaC | ligation: B + aC <-> BaC
            1.0 * x.[4] * x.[29] // B + aC | ligation: B + aC <-> BaC
        |]
        |> Array.sum


    // 102 - Baa
    let d102 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[102] // Baa | ligation: Ba + a <-> Baa
            1.0 * x.[18] * x.[6] // Ba + a | ligation: Ba + a <-> Baa
            -1.0 * x.[102] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[30] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 103 - Bab
    let d103 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[103] // Bab | ligation: Ba + b <-> Bab
            1.0 * x.[18] * x.[7] // Ba + b | ligation: Ba + b <-> Bab
            -1.0 * x.[103] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[31] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 104 - Bac
    let d104 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[104] // Bac | ligation: Ba + c <-> Bac
            1.0 * x.[18] * x.[8] // Ba + c | ligation: Ba + c <-> Bac
            -1.0 * x.[104] // Bac | ligation: B + ac <-> Bac
            1.0 * x.[4] * x.[32] // B + ac | ligation: B + ac <-> Bac
        |]
        |> Array.sum


    // 105 - BbA
    let d105 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[105] // BbA | ligation: Bb + A <-> BbA
            1.0 * x.[19] * x.[3] // Bb + A | ligation: Bb + A <-> BbA
            -1.0 * x.[105] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[33] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 106 - BbB
    let d106 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[106] // BbB | ligation: Bb + B <-> BbB
            1.0 * x.[19] * x.[4] // Bb + B | ligation: Bb + B <-> BbB
            -1.0 * x.[106] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[34] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 107 - BbC
    let d107 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[107] // BbC | ligation: Bb + C <-> BbC
            1.0 * x.[19] * x.[5] // Bb + C | ligation: Bb + C <-> BbC
            -1.0 * x.[107] // BbC | ligation: B + bC <-> BbC
            1.0 * x.[4] * x.[35] // B + bC | ligation: B + bC <-> BbC
        |]
        |> Array.sum


    // 108 - Bba
    let d108 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[108] // Bba | ligation: Bb + a <-> Bba
            1.0 * x.[19] * x.[6] // Bb + a | ligation: Bb + a <-> Bba
            -1.0 * x.[108] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[36] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 109 - Bbb
    let d109 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[109] // Bbb | ligation: Bb + b <-> Bbb
            1.0 * x.[19] * x.[7] // Bb + b | ligation: Bb + b <-> Bbb
            -1.0 * x.[109] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[37] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 110 - Bbc
    let d110 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[110] // Bbc | ligation: Bb + c <-> Bbc
            1.0 * x.[19] * x.[8] // Bb + c | ligation: Bb + c <-> Bbc
            -1.0 * x.[110] // Bbc | ligation: B + bc <-> Bbc
            1.0 * x.[4] * x.[38] // B + bc | ligation: B + bc <-> Bbc
        |]
        |> Array.sum


    // 111 - BcA
    let d111 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[111] // BcA | ligation: Bc + A <-> BcA
            1.0 * x.[20] * x.[3] // Bc + A | ligation: Bc + A <-> BcA
            -1.0 * x.[111] // BcA | ligation: B + cA <-> BcA
            1.0 * x.[4] * x.[39] // B + cA | ligation: B + cA <-> BcA
        |]
        |> Array.sum


    // 112 - BcB
    let d112 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[112] // BcB | ligation: Bc + B <-> BcB
            1.0 * x.[20] * x.[4] // Bc + B | ligation: Bc + B <-> BcB
            -1.0 * x.[112] // BcB | ligation: B + cB <-> BcB
            1.0 * x.[4] * x.[40] // B + cB | ligation: B + cB <-> BcB
        |]
        |> Array.sum


    // 113 - BcC
    let d113 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[113] // BcC | ligation: Bc + C <-> BcC
            1.0 * x.[20] * x.[5] // Bc + C | ligation: Bc + C <-> BcC
            -1.0 * x.[113] // BcC | ligation: B + cC <-> BcC
            1.0 * x.[4] * x.[41] // B + cC | ligation: B + cC <-> BcC
        |]
        |> Array.sum


    // 114 - Bca
    let d114 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[114] // Bca | ligation: Bc + a <-> Bca
            1.0 * x.[20] * x.[6] // Bc + a | ligation: Bc + a <-> Bca
            -1.0 * x.[114] // Bca | ligation: B + ca <-> Bca
            1.0 * x.[4] * x.[42] // B + ca | ligation: B + ca <-> Bca
        |]
        |> Array.sum


    // 115 - Bcb
    let d115 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[115] // Bcb | ligation: Bc + b <-> Bcb
            1.0 * x.[20] * x.[7] // Bc + b | ligation: Bc + b <-> Bcb
            -1.0 * x.[115] // Bcb | ligation: B + cb <-> Bcb
            1.0 * x.[4] * x.[43] // B + cb | ligation: B + cb <-> Bcb
        |]
        |> Array.sum


    // 116 - Bcc
    let d116 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[116] // Bcc | ligation: Bc + c <-> Bcc
            1.0 * x.[20] * x.[8] // Bc + c | ligation: Bc + c <-> Bcc
            -1.0 * x.[116] // Bcc | ligation: B + cc <-> Bcc
            1.0 * x.[4] * x.[44] // B + cc | ligation: B + cc <-> Bcc
        |]
        |> Array.sum


    // 117 - CAA
    let d117 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[117] * x.[190] // CAA + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            2214.00742039413 * x.[21] * x.[3] * x.[190] // CA + A + bAB | catalytic ligation: CA + A + bAB <-> CAA + bAB
            -56.7694210357472 * x.[117] * x.[103] // CAA + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            56.7694210357472 * x.[21] * x.[3] * x.[103] // CA + A + Bab | catalytic ligation: CA + A + Bab <-> CAA + Bab
            -60.8364303415014 * x.[117] * x.[121] // CAA + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            60.8364303415014 * x.[21] * x.[3] * x.[121] // CA + A + CAb | catalytic ligation: CA + A + CAb <-> CAA + CAb
            -2372.62078331855 * x.[117] * x.[244] // CAA + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            2372.62078331855 * x.[21] * x.[3] * x.[244] // CA + A + caB | catalytic ligation: CA + A + caB <-> CAA + caB
            -1.0 * x.[117] // CAA | ligation: CA + A <-> CAA
            1.0 * x.[21] * x.[3] // CA + A | ligation: CA + A <-> CAA
            -1.0 * x.[117] // CAA | ligation: C + AA <-> CAA
            1.0 * x.[5] * x.[9] // C + AA | ligation: C + AA <-> CAA
        |]
        |> Array.sum


    // 118 - CAB
    let d118 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[118] * x.[121] // CAB + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            60.8364303415014 * x.[21] * x.[4] * x.[121] // CA + B + CAb | catalytic ligation: CA + B + CAb <-> CAB + CAb
            -2372.62078331855 * x.[118] * x.[244] // CAB + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            2372.62078331855 * x.[21] * x.[4] * x.[244] // CA + B + caB | catalytic ligation: CA + B + caB <-> CAB + caB
            -1.0 * x.[118] // CAB | ligation: CA + B <-> CAB
            1.0 * x.[21] * x.[4] // CA + B | ligation: CA + B <-> CAB
            -1.0 * x.[118] // CAB | ligation: C + AB <-> CAB
            1.0 * x.[5] * x.[10] // C + AB | ligation: C + AB <-> CAB
        |]
        |> Array.sum


    // 119 - CAC
    let d119 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[119] // CAC | ligation: CA + C <-> CAC
            1.0 * x.[21] * x.[5] // CA + C | ligation: CA + C <-> CAC
            -1.0 * x.[119] // CAC | ligation: C + AC <-> CAC
            1.0 * x.[5] * x.[11] // C + AC | ligation: C + AC <-> CAC
        |]
        |> Array.sum


    // 120 - CAa
    let d120 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[120] // CAa | ligation: CA + a <-> CAa
            1.0 * x.[21] * x.[6] // CA + a | ligation: CA + a <-> CAa
            -1.0 * x.[120] // CAa | ligation: C + Aa <-> CAa
            1.0 * x.[5] * x.[12] // C + Aa | ligation: C + Aa <-> CAa
        |]
        |> Array.sum


    // 121 - CAb
    let d121 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[121] // CAb | ligation: CA + b <-> CAb
            1.0 * x.[21] * x.[7] // CA + b | ligation: CA + b <-> CAb
            -1.0 * x.[121] // CAb | ligation: C + Ab <-> CAb
            1.0 * x.[5] * x.[13] // C + Ab | ligation: C + Ab <-> CAb
        |]
        |> Array.sum


    // 122 - CAc
    let d122 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[122] // CAc | ligation: CA + c <-> CAc
            1.0 * x.[21] * x.[8] // CA + c | ligation: CA + c <-> CAc
            -1.0 * x.[122] // CAc | ligation: C + Ac <-> CAc
            1.0 * x.[5] * x.[14] // C + Ac | ligation: C + Ac <-> CAc
        |]
        |> Array.sum


    // 123 - CBA
    let d123 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[123] * x.[121] // CBA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            60.8364303415014 * x.[5] * x.[15] * x.[121] // C + BA + CAb | catalytic ligation: C + BA + CAb <-> CBA + CAb
            -2372.62078331855 * x.[123] * x.[244] // CBA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            2372.62078331855 * x.[5] * x.[15] * x.[244] // C + BA + caB | catalytic ligation: C + BA + caB <-> CBA + caB
            -1.0 * x.[123] // CBA | ligation: CB + A <-> CBA
            1.0 * x.[22] * x.[3] // CB + A | ligation: CB + A <-> CBA
            -1.0 * x.[123] // CBA | ligation: C + BA <-> CBA
            1.0 * x.[5] * x.[15] // C + BA | ligation: C + BA <-> CBA
        |]
        |> Array.sum


    // 124 - CBB
    let d124 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[124] * x.[121] // CBB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            60.8364303415014 * x.[5] * x.[16] * x.[121] // C + BB + CAb | catalytic ligation: C + BB + CAb <-> CBB + CAb
            -2372.62078331855 * x.[124] * x.[244] // CBB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            2372.62078331855 * x.[5] * x.[16] * x.[244] // C + BB + caB | catalytic ligation: C + BB + caB <-> CBB + caB
            -1.0 * x.[124] // CBB | ligation: CB + B <-> CBB
            1.0 * x.[22] * x.[4] // CB + B | ligation: CB + B <-> CBB
            -1.0 * x.[124] // CBB | ligation: C + BB <-> CBB
            1.0 * x.[5] * x.[16] // C + BB | ligation: C + BB <-> CBB
        |]
        |> Array.sum


    // 125 - CBC
    let d125 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[125] * x.[190] // CBC + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            2214.00742039413 * x.[22] * x.[5] * x.[190] // CB + C + bAB | catalytic ligation: CB + C + bAB <-> CBC + bAB
            -56.7694210357472 * x.[125] * x.[103] // CBC + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            56.7694210357472 * x.[22] * x.[5] * x.[103] // CB + C + Bab | catalytic ligation: CB + C + Bab <-> CBC + Bab
            -60.8364303415014 * x.[125] * x.[121] // CBC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            60.8364303415014 * x.[5] * x.[17] * x.[121] // C + BC + CAb | catalytic ligation: C + BC + CAb <-> CBC + CAb
            -2372.62078331855 * x.[125] * x.[244] // CBC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            2372.62078331855 * x.[5] * x.[17] * x.[244] // C + BC + caB | catalytic ligation: C + BC + caB <-> CBC + caB
            -1.0 * x.[125] // CBC | ligation: CB + C <-> CBC
            1.0 * x.[22] * x.[5] // CB + C | ligation: CB + C <-> CBC
            -1.0 * x.[125] // CBC | ligation: C + BC <-> CBC
            1.0 * x.[5] * x.[17] // C + BC | ligation: C + BC <-> CBC
        |]
        |> Array.sum


    // 126 - CBa
    let d126 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[126] * x.[121] // CBa + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            60.8364303415014 * x.[5] * x.[18] * x.[121] // C + Ba + CAb | catalytic ligation: C + Ba + CAb <-> CBa + CAb
            -2372.62078331855 * x.[126] * x.[244] // CBa + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            2372.62078331855 * x.[5] * x.[18] * x.[244] // C + Ba + caB | catalytic ligation: C + Ba + caB <-> CBa + caB
            -1.0 * x.[126] // CBa | ligation: CB + a <-> CBa
            1.0 * x.[22] * x.[6] // CB + a | ligation: CB + a <-> CBa
            -1.0 * x.[126] // CBa | ligation: C + Ba <-> CBa
            1.0 * x.[5] * x.[18] // C + Ba | ligation: C + Ba <-> CBa
        |]
        |> Array.sum


    // 127 - CBb
    let d127 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[127] * x.[121] // CBb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            60.8364303415014 * x.[5] * x.[19] * x.[121] // C + Bb + CAb | catalytic ligation: C + Bb + CAb <-> CBb + CAb
            -2372.62078331855 * x.[127] * x.[244] // CBb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            2372.62078331855 * x.[5] * x.[19] * x.[244] // C + Bb + caB | catalytic ligation: C + Bb + caB <-> CBb + caB
            -1.0 * x.[127] // CBb | ligation: CB + b <-> CBb
            1.0 * x.[22] * x.[7] // CB + b | ligation: CB + b <-> CBb
            -1.0 * x.[127] // CBb | ligation: C + Bb <-> CBb
            1.0 * x.[5] * x.[19] // C + Bb | ligation: C + Bb <-> CBb
        |]
        |> Array.sum


    // 128 - CBc
    let d128 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[128] * x.[121] // CBc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            60.8364303415014 * x.[5] * x.[20] * x.[121] // C + Bc + CAb | catalytic ligation: C + Bc + CAb <-> CBc + CAb
            -2372.62078331855 * x.[128] * x.[244] // CBc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            2372.62078331855 * x.[5] * x.[20] * x.[244] // C + Bc + caB | catalytic ligation: C + Bc + caB <-> CBc + caB
            -1.0 * x.[128] // CBc | ligation: CB + c <-> CBc
            1.0 * x.[22] * x.[8] // CB + c | ligation: CB + c <-> CBc
            -1.0 * x.[128] // CBc | ligation: C + Bc <-> CBc
            1.0 * x.[5] * x.[20] // C + Bc | ligation: C + Bc <-> CBc
        |]
        |> Array.sum


    // 129 - CCA
    let d129 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[129] * x.[190] // CCA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            2214.00742039413 * x.[5] * x.[21] * x.[190] // C + CA + bAB | catalytic ligation: C + CA + bAB <-> CCA + bAB
            -56.7694210357472 * x.[129] * x.[103] // CCA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            56.7694210357472 * x.[5] * x.[21] * x.[103] // C + CA + Bab | catalytic ligation: C + CA + Bab <-> CCA + Bab
            -1.0 * x.[129] // CCA | ligation: CC + A <-> CCA
            1.0 * x.[23] * x.[3] // CC + A | ligation: CC + A <-> CCA
            -1.0 * x.[129] // CCA | ligation: C + CA <-> CCA
            1.0 * x.[5] * x.[21] // C + CA | ligation: C + CA <-> CCA
        |]
        |> Array.sum


    // 130 - CCB
    let d130 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[130] * x.[190] // CCB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            2214.00742039413 * x.[5] * x.[22] * x.[190] // C + CB + bAB | catalytic ligation: C + CB + bAB <-> CCB + bAB
            -56.7694210357472 * x.[130] * x.[103] // CCB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            56.7694210357472 * x.[5] * x.[22] * x.[103] // C + CB + Bab | catalytic ligation: C + CB + Bab <-> CCB + Bab
            -60.8364303415013 * x.[130] * x.[121] // CCB + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            60.8364303415013 * x.[23] * x.[4] * x.[121] // CC + B + CAb | catalytic ligation: CC + B + CAb <-> CCB + CAb
            -2372.62078331855 * x.[130] * x.[244] // CCB + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            2372.62078331855 * x.[23] * x.[4] * x.[244] // CC + B + caB | catalytic ligation: CC + B + caB <-> CCB + caB
            -1.0 * x.[130] // CCB | ligation: CC + B <-> CCB
            1.0 * x.[23] * x.[4] // CC + B | ligation: CC + B <-> CCB
            -1.0 * x.[130] // CCB | ligation: C + CB <-> CCB
            1.0 * x.[5] * x.[22] // C + CB | ligation: C + CB <-> CCB
        |]
        |> Array.sum


    // 131 - CCC
    let d131 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            2214.00742039413 * x.[23] * x.[5] * x.[190] // CC + C + bAB | catalytic ligation: CC + C + bAB <-> CCC + bAB
            -56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            56.7694210357472 * x.[23] * x.[5] * x.[103] // CC + C + Bab | catalytic ligation: CC + C + Bab <-> CCC + Bab
            -2214.00742039413 * x.[131] * x.[190] // CCC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            2214.00742039413 * x.[5] * x.[23] * x.[190] // C + CC + bAB | catalytic ligation: C + CC + bAB <-> CCC + bAB
            -56.7694210357472 * x.[131] * x.[103] // CCC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            56.7694210357472 * x.[5] * x.[23] * x.[103] // C + CC + Bab | catalytic ligation: C + CC + Bab <-> CCC + Bab
            -1.0 * x.[131] // CCC | ligation: CC + C <-> CCC
            1.0 * x.[23] * x.[5] // CC + C | ligation: CC + C <-> CCC
            -1.0 * x.[131] // CCC | ligation: C + CC <-> CCC
            1.0 * x.[5] * x.[23] // C + CC | ligation: C + CC <-> CCC
        |]
        |> Array.sum


    // 132 - CCa
    let d132 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[132] * x.[190] // CCa + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            2214.00742039413 * x.[5] * x.[24] * x.[190] // C + Ca + bAB | catalytic ligation: C + Ca + bAB <-> CCa + bAB
            -56.7694210357472 * x.[132] * x.[103] // CCa + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            56.7694210357472 * x.[5] * x.[24] * x.[103] // C + Ca + Bab | catalytic ligation: C + Ca + Bab <-> CCa + Bab
            -1.0 * x.[132] // CCa | ligation: CC + a <-> CCa
            1.0 * x.[23] * x.[6] // CC + a | ligation: CC + a <-> CCa
            -1.0 * x.[132] // CCa | ligation: C + Ca <-> CCa
            1.0 * x.[5] * x.[24] // C + Ca | ligation: C + Ca <-> CCa
        |]
        |> Array.sum


    // 133 - CCb
    let d133 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[133] * x.[190] // CCb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            2214.00742039413 * x.[5] * x.[25] * x.[190] // C + Cb + bAB | catalytic ligation: C + Cb + bAB <-> CCb + bAB
            -56.7694210357472 * x.[133] * x.[103] // CCb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            56.7694210357472 * x.[5] * x.[25] * x.[103] // C + Cb + Bab | catalytic ligation: C + Cb + Bab <-> CCb + Bab
            -1.0 * x.[133] // CCb | ligation: CC + b <-> CCb
            1.0 * x.[23] * x.[7] // CC + b | ligation: CC + b <-> CCb
            -1.0 * x.[133] // CCb | ligation: C + Cb <-> CCb
            1.0 * x.[5] * x.[25] // C + Cb | ligation: C + Cb <-> CCb
        |]
        |> Array.sum


    // 134 - CCc
    let d134 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[134] * x.[190] // CCc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            2214.00742039413 * x.[5] * x.[26] * x.[190] // C + Cc + bAB | catalytic ligation: C + Cc + bAB <-> CCc + bAB
            -56.7694210357472 * x.[134] * x.[103] // CCc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            56.7694210357472 * x.[5] * x.[26] * x.[103] // C + Cc + Bab | catalytic ligation: C + Cc + Bab <-> CCc + Bab
            -1.0 * x.[134] // CCc | ligation: CC + c <-> CCc
            1.0 * x.[23] * x.[8] // CC + c | ligation: CC + c <-> CCc
            -1.0 * x.[134] // CCc | ligation: C + Cc <-> CCc
            1.0 * x.[5] * x.[26] // C + Cc | ligation: C + Cc <-> CCc
        |]
        |> Array.sum


    // 135 - CaA
    let d135 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[135] // CaA | ligation: Ca + A <-> CaA
            1.0 * x.[24] * x.[3] // Ca + A | ligation: Ca + A <-> CaA
            -1.0 * x.[135] // CaA | ligation: C + aA <-> CaA
            1.0 * x.[5] * x.[27] // C + aA | ligation: C + aA <-> CaA
        |]
        |> Array.sum


    // 136 - CaB
    let d136 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[136] // CaB | ligation: Ca + B <-> CaB
            1.0 * x.[24] * x.[4] // Ca + B | ligation: Ca + B <-> CaB
            -1.0 * x.[136] // CaB | ligation: C + aB <-> CaB
            1.0 * x.[5] * x.[28] // C + aB | ligation: C + aB <-> CaB
        |]
        |> Array.sum


    // 137 - CaC
    let d137 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[137] // CaC | ligation: Ca + C <-> CaC
            1.0 * x.[24] * x.[5] // Ca + C | ligation: Ca + C <-> CaC
            -1.0 * x.[137] // CaC | ligation: C + aC <-> CaC
            1.0 * x.[5] * x.[29] // C + aC | ligation: C + aC <-> CaC
        |]
        |> Array.sum


    // 138 - Caa
    let d138 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[138] // Caa | ligation: Ca + a <-> Caa
            1.0 * x.[24] * x.[6] // Ca + a | ligation: Ca + a <-> Caa
            -1.0 * x.[138] // Caa | ligation: C + aa <-> Caa
            1.0 * x.[5] * x.[30] // C + aa | ligation: C + aa <-> Caa
        |]
        |> Array.sum


    // 139 - Cab
    let d139 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[139] // Cab | ligation: Ca + b <-> Cab
            1.0 * x.[24] * x.[7] // Ca + b | ligation: Ca + b <-> Cab
            -1.0 * x.[139] // Cab | ligation: C + ab <-> Cab
            1.0 * x.[5] * x.[31] // C + ab | ligation: C + ab <-> Cab
        |]
        |> Array.sum


    // 140 - Cac
    let d140 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[140] // Cac | ligation: Ca + c <-> Cac
            1.0 * x.[24] * x.[8] // Ca + c | ligation: Ca + c <-> Cac
            -1.0 * x.[140] // Cac | ligation: C + ac <-> Cac
            1.0 * x.[5] * x.[32] // C + ac | ligation: C + ac <-> Cac
        |]
        |> Array.sum


    // 141 - CbA
    let d141 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[141] // CbA | ligation: Cb + A <-> CbA
            1.0 * x.[25] * x.[3] // Cb + A | ligation: Cb + A <-> CbA
            -1.0 * x.[141] // CbA | ligation: C + bA <-> CbA
            1.0 * x.[5] * x.[33] // C + bA | ligation: C + bA <-> CbA
        |]
        |> Array.sum


    // 142 - CbB
    let d142 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[142] // CbB | ligation: Cb + B <-> CbB
            1.0 * x.[25] * x.[4] // Cb + B | ligation: Cb + B <-> CbB
            -1.0 * x.[142] // CbB | ligation: C + bB <-> CbB
            1.0 * x.[5] * x.[34] // C + bB | ligation: C + bB <-> CbB
        |]
        |> Array.sum


    // 143 - CbC
    let d143 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[143] // CbC | ligation: Cb + C <-> CbC
            1.0 * x.[25] * x.[5] // Cb + C | ligation: Cb + C <-> CbC
            -1.0 * x.[143] // CbC | ligation: C + bC <-> CbC
            1.0 * x.[5] * x.[35] // C + bC | ligation: C + bC <-> CbC
        |]
        |> Array.sum


    // 144 - Cba
    let d144 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[144] // Cba | ligation: Cb + a <-> Cba
            1.0 * x.[25] * x.[6] // Cb + a | ligation: Cb + a <-> Cba
            -1.0 * x.[144] // Cba | ligation: C + ba <-> Cba
            1.0 * x.[5] * x.[36] // C + ba | ligation: C + ba <-> Cba
        |]
        |> Array.sum


    // 145 - Cbb
    let d145 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[145] // Cbb | ligation: Cb + b <-> Cbb
            1.0 * x.[25] * x.[7] // Cb + b | ligation: Cb + b <-> Cbb
            -1.0 * x.[145] // Cbb | ligation: C + bb <-> Cbb
            1.0 * x.[5] * x.[37] // C + bb | ligation: C + bb <-> Cbb
        |]
        |> Array.sum


    // 146 - Cbc
    let d146 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[146] // Cbc | ligation: Cb + c <-> Cbc
            1.0 * x.[25] * x.[8] // Cb + c | ligation: Cb + c <-> Cbc
            -1.0 * x.[146] // Cbc | ligation: C + bc <-> Cbc
            1.0 * x.[5] * x.[38] // C + bc | ligation: C + bc <-> Cbc
        |]
        |> Array.sum


    // 147 - CcA
    let d147 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[147] // CcA | ligation: Cc + A <-> CcA
            1.0 * x.[26] * x.[3] // Cc + A | ligation: Cc + A <-> CcA
            -1.0 * x.[147] // CcA | ligation: C + cA <-> CcA
            1.0 * x.[5] * x.[39] // C + cA | ligation: C + cA <-> CcA
        |]
        |> Array.sum


    // 148 - CcB
    let d148 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[148] // CcB | ligation: Cc + B <-> CcB
            1.0 * x.[26] * x.[4] // Cc + B | ligation: Cc + B <-> CcB
            -1.0 * x.[148] // CcB | ligation: C + cB <-> CcB
            1.0 * x.[5] * x.[40] // C + cB | ligation: C + cB <-> CcB
        |]
        |> Array.sum


    // 149 - CcC
    let d149 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[149] // CcC | ligation: Cc + C <-> CcC
            1.0 * x.[26] * x.[5] // Cc + C | ligation: Cc + C <-> CcC
            -1.0 * x.[149] // CcC | ligation: C + cC <-> CcC
            1.0 * x.[5] * x.[41] // C + cC | ligation: C + cC <-> CcC
        |]
        |> Array.sum


    // 150 - Cca
    let d150 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[150] // Cca | ligation: Cc + a <-> Cca
            1.0 * x.[26] * x.[6] // Cc + a | ligation: Cc + a <-> Cca
            -1.0 * x.[150] // Cca | ligation: C + ca <-> Cca
            1.0 * x.[5] * x.[42] // C + ca | ligation: C + ca <-> Cca
        |]
        |> Array.sum


    // 151 - Ccb
    let d151 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[151] // Ccb | ligation: Cc + b <-> Ccb
            1.0 * x.[26] * x.[7] // Cc + b | ligation: Cc + b <-> Ccb
            -1.0 * x.[151] // Ccb | ligation: C + cb <-> Ccb
            1.0 * x.[5] * x.[43] // C + cb | ligation: C + cb <-> Ccb
        |]
        |> Array.sum


    // 152 - Ccc
    let d152 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[152] // Ccc | ligation: Cc + c <-> Ccc
            1.0 * x.[26] * x.[8] // Cc + c | ligation: Cc + c <-> Ccc
            -1.0 * x.[152] // Ccc | ligation: C + cc <-> Ccc
            1.0 * x.[5] * x.[44] // C + cc | ligation: C + cc <-> Ccc
        |]
        |> Array.sum


    // 153 - aAA
    let d153 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[153] // aAA | ligation: aA + A <-> aAA
            1.0 * x.[27] * x.[3] // aA + A | ligation: aA + A <-> aAA
            -1.0 * x.[153] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[6] * x.[9] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 154 - aAB
    let d154 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[154] // aAB | ligation: aA + B <-> aAB
            1.0 * x.[27] * x.[4] // aA + B | ligation: aA + B <-> aAB
            -1.0 * x.[154] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[6] * x.[10] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 155 - aAC
    let d155 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[155] // aAC | ligation: aA + C <-> aAC
            1.0 * x.[27] * x.[5] // aA + C | ligation: aA + C <-> aAC
            -1.0 * x.[155] // aAC | ligation: a + AC <-> aAC
            1.0 * x.[6] * x.[11] // a + AC | ligation: a + AC <-> aAC
        |]
        |> Array.sum


    // 156 - aAa
    let d156 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[156] // aAa | ligation: aA + a <-> aAa
            1.0 * x.[27] * x.[6] // aA + a | ligation: aA + a <-> aAa
            -1.0 * x.[156] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[6] * x.[12] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 157 - aAb
    let d157 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[157] // aAb | ligation: aA + b <-> aAb
            1.0 * x.[27] * x.[7] // aA + b | ligation: aA + b <-> aAb
            -1.0 * x.[157] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[6] * x.[13] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 158 - aAc
    let d158 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[158] // aAc | ligation: aA + c <-> aAc
            1.0 * x.[27] * x.[8] // aA + c | ligation: aA + c <-> aAc
            -1.0 * x.[158] // aAc | ligation: a + Ac <-> aAc
            1.0 * x.[6] * x.[14] // a + Ac | ligation: a + Ac <-> aAc
        |]
        |> Array.sum


    // 159 - aBA
    let d159 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[159] // aBA | ligation: aB + A <-> aBA
            1.0 * x.[28] * x.[3] // aB + A | ligation: aB + A <-> aBA
            -1.0 * x.[159] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[6] * x.[15] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 160 - aBB
    let d160 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[160] // aBB | ligation: aB + B <-> aBB
            1.0 * x.[28] * x.[4] // aB + B | ligation: aB + B <-> aBB
            -1.0 * x.[160] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[6] * x.[16] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 161 - aBC
    let d161 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[161] // aBC | ligation: aB + C <-> aBC
            1.0 * x.[28] * x.[5] // aB + C | ligation: aB + C <-> aBC
            -1.0 * x.[161] // aBC | ligation: a + BC <-> aBC
            1.0 * x.[6] * x.[17] // a + BC | ligation: a + BC <-> aBC
        |]
        |> Array.sum


    // 162 - aBa
    let d162 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[162] // aBa | ligation: aB + a <-> aBa
            1.0 * x.[28] * x.[6] // aB + a | ligation: aB + a <-> aBa
            -1.0 * x.[162] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[6] * x.[18] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 163 - aBb
    let d163 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[163] // aBb | ligation: aB + b <-> aBb
            1.0 * x.[28] * x.[7] // aB + b | ligation: aB + b <-> aBb
            -1.0 * x.[163] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[6] * x.[19] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 164 - aBc
    let d164 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[164] // aBc | ligation: aB + c <-> aBc
            1.0 * x.[28] * x.[8] // aB + c | ligation: aB + c <-> aBc
            -1.0 * x.[164] // aBc | ligation: a + Bc <-> aBc
            1.0 * x.[6] * x.[20] // a + Bc | ligation: a + Bc <-> aBc
        |]
        |> Array.sum


    // 165 - aCA
    let d165 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[165] // aCA | ligation: aC + A <-> aCA
            1.0 * x.[29] * x.[3] // aC + A | ligation: aC + A <-> aCA
            -1.0 * x.[165] // aCA | ligation: a + CA <-> aCA
            1.0 * x.[6] * x.[21] // a + CA | ligation: a + CA <-> aCA
        |]
        |> Array.sum


    // 166 - aCB
    let d166 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[166] // aCB | ligation: aC + B <-> aCB
            1.0 * x.[29] * x.[4] // aC + B | ligation: aC + B <-> aCB
            -1.0 * x.[166] // aCB | ligation: a + CB <-> aCB
            1.0 * x.[6] * x.[22] // a + CB | ligation: a + CB <-> aCB
        |]
        |> Array.sum


    // 167 - aCC
    let d167 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[167] // aCC | ligation: aC + C <-> aCC
            1.0 * x.[29] * x.[5] // aC + C | ligation: aC + C <-> aCC
            -1.0 * x.[167] // aCC | ligation: a + CC <-> aCC
            1.0 * x.[6] * x.[23] // a + CC | ligation: a + CC <-> aCC
        |]
        |> Array.sum


    // 168 - aCa
    let d168 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[168] // aCa | ligation: aC + a <-> aCa
            1.0 * x.[29] * x.[6] // aC + a | ligation: aC + a <-> aCa
            -1.0 * x.[168] // aCa | ligation: a + Ca <-> aCa
            1.0 * x.[6] * x.[24] // a + Ca | ligation: a + Ca <-> aCa
        |]
        |> Array.sum


    // 169 - aCb
    let d169 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[169] // aCb | ligation: aC + b <-> aCb
            1.0 * x.[29] * x.[7] // aC + b | ligation: aC + b <-> aCb
            -1.0 * x.[169] // aCb | ligation: a + Cb <-> aCb
            1.0 * x.[6] * x.[25] // a + Cb | ligation: a + Cb <-> aCb
        |]
        |> Array.sum


    // 170 - aCc
    let d170 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[170] // aCc | ligation: aC + c <-> aCc
            1.0 * x.[29] * x.[8] // aC + c | ligation: aC + c <-> aCc
            -1.0 * x.[170] // aCc | ligation: a + Cc <-> aCc
            1.0 * x.[6] * x.[26] // a + Cc | ligation: a + Cc <-> aCc
        |]
        |> Array.sum


    // 171 - aaA
    let d171 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[171] * x.[103] // aaA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            2214.00742039413 * x.[6] * x.[27] * x.[103] // a + aA + Bab | catalytic ligation: a + aA + Bab <-> aaA + Bab
            -56.7694210357472 * x.[171] * x.[190] // aaA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            56.7694210357472 * x.[6] * x.[27] * x.[190] // a + aA + bAB | catalytic ligation: a + aA + bAB <-> aaA + bAB
            -60.8364303415014 * x.[171] * x.[244] // aaA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            60.8364303415014 * x.[6] * x.[27] * x.[244] // a + aA + caB | catalytic ligation: a + aA + caB <-> aaA + caB
            -2372.62078331855 * x.[171] * x.[121] // aaA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            2372.62078331855 * x.[6] * x.[27] * x.[121] // a + aA + CAb | catalytic ligation: a + aA + CAb <-> aaA + CAb
            -1.0 * x.[171] // aaA | ligation: aa + A <-> aaA
            1.0 * x.[30] * x.[3] // aa + A | ligation: aa + A <-> aaA
            -1.0 * x.[171] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[6] * x.[27] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 172 - aaB
    let d172 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[172] * x.[103] // aaB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            2214.00742039413 * x.[6] * x.[28] * x.[103] // a + aB + Bab | catalytic ligation: a + aB + Bab <-> aaB + Bab
            -56.7694210357472 * x.[172] * x.[190] // aaB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            56.7694210357472 * x.[6] * x.[28] * x.[190] // a + aB + bAB | catalytic ligation: a + aB + bAB <-> aaB + bAB
            -60.8364303415014 * x.[172] * x.[244] // aaB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            60.8364303415014 * x.[6] * x.[28] * x.[244] // a + aB + caB | catalytic ligation: a + aB + caB <-> aaB + caB
            -2372.62078331855 * x.[172] * x.[121] // aaB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            2372.62078331855 * x.[6] * x.[28] * x.[121] // a + aB + CAb | catalytic ligation: a + aB + CAb <-> aaB + CAb
            -1.0 * x.[172] // aaB | ligation: aa + B <-> aaB
            1.0 * x.[30] * x.[4] // aa + B | ligation: aa + B <-> aaB
            -1.0 * x.[172] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[6] * x.[28] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 173 - aaC
    let d173 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[173] * x.[103] // aaC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            2214.00742039413 * x.[6] * x.[29] * x.[103] // a + aC + Bab | catalytic ligation: a + aC + Bab <-> aaC + Bab
            -56.7694210357472 * x.[173] * x.[190] // aaC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            56.7694210357472 * x.[6] * x.[29] * x.[190] // a + aC + bAB | catalytic ligation: a + aC + bAB <-> aaC + bAB
            -60.8364303415014 * x.[173] * x.[244] // aaC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            60.8364303415014 * x.[6] * x.[29] * x.[244] // a + aC + caB | catalytic ligation: a + aC + caB <-> aaC + caB
            -2372.62078331855 * x.[173] * x.[121] // aaC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            2372.62078331855 * x.[6] * x.[29] * x.[121] // a + aC + CAb | catalytic ligation: a + aC + CAb <-> aaC + CAb
            -1.0 * x.[173] // aaC | ligation: aa + C <-> aaC
            1.0 * x.[30] * x.[5] // aa + C | ligation: aa + C <-> aaC
            -1.0 * x.[173] // aaC | ligation: a + aC <-> aaC
            1.0 * x.[6] * x.[29] // a + aC | ligation: a + aC <-> aaC
        |]
        |> Array.sum


    // 174 - aaa
    let d174 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            2214.00742039413 * x.[6] * x.[30] * x.[103] // a + aa + Bab | catalytic ligation: a + aa + Bab <-> aaa + Bab
            -56.7694210357472 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            56.7694210357472 * x.[6] * x.[30] * x.[190] // a + aa + bAB | catalytic ligation: a + aa + bAB <-> aaa + bAB
            -2214.00742039413 * x.[174] * x.[103] // aaa + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            2214.00742039413 * x.[30] * x.[6] * x.[103] // aa + a + Bab | catalytic ligation: aa + a + Bab <-> aaa + Bab
            -56.769421035747 * x.[174] * x.[190] // aaa + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            56.769421035747 * x.[30] * x.[6] * x.[190] // aa + a + bAB | catalytic ligation: aa + a + bAB <-> aaa + bAB
            -60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            60.8364303415014 * x.[30] * x.[6] * x.[244] // aa + a + caB | catalytic ligation: aa + a + caB <-> aaa + caB
            -2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            2372.62078331855 * x.[30] * x.[6] * x.[121] // aa + a + CAb | catalytic ligation: aa + a + CAb <-> aaa + CAb
            -60.8364303415014 * x.[174] * x.[244] // aaa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            60.8364303415014 * x.[6] * x.[30] * x.[244] // a + aa + caB | catalytic ligation: a + aa + caB <-> aaa + caB
            -2372.62078331855 * x.[174] * x.[121] // aaa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            2372.62078331855 * x.[6] * x.[30] * x.[121] // a + aa + CAb | catalytic ligation: a + aa + CAb <-> aaa + CAb
            -1.0 * x.[174] // aaa | ligation: aa + a <-> aaa
            1.0 * x.[30] * x.[6] // aa + a | ligation: aa + a <-> aaa
            -1.0 * x.[174] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[6] * x.[30] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 175 - aab
    let d175 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[175] * x.[103] // aab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            2214.00742039413 * x.[6] * x.[31] * x.[103] // a + ab + Bab | catalytic ligation: a + ab + Bab <-> aab + Bab
            -56.7694210357472 * x.[175] * x.[190] // aab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            56.7694210357472 * x.[6] * x.[31] * x.[190] // a + ab + bAB | catalytic ligation: a + ab + bAB <-> aab + bAB
            -60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            60.8364303415014 * x.[6] * x.[31] * x.[244] // a + ab + caB | catalytic ligation: a + ab + caB <-> aab + caB
            -2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            2372.62078331855 * x.[6] * x.[31] * x.[121] // a + ab + CAb | catalytic ligation: a + ab + CAb <-> aab + CAb
            -60.8364303415014 * x.[175] * x.[244] // aab + caB | catalytic ligation: aa + b + caB <-> aab + caB
            60.8364303415014 * x.[30] * x.[7] * x.[244] // aa + b + caB | catalytic ligation: aa + b + caB <-> aab + caB
            -2372.62078331855 * x.[175] * x.[121] // aab + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            2372.62078331855 * x.[30] * x.[7] * x.[121] // aa + b + CAb | catalytic ligation: aa + b + CAb <-> aab + CAb
            -1.0 * x.[175] // aab | ligation: aa + b <-> aab
            1.0 * x.[30] * x.[7] // aa + b | ligation: aa + b <-> aab
            -1.0 * x.[175] // aab | ligation: a + ab <-> aab
            1.0 * x.[6] * x.[31] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 176 - aac
    let d176 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[176] * x.[103] // aac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            2214.00742039413 * x.[6] * x.[32] * x.[103] // a + ac + Bab | catalytic ligation: a + ac + Bab <-> aac + Bab
            -56.7694210357472 * x.[176] * x.[190] // aac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            56.7694210357472 * x.[6] * x.[32] * x.[190] // a + ac + bAB | catalytic ligation: a + ac + bAB <-> aac + bAB
            -60.8364303415014 * x.[176] * x.[244] // aac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            60.8364303415014 * x.[6] * x.[32] * x.[244] // a + ac + caB | catalytic ligation: a + ac + caB <-> aac + caB
            -2372.62078331855 * x.[176] * x.[121] // aac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            2372.62078331855 * x.[6] * x.[32] * x.[121] // a + ac + CAb | catalytic ligation: a + ac + CAb <-> aac + CAb
            -1.0 * x.[176] // aac | ligation: aa + c <-> aac
            1.0 * x.[30] * x.[8] // aa + c | ligation: aa + c <-> aac
            -1.0 * x.[176] // aac | ligation: a + ac <-> aac
            1.0 * x.[6] * x.[32] // a + ac | ligation: a + ac <-> aac
        |]
        |> Array.sum


    // 177 - abA
    let d177 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[177] * x.[244] // abA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            60.8364303415014 * x.[6] * x.[33] * x.[244] // a + bA + caB | catalytic ligation: a + bA + caB <-> abA + caB
            -2372.62078331855 * x.[177] * x.[121] // abA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            2372.62078331855 * x.[6] * x.[33] * x.[121] // a + bA + CAb | catalytic ligation: a + bA + CAb <-> abA + CAb
            -1.0 * x.[177] // abA | ligation: ab + A <-> abA
            1.0 * x.[31] * x.[3] // ab + A | ligation: ab + A <-> abA
            -1.0 * x.[177] // abA | ligation: a + bA <-> abA
            1.0 * x.[6] * x.[33] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 178 - abB
    let d178 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[178] * x.[244] // abB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            60.8364303415014 * x.[6] * x.[34] * x.[244] // a + bB + caB | catalytic ligation: a + bB + caB <-> abB + caB
            -2372.62078331855 * x.[178] * x.[121] // abB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            2372.62078331855 * x.[6] * x.[34] * x.[121] // a + bB + CAb | catalytic ligation: a + bB + CAb <-> abB + CAb
            -1.0 * x.[178] // abB | ligation: ab + B <-> abB
            1.0 * x.[31] * x.[4] // ab + B | ligation: ab + B <-> abB
            -1.0 * x.[178] // abB | ligation: a + bB <-> abB
            1.0 * x.[6] * x.[34] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 179 - abC
    let d179 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[179] * x.[244] // abC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            60.8364303415014 * x.[6] * x.[35] * x.[244] // a + bC + caB | catalytic ligation: a + bC + caB <-> abC + caB
            -2372.62078331855 * x.[179] * x.[121] // abC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            2372.62078331855 * x.[6] * x.[35] * x.[121] // a + bC + CAb | catalytic ligation: a + bC + CAb <-> abC + CAb
            -1.0 * x.[179] // abC | ligation: ab + C <-> abC
            1.0 * x.[31] * x.[5] // ab + C | ligation: ab + C <-> abC
            -1.0 * x.[179] // abC | ligation: a + bC <-> abC
            1.0 * x.[6] * x.[35] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 180 - aba
    let d180 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[180] * x.[244] // aba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            60.8364303415014 * x.[6] * x.[36] * x.[244] // a + ba + caB | catalytic ligation: a + ba + caB <-> aba + caB
            -2372.62078331855 * x.[180] * x.[121] // aba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            2372.62078331855 * x.[6] * x.[36] * x.[121] // a + ba + CAb | catalytic ligation: a + ba + CAb <-> aba + CAb
            -1.0 * x.[180] // aba | ligation: ab + a <-> aba
            1.0 * x.[31] * x.[6] // ab + a | ligation: ab + a <-> aba
            -1.0 * x.[180] // aba | ligation: a + ba <-> aba
            1.0 * x.[6] * x.[36] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 181 - abb
    let d181 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[181] * x.[244] // abb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            60.8364303415014 * x.[6] * x.[37] * x.[244] // a + bb + caB | catalytic ligation: a + bb + caB <-> abb + caB
            -2372.62078331855 * x.[181] * x.[121] // abb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            2372.62078331855 * x.[6] * x.[37] * x.[121] // a + bb + CAb | catalytic ligation: a + bb + CAb <-> abb + CAb
            -1.0 * x.[181] // abb | ligation: ab + b <-> abb
            1.0 * x.[31] * x.[7] // ab + b | ligation: ab + b <-> abb
            -1.0 * x.[181] // abb | ligation: a + bb <-> abb
            1.0 * x.[6] * x.[37] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 182 - abc
    let d182 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[182] * x.[103] // abc + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            2214.00742039413 * x.[31] * x.[8] * x.[103] // ab + c + Bab | catalytic ligation: ab + c + Bab <-> abc + Bab
            -56.7694210357472 * x.[182] * x.[190] // abc + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            56.7694210357472 * x.[31] * x.[8] * x.[190] // ab + c + bAB | catalytic ligation: ab + c + bAB <-> abc + bAB
            -60.8364303415014 * x.[182] * x.[244] // abc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            60.8364303415014 * x.[6] * x.[38] * x.[244] // a + bc + caB | catalytic ligation: a + bc + caB <-> abc + caB
            -2372.62078331855 * x.[182] * x.[121] // abc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            2372.62078331855 * x.[6] * x.[38] * x.[121] // a + bc + CAb | catalytic ligation: a + bc + CAb <-> abc + CAb
            -1.0 * x.[182] // abc | ligation: ab + c <-> abc
            1.0 * x.[31] * x.[8] // ab + c | ligation: ab + c <-> abc
            -1.0 * x.[182] // abc | ligation: a + bc <-> abc
            1.0 * x.[6] * x.[38] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 183 - acA
    let d183 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[183] // acA | ligation: ac + A <-> acA
            1.0 * x.[32] * x.[3] // ac + A | ligation: ac + A <-> acA
            -1.0 * x.[183] // acA | ligation: a + cA <-> acA
            1.0 * x.[6] * x.[39] // a + cA | ligation: a + cA <-> acA
        |]
        |> Array.sum


    // 184 - acB
    let d184 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[184] // acB | ligation: ac + B <-> acB
            1.0 * x.[32] * x.[4] // ac + B | ligation: ac + B <-> acB
            -1.0 * x.[184] // acB | ligation: a + cB <-> acB
            1.0 * x.[6] * x.[40] // a + cB | ligation: a + cB <-> acB
        |]
        |> Array.sum


    // 185 - acC
    let d185 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[185] // acC | ligation: ac + C <-> acC
            1.0 * x.[32] * x.[5] // ac + C | ligation: ac + C <-> acC
            -1.0 * x.[185] // acC | ligation: a + cC <-> acC
            1.0 * x.[6] * x.[41] // a + cC | ligation: a + cC <-> acC
        |]
        |> Array.sum


    // 186 - aca
    let d186 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[186] // aca | ligation: ac + a <-> aca
            1.0 * x.[32] * x.[6] // ac + a | ligation: ac + a <-> aca
            -1.0 * x.[186] // aca | ligation: a + ca <-> aca
            1.0 * x.[6] * x.[42] // a + ca | ligation: a + ca <-> aca
        |]
        |> Array.sum


    // 187 - acb
    let d187 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[187] * x.[244] // acb + caB | catalytic ligation: ac + b + caB <-> acb + caB
            60.8364303415014 * x.[32] * x.[7] * x.[244] // ac + b + caB | catalytic ligation: ac + b + caB <-> acb + caB
            -2372.62078331855 * x.[187] * x.[121] // acb + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            2372.62078331855 * x.[32] * x.[7] * x.[121] // ac + b + CAb | catalytic ligation: ac + b + CAb <-> acb + CAb
            -1.0 * x.[187] // acb | ligation: ac + b <-> acb
            1.0 * x.[32] * x.[7] // ac + b | ligation: ac + b <-> acb
            -1.0 * x.[187] // acb | ligation: a + cb <-> acb
            1.0 * x.[6] * x.[43] // a + cb | ligation: a + cb <-> acb
        |]
        |> Array.sum


    // 188 - acc
    let d188 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[188] * x.[103] // acc + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            2214.00742039413 * x.[32] * x.[8] * x.[103] // ac + c + Bab | catalytic ligation: ac + c + Bab <-> acc + Bab
            -56.7694210357472 * x.[188] * x.[190] // acc + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            56.7694210357472 * x.[32] * x.[8] * x.[190] // ac + c + bAB | catalytic ligation: ac + c + bAB <-> acc + bAB
            -1.0 * x.[188] // acc | ligation: ac + c <-> acc
            1.0 * x.[32] * x.[8] // ac + c | ligation: ac + c <-> acc
            -1.0 * x.[188] // acc | ligation: a + cc <-> acc
            1.0 * x.[6] * x.[44] // a + cc | ligation: a + cc <-> acc
        |]
        |> Array.sum


    // 189 - bAA
    let d189 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[189] // bAA | ligation: bA + A <-> bAA
            1.0 * x.[33] * x.[3] // bA + A | ligation: bA + A <-> bAA
            -1.0 * x.[189] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[7] * x.[9] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 190 - bAB
    let d190 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[190] // bAB | ligation: bA + B <-> bAB
            1.0 * x.[33] * x.[4] // bA + B | ligation: bA + B <-> bAB
            -1.0 * x.[190] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[7] * x.[10] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 191 - bAC
    let d191 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[191] // bAC | ligation: bA + C <-> bAC
            1.0 * x.[33] * x.[5] // bA + C | ligation: bA + C <-> bAC
            -1.0 * x.[191] // bAC | ligation: b + AC <-> bAC
            1.0 * x.[7] * x.[11] // b + AC | ligation: b + AC <-> bAC
        |]
        |> Array.sum


    // 192 - bAa
    let d192 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[192] // bAa | ligation: bA + a <-> bAa
            1.0 * x.[33] * x.[6] // bA + a | ligation: bA + a <-> bAa
            -1.0 * x.[192] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[7] * x.[12] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 193 - bAb
    let d193 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[193] // bAb | ligation: bA + b <-> bAb
            1.0 * x.[33] * x.[7] // bA + b | ligation: bA + b <-> bAb
            -1.0 * x.[193] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[7] * x.[13] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 194 - bAc
    let d194 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[194] // bAc | ligation: bA + c <-> bAc
            1.0 * x.[33] * x.[8] // bA + c | ligation: bA + c <-> bAc
            -1.0 * x.[194] // bAc | ligation: b + Ac <-> bAc
            1.0 * x.[7] * x.[14] // b + Ac | ligation: b + Ac <-> bAc
        |]
        |> Array.sum


    // 195 - bBA
    let d195 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[195] // bBA | ligation: bB + A <-> bBA
            1.0 * x.[34] * x.[3] // bB + A | ligation: bB + A <-> bBA
            -1.0 * x.[195] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[7] * x.[15] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 196 - bBB
    let d196 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[196] // bBB | ligation: bB + B <-> bBB
            1.0 * x.[34] * x.[4] // bB + B | ligation: bB + B <-> bBB
            -1.0 * x.[196] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[7] * x.[16] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 197 - bBC
    let d197 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[197] // bBC | ligation: bB + C <-> bBC
            1.0 * x.[34] * x.[5] // bB + C | ligation: bB + C <-> bBC
            -1.0 * x.[197] // bBC | ligation: b + BC <-> bBC
            1.0 * x.[7] * x.[17] // b + BC | ligation: b + BC <-> bBC
        |]
        |> Array.sum


    // 198 - bBa
    let d198 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[198] // bBa | ligation: bB + a <-> bBa
            1.0 * x.[34] * x.[6] // bB + a | ligation: bB + a <-> bBa
            -1.0 * x.[198] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[7] * x.[18] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 199 - bBb
    let d199 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[199] // bBb | ligation: bB + b <-> bBb
            1.0 * x.[34] * x.[7] // bB + b | ligation: bB + b <-> bBb
            -1.0 * x.[199] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[7] * x.[19] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 200 - bBc
    let d200 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[200] // bBc | ligation: bB + c <-> bBc
            1.0 * x.[34] * x.[8] // bB + c | ligation: bB + c <-> bBc
            -1.0 * x.[200] // bBc | ligation: b + Bc <-> bBc
            1.0 * x.[7] * x.[20] // b + Bc | ligation: b + Bc <-> bBc
        |]
        |> Array.sum


    // 201 - bCA
    let d201 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[201] // bCA | ligation: bC + A <-> bCA
            1.0 * x.[35] * x.[3] // bC + A | ligation: bC + A <-> bCA
            -1.0 * x.[201] // bCA | ligation: b + CA <-> bCA
            1.0 * x.[7] * x.[21] // b + CA | ligation: b + CA <-> bCA
        |]
        |> Array.sum


    // 202 - bCB
    let d202 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[202] // bCB | ligation: bC + B <-> bCB
            1.0 * x.[35] * x.[4] // bC + B | ligation: bC + B <-> bCB
            -1.0 * x.[202] // bCB | ligation: b + CB <-> bCB
            1.0 * x.[7] * x.[22] // b + CB | ligation: b + CB <-> bCB
        |]
        |> Array.sum


    // 203 - bCC
    let d203 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[203] // bCC | ligation: bC + C <-> bCC
            1.0 * x.[35] * x.[5] // bC + C | ligation: bC + C <-> bCC
            -1.0 * x.[203] // bCC | ligation: b + CC <-> bCC
            1.0 * x.[7] * x.[23] // b + CC | ligation: b + CC <-> bCC
        |]
        |> Array.sum


    // 204 - bCa
    let d204 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[204] // bCa | ligation: bC + a <-> bCa
            1.0 * x.[35] * x.[6] // bC + a | ligation: bC + a <-> bCa
            -1.0 * x.[204] // bCa | ligation: b + Ca <-> bCa
            1.0 * x.[7] * x.[24] // b + Ca | ligation: b + Ca <-> bCa
        |]
        |> Array.sum


    // 205 - bCb
    let d205 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[205] // bCb | ligation: bC + b <-> bCb
            1.0 * x.[35] * x.[7] // bC + b | ligation: bC + b <-> bCb
            -1.0 * x.[205] // bCb | ligation: b + Cb <-> bCb
            1.0 * x.[7] * x.[25] // b + Cb | ligation: b + Cb <-> bCb
        |]
        |> Array.sum


    // 206 - bCc
    let d206 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[206] // bCc | ligation: bC + c <-> bCc
            1.0 * x.[35] * x.[8] // bC + c | ligation: bC + c <-> bCc
            -1.0 * x.[206] // bCc | ligation: b + Cc <-> bCc
            1.0 * x.[7] * x.[26] // b + Cc | ligation: b + Cc <-> bCc
        |]
        |> Array.sum


    // 207 - baA
    let d207 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[207] // baA | ligation: ba + A <-> baA
            1.0 * x.[36] * x.[3] // ba + A | ligation: ba + A <-> baA
            -1.0 * x.[207] // baA | ligation: b + aA <-> baA
            1.0 * x.[7] * x.[27] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 208 - baB
    let d208 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[208] // baB | ligation: ba + B <-> baB
            1.0 * x.[36] * x.[4] // ba + B | ligation: ba + B <-> baB
            -1.0 * x.[208] // baB | ligation: b + aB <-> baB
            1.0 * x.[7] * x.[28] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 209 - baC
    let d209 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[209] // baC | ligation: ba + C <-> baC
            1.0 * x.[36] * x.[5] // ba + C | ligation: ba + C <-> baC
            -1.0 * x.[209] // baC | ligation: b + aC <-> baC
            1.0 * x.[7] * x.[29] // b + aC | ligation: b + aC <-> baC
        |]
        |> Array.sum


    // 210 - baa
    let d210 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[210] * x.[103] // baa + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            2214.00742039413 * x.[36] * x.[6] * x.[103] // ba + a + Bab | catalytic ligation: ba + a + Bab <-> baa + Bab
            -56.7694210357472 * x.[210] * x.[190] // baa + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            56.7694210357472 * x.[36] * x.[6] * x.[190] // ba + a + bAB | catalytic ligation: ba + a + bAB <-> baa + bAB
            -60.8364303415014 * x.[210] * x.[244] // baa + caB | catalytic ligation: ba + a + caB <-> baa + caB
            60.8364303415014 * x.[36] * x.[6] * x.[244] // ba + a + caB | catalytic ligation: ba + a + caB <-> baa + caB
            -2372.62078331855 * x.[210] * x.[121] // baa + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            2372.62078331855 * x.[36] * x.[6] * x.[121] // ba + a + CAb | catalytic ligation: ba + a + CAb <-> baa + CAb
            -1.0 * x.[210] // baa | ligation: ba + a <-> baa
            1.0 * x.[36] * x.[6] // ba + a | ligation: ba + a <-> baa
            -1.0 * x.[210] // baa | ligation: b + aa <-> baa
            1.0 * x.[7] * x.[30] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 211 - bab
    let d211 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[211] * x.[244] // bab + caB | catalytic ligation: ba + b + caB <-> bab + caB
            60.8364303415014 * x.[36] * x.[7] * x.[244] // ba + b + caB | catalytic ligation: ba + b + caB <-> bab + caB
            -2372.62078331855 * x.[211] * x.[121] // bab + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            2372.62078331855 * x.[36] * x.[7] * x.[121] // ba + b + CAb | catalytic ligation: ba + b + CAb <-> bab + CAb
            -1.0 * x.[211] // bab | ligation: ba + b <-> bab
            1.0 * x.[36] * x.[7] // ba + b | ligation: ba + b <-> bab
            -1.0 * x.[211] // bab | ligation: b + ab <-> bab
            1.0 * x.[7] * x.[31] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 212 - bac
    let d212 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[212] // bac | ligation: ba + c <-> bac
            1.0 * x.[36] * x.[8] // ba + c | ligation: ba + c <-> bac
            -1.0 * x.[212] // bac | ligation: b + ac <-> bac
            1.0 * x.[7] * x.[32] // b + ac | ligation: b + ac <-> bac
        |]
        |> Array.sum


    // 213 - bbA
    let d213 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[213] // bbA | ligation: bb + A <-> bbA
            1.0 * x.[37] * x.[3] // bb + A | ligation: bb + A <-> bbA
            -1.0 * x.[213] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[7] * x.[33] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 214 - bbB
    let d214 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[214] // bbB | ligation: bb + B <-> bbB
            1.0 * x.[37] * x.[4] // bb + B | ligation: bb + B <-> bbB
            -1.0 * x.[214] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[7] * x.[34] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 215 - bbC
    let d215 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[215] // bbC | ligation: bb + C <-> bbC
            1.0 * x.[37] * x.[5] // bb + C | ligation: bb + C <-> bbC
            -1.0 * x.[215] // bbC | ligation: b + bC <-> bbC
            1.0 * x.[7] * x.[35] // b + bC | ligation: b + bC <-> bbC
        |]
        |> Array.sum


    // 216 - bba
    let d216 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[216] // bba | ligation: bb + a <-> bba
            1.0 * x.[37] * x.[6] // bb + a | ligation: bb + a <-> bba
            -1.0 * x.[216] // bba | ligation: b + ba <-> bba
            1.0 * x.[7] * x.[36] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 217 - bbb
    let d217 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[217] // bbb | ligation: bb + b <-> bbb
            1.0 * x.[37] * x.[7] // bb + b | ligation: bb + b <-> bbb
            -1.0 * x.[217] // bbb | ligation: b + bb <-> bbb
            1.0 * x.[7] * x.[37] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum


    // 218 - bbc
    let d218 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[218] * x.[103] // bbc + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            2214.00742039413 * x.[37] * x.[8] * x.[103] // bb + c + Bab | catalytic ligation: bb + c + Bab <-> bbc + Bab
            -56.7694210357472 * x.[218] * x.[190] // bbc + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            56.7694210357472 * x.[37] * x.[8] * x.[190] // bb + c + bAB | catalytic ligation: bb + c + bAB <-> bbc + bAB
            -1.0 * x.[218] // bbc | ligation: bb + c <-> bbc
            1.0 * x.[37] * x.[8] // bb + c | ligation: bb + c <-> bbc
            -1.0 * x.[218] // bbc | ligation: b + bc <-> bbc
            1.0 * x.[7] * x.[38] // b + bc | ligation: b + bc <-> bbc
        |]
        |> Array.sum


    // 219 - bcA
    let d219 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[219] * x.[103] // bcA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            2214.00742039413 * x.[7] * x.[39] * x.[103] // b + cA + Bab | catalytic ligation: b + cA + Bab <-> bcA + Bab
            -56.7694210357472 * x.[219] * x.[190] // bcA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            56.7694210357472 * x.[7] * x.[39] * x.[190] // b + cA + bAB | catalytic ligation: b + cA + bAB <-> bcA + bAB
            -1.0 * x.[219] // bcA | ligation: bc + A <-> bcA
            1.0 * x.[38] * x.[3] // bc + A | ligation: bc + A <-> bcA
            -1.0 * x.[219] // bcA | ligation: b + cA <-> bcA
            1.0 * x.[7] * x.[39] // b + cA | ligation: b + cA <-> bcA
        |]
        |> Array.sum


    // 220 - bcB
    let d220 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[220] * x.[103] // bcB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            2214.00742039413 * x.[7] * x.[40] * x.[103] // b + cB + Bab | catalytic ligation: b + cB + Bab <-> bcB + Bab
            -56.7694210357472 * x.[220] * x.[190] // bcB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            56.7694210357472 * x.[7] * x.[40] * x.[190] // b + cB + bAB | catalytic ligation: b + cB + bAB <-> bcB + bAB
            -1.0 * x.[220] // bcB | ligation: bc + B <-> bcB
            1.0 * x.[38] * x.[4] // bc + B | ligation: bc + B <-> bcB
            -1.0 * x.[220] // bcB | ligation: b + cB <-> bcB
            1.0 * x.[7] * x.[40] // b + cB | ligation: b + cB <-> bcB
        |]
        |> Array.sum


    // 221 - bcC
    let d221 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[221] * x.[103] // bcC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            2214.00742039413 * x.[7] * x.[41] * x.[103] // b + cC + Bab | catalytic ligation: b + cC + Bab <-> bcC + Bab
            -56.7694210357472 * x.[221] * x.[190] // bcC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            56.7694210357472 * x.[7] * x.[41] * x.[190] // b + cC + bAB | catalytic ligation: b + cC + bAB <-> bcC + bAB
            -1.0 * x.[221] // bcC | ligation: bc + C <-> bcC
            1.0 * x.[38] * x.[5] // bc + C | ligation: bc + C <-> bcC
            -1.0 * x.[221] // bcC | ligation: b + cC <-> bcC
            1.0 * x.[7] * x.[41] // b + cC | ligation: b + cC <-> bcC
        |]
        |> Array.sum


    // 222 - bca
    let d222 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[222] * x.[103] // bca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            2214.00742039413 * x.[7] * x.[42] * x.[103] // b + ca + Bab | catalytic ligation: b + ca + Bab <-> bca + Bab
            -56.7694210357472 * x.[222] * x.[190] // bca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            56.7694210357472 * x.[7] * x.[42] * x.[190] // b + ca + bAB | catalytic ligation: b + ca + bAB <-> bca + bAB
            -1.0 * x.[222] // bca | ligation: bc + a <-> bca
            1.0 * x.[38] * x.[6] // bc + a | ligation: bc + a <-> bca
            -1.0 * x.[222] // bca | ligation: b + ca <-> bca
            1.0 * x.[7] * x.[42] // b + ca | ligation: b + ca <-> bca
        |]
        |> Array.sum


    // 223 - bcb
    let d223 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[223] * x.[103] // bcb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            2214.00742039413 * x.[7] * x.[43] * x.[103] // b + cb + Bab | catalytic ligation: b + cb + Bab <-> bcb + Bab
            -56.7694210357472 * x.[223] * x.[190] // bcb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            56.7694210357472 * x.[7] * x.[43] * x.[190] // b + cb + bAB | catalytic ligation: b + cb + bAB <-> bcb + bAB
            -60.8364303415014 * x.[223] * x.[244] // bcb + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            60.8364303415014 * x.[38] * x.[7] * x.[244] // bc + b + caB | catalytic ligation: bc + b + caB <-> bcb + caB
            -2372.62078331855 * x.[223] * x.[121] // bcb + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            2372.62078331855 * x.[38] * x.[7] * x.[121] // bc + b + CAb | catalytic ligation: bc + b + CAb <-> bcb + CAb
            -1.0 * x.[223] // bcb | ligation: bc + b <-> bcb
            1.0 * x.[38] * x.[7] // bc + b | ligation: bc + b <-> bcb
            -1.0 * x.[223] // bcb | ligation: b + cb <-> bcb
            1.0 * x.[7] * x.[43] // b + cb | ligation: b + cb <-> bcb
        |]
        |> Array.sum


    // 224 - bcc
    let d224 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            2214.00742039413 * x.[38] * x.[8] * x.[103] // bc + c + Bab | catalytic ligation: bc + c + Bab <-> bcc + Bab
            -56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            56.7694210357472 * x.[38] * x.[8] * x.[190] // bc + c + bAB | catalytic ligation: bc + c + bAB <-> bcc + bAB
            -2214.00742039413 * x.[224] * x.[103] // bcc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            2214.00742039413 * x.[7] * x.[44] * x.[103] // b + cc + Bab | catalytic ligation: b + cc + Bab <-> bcc + Bab
            -56.7694210357472 * x.[224] * x.[190] // bcc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            56.7694210357472 * x.[7] * x.[44] * x.[190] // b + cc + bAB | catalytic ligation: b + cc + bAB <-> bcc + bAB
            -1.0 * x.[224] // bcc | ligation: bc + c <-> bcc
            1.0 * x.[38] * x.[8] // bc + c | ligation: bc + c <-> bcc
            -1.0 * x.[224] // bcc | ligation: b + cc <-> bcc
            1.0 * x.[7] * x.[44] // b + cc | ligation: b + cc <-> bcc
        |]
        |> Array.sum


    // 225 - cAA
    let d225 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[225] // cAA | ligation: cA + A <-> cAA
            1.0 * x.[39] * x.[3] // cA + A | ligation: cA + A <-> cAA
            -1.0 * x.[225] // cAA | ligation: c + AA <-> cAA
            1.0 * x.[8] * x.[9] // c + AA | ligation: c + AA <-> cAA
        |]
        |> Array.sum


    // 226 - cAB
    let d226 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[226] // cAB | ligation: cA + B <-> cAB
            1.0 * x.[39] * x.[4] // cA + B | ligation: cA + B <-> cAB
            -1.0 * x.[226] // cAB | ligation: c + AB <-> cAB
            1.0 * x.[8] * x.[10] // c + AB | ligation: c + AB <-> cAB
        |]
        |> Array.sum


    // 227 - cAC
    let d227 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[227] // cAC | ligation: cA + C <-> cAC
            1.0 * x.[39] * x.[5] // cA + C | ligation: cA + C <-> cAC
            -1.0 * x.[227] // cAC | ligation: c + AC <-> cAC
            1.0 * x.[8] * x.[11] // c + AC | ligation: c + AC <-> cAC
        |]
        |> Array.sum


    // 228 - cAa
    let d228 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[228] // cAa | ligation: cA + a <-> cAa
            1.0 * x.[39] * x.[6] // cA + a | ligation: cA + a <-> cAa
            -1.0 * x.[228] // cAa | ligation: c + Aa <-> cAa
            1.0 * x.[8] * x.[12] // c + Aa | ligation: c + Aa <-> cAa
        |]
        |> Array.sum


    // 229 - cAb
    let d229 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[229] // cAb | ligation: cA + b <-> cAb
            1.0 * x.[39] * x.[7] // cA + b | ligation: cA + b <-> cAb
            -1.0 * x.[229] // cAb | ligation: c + Ab <-> cAb
            1.0 * x.[8] * x.[13] // c + Ab | ligation: c + Ab <-> cAb
        |]
        |> Array.sum


    // 230 - cAc
    let d230 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[230] // cAc | ligation: cA + c <-> cAc
            1.0 * x.[39] * x.[8] // cA + c | ligation: cA + c <-> cAc
            -1.0 * x.[230] // cAc | ligation: c + Ac <-> cAc
            1.0 * x.[8] * x.[14] // c + Ac | ligation: c + Ac <-> cAc
        |]
        |> Array.sum


    // 231 - cBA
    let d231 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[231] // cBA | ligation: cB + A <-> cBA
            1.0 * x.[40] * x.[3] // cB + A | ligation: cB + A <-> cBA
            -1.0 * x.[231] // cBA | ligation: c + BA <-> cBA
            1.0 * x.[8] * x.[15] // c + BA | ligation: c + BA <-> cBA
        |]
        |> Array.sum


    // 232 - cBB
    let d232 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[232] // cBB | ligation: cB + B <-> cBB
            1.0 * x.[40] * x.[4] // cB + B | ligation: cB + B <-> cBB
            -1.0 * x.[232] // cBB | ligation: c + BB <-> cBB
            1.0 * x.[8] * x.[16] // c + BB | ligation: c + BB <-> cBB
        |]
        |> Array.sum


    // 233 - cBC
    let d233 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[233] // cBC | ligation: cB + C <-> cBC
            1.0 * x.[40] * x.[5] // cB + C | ligation: cB + C <-> cBC
            -1.0 * x.[233] // cBC | ligation: c + BC <-> cBC
            1.0 * x.[8] * x.[17] // c + BC | ligation: c + BC <-> cBC
        |]
        |> Array.sum


    // 234 - cBa
    let d234 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[234] // cBa | ligation: cB + a <-> cBa
            1.0 * x.[40] * x.[6] // cB + a | ligation: cB + a <-> cBa
            -1.0 * x.[234] // cBa | ligation: c + Ba <-> cBa
            1.0 * x.[8] * x.[18] // c + Ba | ligation: c + Ba <-> cBa
        |]
        |> Array.sum


    // 235 - cBb
    let d235 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[235] // cBb | ligation: cB + b <-> cBb
            1.0 * x.[40] * x.[7] // cB + b | ligation: cB + b <-> cBb
            -1.0 * x.[235] // cBb | ligation: c + Bb <-> cBb
            1.0 * x.[8] * x.[19] // c + Bb | ligation: c + Bb <-> cBb
        |]
        |> Array.sum


    // 236 - cBc
    let d236 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[236] // cBc | ligation: cB + c <-> cBc
            1.0 * x.[40] * x.[8] // cB + c | ligation: cB + c <-> cBc
            -1.0 * x.[236] // cBc | ligation: c + Bc <-> cBc
            1.0 * x.[8] * x.[20] // c + Bc | ligation: c + Bc <-> cBc
        |]
        |> Array.sum


    // 237 - cCA
    let d237 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[237] // cCA | ligation: cC + A <-> cCA
            1.0 * x.[41] * x.[3] // cC + A | ligation: cC + A <-> cCA
            -1.0 * x.[237] // cCA | ligation: c + CA <-> cCA
            1.0 * x.[8] * x.[21] // c + CA | ligation: c + CA <-> cCA
        |]
        |> Array.sum


    // 238 - cCB
    let d238 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[238] // cCB | ligation: cC + B <-> cCB
            1.0 * x.[41] * x.[4] // cC + B | ligation: cC + B <-> cCB
            -1.0 * x.[238] // cCB | ligation: c + CB <-> cCB
            1.0 * x.[8] * x.[22] // c + CB | ligation: c + CB <-> cCB
        |]
        |> Array.sum


    // 239 - cCC
    let d239 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[239] // cCC | ligation: cC + C <-> cCC
            1.0 * x.[41] * x.[5] // cC + C | ligation: cC + C <-> cCC
            -1.0 * x.[239] // cCC | ligation: c + CC <-> cCC
            1.0 * x.[8] * x.[23] // c + CC | ligation: c + CC <-> cCC
        |]
        |> Array.sum


    // 240 - cCa
    let d240 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[240] // cCa | ligation: cC + a <-> cCa
            1.0 * x.[41] * x.[6] // cC + a | ligation: cC + a <-> cCa
            -1.0 * x.[240] // cCa | ligation: c + Ca <-> cCa
            1.0 * x.[8] * x.[24] // c + Ca | ligation: c + Ca <-> cCa
        |]
        |> Array.sum


    // 241 - cCb
    let d241 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[241] // cCb | ligation: cC + b <-> cCb
            1.0 * x.[41] * x.[7] // cC + b | ligation: cC + b <-> cCb
            -1.0 * x.[241] // cCb | ligation: c + Cb <-> cCb
            1.0 * x.[8] * x.[25] // c + Cb | ligation: c + Cb <-> cCb
        |]
        |> Array.sum


    // 242 - cCc
    let d242 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[242] // cCc | ligation: cC + c <-> cCc
            1.0 * x.[41] * x.[8] // cC + c | ligation: cC + c <-> cCc
            -1.0 * x.[242] // cCc | ligation: c + Cc <-> cCc
            1.0 * x.[8] * x.[26] // c + Cc | ligation: c + Cc <-> cCc
        |]
        |> Array.sum


    // 243 - caA
    let d243 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[243] // caA | ligation: ca + A <-> caA
            1.0 * x.[42] * x.[3] // ca + A | ligation: ca + A <-> caA
            -1.0 * x.[243] // caA | ligation: c + aA <-> caA
            1.0 * x.[8] * x.[27] // c + aA | ligation: c + aA <-> caA
        |]
        |> Array.sum


    // 244 - caB
    let d244 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[244] // caB | ligation: ca + B <-> caB
            1.0 * x.[42] * x.[4] // ca + B | ligation: ca + B <-> caB
            -1.0 * x.[244] // caB | ligation: c + aB <-> caB
            1.0 * x.[8] * x.[28] // c + aB | ligation: c + aB <-> caB
        |]
        |> Array.sum


    // 245 - caC
    let d245 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[245] // caC | ligation: ca + C <-> caC
            1.0 * x.[42] * x.[5] // ca + C | ligation: ca + C <-> caC
            -1.0 * x.[245] // caC | ligation: c + aC <-> caC
            1.0 * x.[8] * x.[29] // c + aC | ligation: c + aC <-> caC
        |]
        |> Array.sum


    // 246 - caa
    let d246 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[246] * x.[103] // caa + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            2214.00742039413 * x.[42] * x.[6] * x.[103] // ca + a + Bab | catalytic ligation: ca + a + Bab <-> caa + Bab
            -56.7694210357472 * x.[246] * x.[190] // caa + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            56.7694210357472 * x.[42] * x.[6] * x.[190] // ca + a + bAB | catalytic ligation: ca + a + bAB <-> caa + bAB
            -60.8364303415014 * x.[246] * x.[244] // caa + caB | catalytic ligation: ca + a + caB <-> caa + caB
            60.8364303415014 * x.[42] * x.[6] * x.[244] // ca + a + caB | catalytic ligation: ca + a + caB <-> caa + caB
            -2372.62078331855 * x.[246] * x.[121] // caa + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            2372.62078331855 * x.[42] * x.[6] * x.[121] // ca + a + CAb | catalytic ligation: ca + a + CAb <-> caa + CAb
            -1.0 * x.[246] // caa | ligation: ca + a <-> caa
            1.0 * x.[42] * x.[6] // ca + a | ligation: ca + a <-> caa
            -1.0 * x.[246] // caa | ligation: c + aa <-> caa
            1.0 * x.[8] * x.[30] // c + aa | ligation: c + aa <-> caa
        |]
        |> Array.sum


    // 247 - cab
    let d247 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[247] * x.[244] // cab + caB | catalytic ligation: ca + b + caB <-> cab + caB
            60.8364303415014 * x.[42] * x.[7] * x.[244] // ca + b + caB | catalytic ligation: ca + b + caB <-> cab + caB
            -2372.62078331855 * x.[247] * x.[121] // cab + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            2372.62078331855 * x.[42] * x.[7] * x.[121] // ca + b + CAb | catalytic ligation: ca + b + CAb <-> cab + CAb
            -1.0 * x.[247] // cab | ligation: ca + b <-> cab
            1.0 * x.[42] * x.[7] // ca + b | ligation: ca + b <-> cab
            -1.0 * x.[247] // cab | ligation: c + ab <-> cab
            1.0 * x.[8] * x.[31] // c + ab | ligation: c + ab <-> cab
        |]
        |> Array.sum


    // 248 - cac
    let d248 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[248] // cac | ligation: ca + c <-> cac
            1.0 * x.[42] * x.[8] // ca + c | ligation: ca + c <-> cac
            -1.0 * x.[248] // cac | ligation: c + ac <-> cac
            1.0 * x.[8] * x.[32] // c + ac | ligation: c + ac <-> cac
        |]
        |> Array.sum


    // 249 - cbA
    let d249 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[249] * x.[244] // cbA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            60.8364303415014 * x.[8] * x.[33] * x.[244] // c + bA + caB | catalytic ligation: c + bA + caB <-> cbA + caB
            -2372.62078331855 * x.[249] * x.[121] // cbA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            2372.62078331855 * x.[8] * x.[33] * x.[121] // c + bA + CAb | catalytic ligation: c + bA + CAb <-> cbA + CAb
            -1.0 * x.[249] // cbA | ligation: cb + A <-> cbA
            1.0 * x.[43] * x.[3] // cb + A | ligation: cb + A <-> cbA
            -1.0 * x.[249] // cbA | ligation: c + bA <-> cbA
            1.0 * x.[8] * x.[33] // c + bA | ligation: c + bA <-> cbA
        |]
        |> Array.sum


    // 250 - cbB
    let d250 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[250] * x.[244] // cbB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            60.8364303415014 * x.[8] * x.[34] * x.[244] // c + bB + caB | catalytic ligation: c + bB + caB <-> cbB + caB
            -2372.62078331855 * x.[250] * x.[121] // cbB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            2372.62078331855 * x.[8] * x.[34] * x.[121] // c + bB + CAb | catalytic ligation: c + bB + CAb <-> cbB + CAb
            -1.0 * x.[250] // cbB | ligation: cb + B <-> cbB
            1.0 * x.[43] * x.[4] // cb + B | ligation: cb + B <-> cbB
            -1.0 * x.[250] // cbB | ligation: c + bB <-> cbB
            1.0 * x.[8] * x.[34] // c + bB | ligation: c + bB <-> cbB
        |]
        |> Array.sum


    // 251 - cbC
    let d251 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[251] * x.[244] // cbC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            60.8364303415014 * x.[8] * x.[35] * x.[244] // c + bC + caB | catalytic ligation: c + bC + caB <-> cbC + caB
            -2372.62078331855 * x.[251] * x.[121] // cbC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            2372.62078331855 * x.[8] * x.[35] * x.[121] // c + bC + CAb | catalytic ligation: c + bC + CAb <-> cbC + CAb
            -1.0 * x.[251] // cbC | ligation: cb + C <-> cbC
            1.0 * x.[43] * x.[5] // cb + C | ligation: cb + C <-> cbC
            -1.0 * x.[251] // cbC | ligation: c + bC <-> cbC
            1.0 * x.[8] * x.[35] // c + bC | ligation: c + bC <-> cbC
        |]
        |> Array.sum


    // 252 - cba
    let d252 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[252] * x.[244] // cba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            60.8364303415014 * x.[8] * x.[36] * x.[244] // c + ba + caB | catalytic ligation: c + ba + caB <-> cba + caB
            -2372.62078331855 * x.[252] * x.[121] // cba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            2372.62078331855 * x.[8] * x.[36] * x.[121] // c + ba + CAb | catalytic ligation: c + ba + CAb <-> cba + CAb
            -1.0 * x.[252] // cba | ligation: cb + a <-> cba
            1.0 * x.[43] * x.[6] // cb + a | ligation: cb + a <-> cba
            -1.0 * x.[252] // cba | ligation: c + ba <-> cba
            1.0 * x.[8] * x.[36] // c + ba | ligation: c + ba <-> cba
        |]
        |> Array.sum


    // 253 - cbb
    let d253 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -60.8364303415014 * x.[253] * x.[244] // cbb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            60.8364303415014 * x.[8] * x.[37] * x.[244] // c + bb + caB | catalytic ligation: c + bb + caB <-> cbb + caB
            -2372.62078331855 * x.[253] * x.[121] // cbb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            2372.62078331855 * x.[8] * x.[37] * x.[121] // c + bb + CAb | catalytic ligation: c + bb + CAb <-> cbb + CAb
            -1.0 * x.[253] // cbb | ligation: cb + b <-> cbb
            1.0 * x.[43] * x.[7] // cb + b | ligation: cb + b <-> cbb
            -1.0 * x.[253] // cbb | ligation: c + bb <-> cbb
            1.0 * x.[8] * x.[37] // c + bb | ligation: c + bb <-> cbb
        |]
        |> Array.sum


    // 254 - cbc
    let d254 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[254] * x.[103] // cbc + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            2214.00742039413 * x.[43] * x.[8] * x.[103] // cb + c + Bab | catalytic ligation: cb + c + Bab <-> cbc + Bab
            -56.7694210357472 * x.[254] * x.[190] // cbc + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            56.7694210357472 * x.[43] * x.[8] * x.[190] // cb + c + bAB | catalytic ligation: cb + c + bAB <-> cbc + bAB
            -60.8364303415014 * x.[254] * x.[244] // cbc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            60.8364303415014 * x.[8] * x.[38] * x.[244] // c + bc + caB | catalytic ligation: c + bc + caB <-> cbc + caB
            -2372.62078331855 * x.[254] * x.[121] // cbc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            2372.62078331855 * x.[8] * x.[38] * x.[121] // c + bc + CAb | catalytic ligation: c + bc + CAb <-> cbc + CAb
            -1.0 * x.[254] // cbc | ligation: cb + c <-> cbc
            1.0 * x.[43] * x.[8] // cb + c | ligation: cb + c <-> cbc
            -1.0 * x.[254] // cbc | ligation: c + bc <-> cbc
            1.0 * x.[8] * x.[38] // c + bc | ligation: c + bc <-> cbc
        |]
        |> Array.sum


    // 255 - ccA
    let d255 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[255] * x.[103] // ccA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            2214.00742039413 * x.[8] * x.[39] * x.[103] // c + cA + Bab | catalytic ligation: c + cA + Bab <-> ccA + Bab
            -56.7694210357472 * x.[255] * x.[190] // ccA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            56.7694210357472 * x.[8] * x.[39] * x.[190] // c + cA + bAB | catalytic ligation: c + cA + bAB <-> ccA + bAB
            -1.0 * x.[255] // ccA | ligation: cc + A <-> ccA
            1.0 * x.[44] * x.[3] // cc + A | ligation: cc + A <-> ccA
            -1.0 * x.[255] // ccA | ligation: c + cA <-> ccA
            1.0 * x.[8] * x.[39] // c + cA | ligation: c + cA <-> ccA
        |]
        |> Array.sum


    // 256 - ccB
    let d256 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[256] * x.[103] // ccB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            2214.00742039413 * x.[8] * x.[40] * x.[103] // c + cB + Bab | catalytic ligation: c + cB + Bab <-> ccB + Bab
            -56.7694210357472 * x.[256] * x.[190] // ccB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            56.7694210357472 * x.[8] * x.[40] * x.[190] // c + cB + bAB | catalytic ligation: c + cB + bAB <-> ccB + bAB
            -1.0 * x.[256] // ccB | ligation: cc + B <-> ccB
            1.0 * x.[44] * x.[4] // cc + B | ligation: cc + B <-> ccB
            -1.0 * x.[256] // ccB | ligation: c + cB <-> ccB
            1.0 * x.[8] * x.[40] // c + cB | ligation: c + cB <-> ccB
        |]
        |> Array.sum


    // 257 - ccC
    let d257 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[257] * x.[103] // ccC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            2214.00742039413 * x.[8] * x.[41] * x.[103] // c + cC + Bab | catalytic ligation: c + cC + Bab <-> ccC + Bab
            -56.7694210357472 * x.[257] * x.[190] // ccC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            56.7694210357472 * x.[8] * x.[41] * x.[190] // c + cC + bAB | catalytic ligation: c + cC + bAB <-> ccC + bAB
            -1.0 * x.[257] // ccC | ligation: cc + C <-> ccC
            1.0 * x.[44] * x.[5] // cc + C | ligation: cc + C <-> ccC
            -1.0 * x.[257] // ccC | ligation: c + cC <-> ccC
            1.0 * x.[8] * x.[41] // c + cC | ligation: c + cC <-> ccC
        |]
        |> Array.sum


    // 258 - cca
    let d258 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[258] * x.[103] // cca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            2214.00742039413 * x.[8] * x.[42] * x.[103] // c + ca + Bab | catalytic ligation: c + ca + Bab <-> cca + Bab
            -56.7694210357472 * x.[258] * x.[190] // cca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            56.7694210357472 * x.[8] * x.[42] * x.[190] // c + ca + bAB | catalytic ligation: c + ca + bAB <-> cca + bAB
            -1.0 * x.[258] // cca | ligation: cc + a <-> cca
            1.0 * x.[44] * x.[6] // cc + a | ligation: cc + a <-> cca
            -1.0 * x.[258] // cca | ligation: c + ca <-> cca
            1.0 * x.[8] * x.[42] // c + ca | ligation: c + ca <-> cca
        |]
        |> Array.sum


    // 259 - ccb
    let d259 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[259] * x.[103] // ccb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            2214.00742039413 * x.[8] * x.[43] * x.[103] // c + cb + Bab | catalytic ligation: c + cb + Bab <-> ccb + Bab
            -56.7694210357472 * x.[259] * x.[190] // ccb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            56.7694210357472 * x.[8] * x.[43] * x.[190] // c + cb + bAB | catalytic ligation: c + cb + bAB <-> ccb + bAB
            -60.8364303415013 * x.[259] * x.[244] // ccb + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            60.8364303415013 * x.[44] * x.[7] * x.[244] // cc + b + caB | catalytic ligation: cc + b + caB <-> ccb + caB
            -2372.62078331855 * x.[259] * x.[121] // ccb + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            2372.62078331855 * x.[44] * x.[7] * x.[121] // cc + b + CAb | catalytic ligation: cc + b + CAb <-> ccb + CAb
            -1.0 * x.[259] // ccb | ligation: cc + b <-> ccb
            1.0 * x.[44] * x.[7] // cc + b | ligation: cc + b <-> ccb
            -1.0 * x.[259] // ccb | ligation: c + cb <-> ccb
            1.0 * x.[8] * x.[43] // c + cb | ligation: c + cb <-> ccb
        |]
        |> Array.sum


    // 260 - ccc
    let d260 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            2214.00742039413 * x.[44] * x.[8] * x.[103] // cc + c + Bab | catalytic ligation: cc + c + Bab <-> ccc + Bab
            -56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            56.7694210357472 * x.[44] * x.[8] * x.[190] // cc + c + bAB | catalytic ligation: cc + c + bAB <-> ccc + bAB
            -2214.00742039413 * x.[260] * x.[103] // ccc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            2214.00742039413 * x.[8] * x.[44] * x.[103] // c + cc + Bab | catalytic ligation: c + cc + Bab <-> ccc + Bab
            -56.7694210357472 * x.[260] * x.[190] // ccc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            56.7694210357472 * x.[8] * x.[44] * x.[190] // c + cc + bAB | catalytic ligation: c + cc + bAB <-> ccc + bAB
            -1.0 * x.[260] // ccc | ligation: cc + c <-> ccc
            1.0 * x.[44] * x.[8] // cc + c | ligation: cc + c <-> ccc
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
                        regularParams = 
                            {
                                modelDataParams = 
                                    {
                                        modelInfo =
                                            {
                                                fileStructureVersion = 4.0000m
                                                versionNumber = "5.0.3"
                                                modelDataId = ("8896a004-7887-4225-a986-363183dd25fd" |> Guid |> ModelDataId)
                                                numberOfSubstances = 261
                                                numberOfAminoAcids = NumberOfAminoAcids.ThreeAminoAcids
                                                maxPeptideLength = MaxPeptideLength.ThreeMax
                                                seedValue = 63566386
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
                                                                    rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0002; scale = Some 2000.0; shift = None } } |> Distribution |> RateMultDistr
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
                                                                            rateMultiplierDistr = { distributionType = Triangular; distributionParams = { threshold = Some 0.0002; scale = Some 2000.0; shift = None } } |> Distribution |> RateMultDistr
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
                                                (SynthesisName, 6L)
                                                (DestructionName, 6L)
                                                (CatalyticSynthesisName, 1296L)
                                                (CatalyticDestructionName, 1296L)
                                                (LigationName, 234L)
                                                (CatalyticLigationName, 50544L)
                                                (SedimentationDirectName, 66564L)
                                                (SedimentationAllName, 6L)
                                                (RacemizationName, 6L)
                                                (CatalyticRacemizationName, 1296L)
                                            ]
                                        allReactions =
                                            [
                                                (LigationName, 468L)
                                                (CatalyticLigationName, 240L)
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


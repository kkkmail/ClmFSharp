namespace Clm.Model

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 2014903012
    let numberOfAminoAcids = NumberOfAminoAcids.FourAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 587

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
            x.[6] // D
            x.[7] // a
            x.[8] // b
            x.[9] // c
            x.[10] // d
            2.0 * x.[11] // AA
            2.0 * x.[12] // AB
            2.0 * x.[13] // AC
            2.0 * x.[14] // AD
            2.0 * x.[15] // Aa
            2.0 * x.[16] // Ab
            2.0 * x.[17] // Ac
            2.0 * x.[18] // Ad
            2.0 * x.[19] // BA
            2.0 * x.[20] // BB
            2.0 * x.[21] // BC
            2.0 * x.[22] // BD
            2.0 * x.[23] // Ba
            2.0 * x.[24] // Bb
            2.0 * x.[25] // Bc
            2.0 * x.[26] // Bd
            2.0 * x.[27] // CA
            2.0 * x.[28] // CB
            2.0 * x.[29] // CC
            2.0 * x.[30] // CD
            2.0 * x.[31] // Ca
            2.0 * x.[32] // Cb
            2.0 * x.[33] // Cc
            2.0 * x.[34] // Cd
            2.0 * x.[35] // DA
            2.0 * x.[36] // DB
            2.0 * x.[37] // DC
            2.0 * x.[38] // DD
            2.0 * x.[39] // Da
            2.0 * x.[40] // Db
            2.0 * x.[41] // Dc
            2.0 * x.[42] // Dd
            2.0 * x.[43] // aA
            2.0 * x.[44] // aB
            2.0 * x.[45] // aC
            2.0 * x.[46] // aD
            2.0 * x.[47] // aa
            2.0 * x.[48] // ab
            2.0 * x.[49] // ac
            2.0 * x.[50] // ad
            2.0 * x.[51] // bA
            2.0 * x.[52] // bB
            2.0 * x.[53] // bC
            2.0 * x.[54] // bD
            2.0 * x.[55] // ba
            2.0 * x.[56] // bb
            2.0 * x.[57] // bc
            2.0 * x.[58] // bd
            2.0 * x.[59] // cA
            2.0 * x.[60] // cB
            2.0 * x.[61] // cC
            2.0 * x.[62] // cD
            2.0 * x.[63] // ca
            2.0 * x.[64] // cb
            2.0 * x.[65] // cc
            2.0 * x.[66] // cd
            2.0 * x.[67] // dA
            2.0 * x.[68] // dB
            2.0 * x.[69] // dC
            2.0 * x.[70] // dD
            2.0 * x.[71] // da
            2.0 * x.[72] // db
            2.0 * x.[73] // dc
            2.0 * x.[74] // dd
            3.0 * x.[75] // AAA
            3.0 * x.[76] // AAB
            3.0 * x.[77] // AAC
            3.0 * x.[78] // AAD
            3.0 * x.[79] // AAa
            3.0 * x.[80] // AAb
            3.0 * x.[81] // AAc
            3.0 * x.[82] // AAd
            3.0 * x.[83] // ABA
            3.0 * x.[84] // ABB
            3.0 * x.[85] // ABC
            3.0 * x.[86] // ABD
            3.0 * x.[87] // ABa
            3.0 * x.[88] // ABb
            3.0 * x.[89] // ABc
            3.0 * x.[90] // ABd
            3.0 * x.[91] // ACA
            3.0 * x.[92] // ACB
            3.0 * x.[93] // ACC
            3.0 * x.[94] // ACD
            3.0 * x.[95] // ACa
            3.0 * x.[96] // ACb
            3.0 * x.[97] // ACc
            3.0 * x.[98] // ACd
            3.0 * x.[99] // ADA
            3.0 * x.[100] // ADB
            3.0 * x.[101] // ADC
            3.0 * x.[102] // ADD
            3.0 * x.[103] // ADa
            3.0 * x.[104] // ADb
            3.0 * x.[105] // ADc
            3.0 * x.[106] // ADd
            3.0 * x.[107] // AaA
            3.0 * x.[108] // AaB
            3.0 * x.[109] // AaC
            3.0 * x.[110] // AaD
            3.0 * x.[111] // Aaa
            3.0 * x.[112] // Aab
            3.0 * x.[113] // Aac
            3.0 * x.[114] // Aad
            3.0 * x.[115] // AbA
            3.0 * x.[116] // AbB
            3.0 * x.[117] // AbC
            3.0 * x.[118] // AbD
            3.0 * x.[119] // Aba
            3.0 * x.[120] // Abb
            3.0 * x.[121] // Abc
            3.0 * x.[122] // Abd
            3.0 * x.[123] // AcA
            3.0 * x.[124] // AcB
            3.0 * x.[125] // AcC
            3.0 * x.[126] // AcD
            3.0 * x.[127] // Aca
            3.0 * x.[128] // Acb
            3.0 * x.[129] // Acc
            3.0 * x.[130] // Acd
            3.0 * x.[131] // AdA
            3.0 * x.[132] // AdB
            3.0 * x.[133] // AdC
            3.0 * x.[134] // AdD
            3.0 * x.[135] // Ada
            3.0 * x.[136] // Adb
            3.0 * x.[137] // Adc
            3.0 * x.[138] // Add
            3.0 * x.[139] // BAA
            3.0 * x.[140] // BAB
            3.0 * x.[141] // BAC
            3.0 * x.[142] // BAD
            3.0 * x.[143] // BAa
            3.0 * x.[144] // BAb
            3.0 * x.[145] // BAc
            3.0 * x.[146] // BAd
            3.0 * x.[147] // BBA
            3.0 * x.[148] // BBB
            3.0 * x.[149] // BBC
            3.0 * x.[150] // BBD
            3.0 * x.[151] // BBa
            3.0 * x.[152] // BBb
            3.0 * x.[153] // BBc
            3.0 * x.[154] // BBd
            3.0 * x.[155] // BCA
            3.0 * x.[156] // BCB
            3.0 * x.[157] // BCC
            3.0 * x.[158] // BCD
            3.0 * x.[159] // BCa
            3.0 * x.[160] // BCb
            3.0 * x.[161] // BCc
            3.0 * x.[162] // BCd
            3.0 * x.[163] // BDA
            3.0 * x.[164] // BDB
            3.0 * x.[165] // BDC
            3.0 * x.[166] // BDD
            3.0 * x.[167] // BDa
            3.0 * x.[168] // BDb
            3.0 * x.[169] // BDc
            3.0 * x.[170] // BDd
            3.0 * x.[171] // BaA
            3.0 * x.[172] // BaB
            3.0 * x.[173] // BaC
            3.0 * x.[174] // BaD
            3.0 * x.[175] // Baa
            3.0 * x.[176] // Bab
            3.0 * x.[177] // Bac
            3.0 * x.[178] // Bad
            3.0 * x.[179] // BbA
            3.0 * x.[180] // BbB
            3.0 * x.[181] // BbC
            3.0 * x.[182] // BbD
            3.0 * x.[183] // Bba
            3.0 * x.[184] // Bbb
            3.0 * x.[185] // Bbc
            3.0 * x.[186] // Bbd
            3.0 * x.[187] // BcA
            3.0 * x.[188] // BcB
            3.0 * x.[189] // BcC
            3.0 * x.[190] // BcD
            3.0 * x.[191] // Bca
            3.0 * x.[192] // Bcb
            3.0 * x.[193] // Bcc
            3.0 * x.[194] // Bcd
            3.0 * x.[195] // BdA
            3.0 * x.[196] // BdB
            3.0 * x.[197] // BdC
            3.0 * x.[198] // BdD
            3.0 * x.[199] // Bda
            3.0 * x.[200] // Bdb
            3.0 * x.[201] // Bdc
            3.0 * x.[202] // Bdd
            3.0 * x.[203] // CAA
            3.0 * x.[204] // CAB
            3.0 * x.[205] // CAC
            3.0 * x.[206] // CAD
            3.0 * x.[207] // CAa
            3.0 * x.[208] // CAb
            3.0 * x.[209] // CAc
            3.0 * x.[210] // CAd
            3.0 * x.[211] // CBA
            3.0 * x.[212] // CBB
            3.0 * x.[213] // CBC
            3.0 * x.[214] // CBD
            3.0 * x.[215] // CBa
            3.0 * x.[216] // CBb
            3.0 * x.[217] // CBc
            3.0 * x.[218] // CBd
            3.0 * x.[219] // CCA
            3.0 * x.[220] // CCB
            3.0 * x.[221] // CCC
            3.0 * x.[222] // CCD
            3.0 * x.[223] // CCa
            3.0 * x.[224] // CCb
            3.0 * x.[225] // CCc
            3.0 * x.[226] // CCd
            3.0 * x.[227] // CDA
            3.0 * x.[228] // CDB
            3.0 * x.[229] // CDC
            3.0 * x.[230] // CDD
            3.0 * x.[231] // CDa
            3.0 * x.[232] // CDb
            3.0 * x.[233] // CDc
            3.0 * x.[234] // CDd
            3.0 * x.[235] // CaA
            3.0 * x.[236] // CaB
            3.0 * x.[237] // CaC
            3.0 * x.[238] // CaD
            3.0 * x.[239] // Caa
            3.0 * x.[240] // Cab
            3.0 * x.[241] // Cac
            3.0 * x.[242] // Cad
            3.0 * x.[243] // CbA
            3.0 * x.[244] // CbB
            3.0 * x.[245] // CbC
            3.0 * x.[246] // CbD
            3.0 * x.[247] // Cba
            3.0 * x.[248] // Cbb
            3.0 * x.[249] // Cbc
            3.0 * x.[250] // Cbd
            3.0 * x.[251] // CcA
            3.0 * x.[252] // CcB
            3.0 * x.[253] // CcC
            3.0 * x.[254] // CcD
            3.0 * x.[255] // Cca
            3.0 * x.[256] // Ccb
            3.0 * x.[257] // Ccc
            3.0 * x.[258] // Ccd
            3.0 * x.[259] // CdA
            3.0 * x.[260] // CdB
            3.0 * x.[261] // CdC
            3.0 * x.[262] // CdD
            3.0 * x.[263] // Cda
            3.0 * x.[264] // Cdb
            3.0 * x.[265] // Cdc
            3.0 * x.[266] // Cdd
            3.0 * x.[267] // DAA
            3.0 * x.[268] // DAB
            3.0 * x.[269] // DAC
            3.0 * x.[270] // DAD
            3.0 * x.[271] // DAa
            3.0 * x.[272] // DAb
            3.0 * x.[273] // DAc
            3.0 * x.[274] // DAd
            3.0 * x.[275] // DBA
            3.0 * x.[276] // DBB
            3.0 * x.[277] // DBC
            3.0 * x.[278] // DBD
            3.0 * x.[279] // DBa
            3.0 * x.[280] // DBb
            3.0 * x.[281] // DBc
            3.0 * x.[282] // DBd
            3.0 * x.[283] // DCA
            3.0 * x.[284] // DCB
            3.0 * x.[285] // DCC
            3.0 * x.[286] // DCD
            3.0 * x.[287] // DCa
            3.0 * x.[288] // DCb
            3.0 * x.[289] // DCc
            3.0 * x.[290] // DCd
            3.0 * x.[291] // DDA
            3.0 * x.[292] // DDB
            3.0 * x.[293] // DDC
            3.0 * x.[294] // DDD
            3.0 * x.[295] // DDa
            3.0 * x.[296] // DDb
            3.0 * x.[297] // DDc
            3.0 * x.[298] // DDd
            3.0 * x.[299] // DaA
            3.0 * x.[300] // DaB
            3.0 * x.[301] // DaC
            3.0 * x.[302] // DaD
            3.0 * x.[303] // Daa
            3.0 * x.[304] // Dab
            3.0 * x.[305] // Dac
            3.0 * x.[306] // Dad
            3.0 * x.[307] // DbA
            3.0 * x.[308] // DbB
            3.0 * x.[309] // DbC
            3.0 * x.[310] // DbD
            3.0 * x.[311] // Dba
            3.0 * x.[312] // Dbb
            3.0 * x.[313] // Dbc
            3.0 * x.[314] // Dbd
            3.0 * x.[315] // DcA
            3.0 * x.[316] // DcB
            3.0 * x.[317] // DcC
            3.0 * x.[318] // DcD
            3.0 * x.[319] // Dca
            3.0 * x.[320] // Dcb
            3.0 * x.[321] // Dcc
            3.0 * x.[322] // Dcd
            3.0 * x.[323] // DdA
            3.0 * x.[324] // DdB
            3.0 * x.[325] // DdC
            3.0 * x.[326] // DdD
            3.0 * x.[327] // Dda
            3.0 * x.[328] // Ddb
            3.0 * x.[329] // Ddc
            3.0 * x.[330] // Ddd
            3.0 * x.[331] // aAA
            3.0 * x.[332] // aAB
            3.0 * x.[333] // aAC
            3.0 * x.[334] // aAD
            3.0 * x.[335] // aAa
            3.0 * x.[336] // aAb
            3.0 * x.[337] // aAc
            3.0 * x.[338] // aAd
            3.0 * x.[339] // aBA
            3.0 * x.[340] // aBB
            3.0 * x.[341] // aBC
            3.0 * x.[342] // aBD
            3.0 * x.[343] // aBa
            3.0 * x.[344] // aBb
            3.0 * x.[345] // aBc
            3.0 * x.[346] // aBd
            3.0 * x.[347] // aCA
            3.0 * x.[348] // aCB
            3.0 * x.[349] // aCC
            3.0 * x.[350] // aCD
            3.0 * x.[351] // aCa
            3.0 * x.[352] // aCb
            3.0 * x.[353] // aCc
            3.0 * x.[354] // aCd
            3.0 * x.[355] // aDA
            3.0 * x.[356] // aDB
            3.0 * x.[357] // aDC
            3.0 * x.[358] // aDD
            3.0 * x.[359] // aDa
            3.0 * x.[360] // aDb
            3.0 * x.[361] // aDc
            3.0 * x.[362] // aDd
            3.0 * x.[363] // aaA
            3.0 * x.[364] // aaB
            3.0 * x.[365] // aaC
            3.0 * x.[366] // aaD
            3.0 * x.[367] // aaa
            3.0 * x.[368] // aab
            3.0 * x.[369] // aac
            3.0 * x.[370] // aad
            3.0 * x.[371] // abA
            3.0 * x.[372] // abB
            3.0 * x.[373] // abC
            3.0 * x.[374] // abD
            3.0 * x.[375] // aba
            3.0 * x.[376] // abb
            3.0 * x.[377] // abc
            3.0 * x.[378] // abd
            3.0 * x.[379] // acA
            3.0 * x.[380] // acB
            3.0 * x.[381] // acC
            3.0 * x.[382] // acD
            3.0 * x.[383] // aca
            3.0 * x.[384] // acb
            3.0 * x.[385] // acc
            3.0 * x.[386] // acd
            3.0 * x.[387] // adA
            3.0 * x.[388] // adB
            3.0 * x.[389] // adC
            3.0 * x.[390] // adD
            3.0 * x.[391] // ada
            3.0 * x.[392] // adb
            3.0 * x.[393] // adc
            3.0 * x.[394] // add
            3.0 * x.[395] // bAA
            3.0 * x.[396] // bAB
            3.0 * x.[397] // bAC
            3.0 * x.[398] // bAD
            3.0 * x.[399] // bAa
            3.0 * x.[400] // bAb
            3.0 * x.[401] // bAc
            3.0 * x.[402] // bAd
            3.0 * x.[403] // bBA
            3.0 * x.[404] // bBB
            3.0 * x.[405] // bBC
            3.0 * x.[406] // bBD
            3.0 * x.[407] // bBa
            3.0 * x.[408] // bBb
            3.0 * x.[409] // bBc
            3.0 * x.[410] // bBd
            3.0 * x.[411] // bCA
            3.0 * x.[412] // bCB
            3.0 * x.[413] // bCC
            3.0 * x.[414] // bCD
            3.0 * x.[415] // bCa
            3.0 * x.[416] // bCb
            3.0 * x.[417] // bCc
            3.0 * x.[418] // bCd
            3.0 * x.[419] // bDA
            3.0 * x.[420] // bDB
            3.0 * x.[421] // bDC
            3.0 * x.[422] // bDD
            3.0 * x.[423] // bDa
            3.0 * x.[424] // bDb
            3.0 * x.[425] // bDc
            3.0 * x.[426] // bDd
            3.0 * x.[427] // baA
            3.0 * x.[428] // baB
            3.0 * x.[429] // baC
            3.0 * x.[430] // baD
            3.0 * x.[431] // baa
            3.0 * x.[432] // bab
            3.0 * x.[433] // bac
            3.0 * x.[434] // bad
            3.0 * x.[435] // bbA
            3.0 * x.[436] // bbB
            3.0 * x.[437] // bbC
            3.0 * x.[438] // bbD
            3.0 * x.[439] // bba
            3.0 * x.[440] // bbb
            3.0 * x.[441] // bbc
            3.0 * x.[442] // bbd
            3.0 * x.[443] // bcA
            3.0 * x.[444] // bcB
            3.0 * x.[445] // bcC
            3.0 * x.[446] // bcD
            3.0 * x.[447] // bca
            3.0 * x.[448] // bcb
            3.0 * x.[449] // bcc
            3.0 * x.[450] // bcd
            3.0 * x.[451] // bdA
            3.0 * x.[452] // bdB
            3.0 * x.[453] // bdC
            3.0 * x.[454] // bdD
            3.0 * x.[455] // bda
            3.0 * x.[456] // bdb
            3.0 * x.[457] // bdc
            3.0 * x.[458] // bdd
            3.0 * x.[459] // cAA
            3.0 * x.[460] // cAB
            3.0 * x.[461] // cAC
            3.0 * x.[462] // cAD
            3.0 * x.[463] // cAa
            3.0 * x.[464] // cAb
            3.0 * x.[465] // cAc
            3.0 * x.[466] // cAd
            3.0 * x.[467] // cBA
            3.0 * x.[468] // cBB
            3.0 * x.[469] // cBC
            3.0 * x.[470] // cBD
            3.0 * x.[471] // cBa
            3.0 * x.[472] // cBb
            3.0 * x.[473] // cBc
            3.0 * x.[474] // cBd
            3.0 * x.[475] // cCA
            3.0 * x.[476] // cCB
            3.0 * x.[477] // cCC
            3.0 * x.[478] // cCD
            3.0 * x.[479] // cCa
            3.0 * x.[480] // cCb
            3.0 * x.[481] // cCc
            3.0 * x.[482] // cCd
            3.0 * x.[483] // cDA
            3.0 * x.[484] // cDB
            3.0 * x.[485] // cDC
            3.0 * x.[486] // cDD
            3.0 * x.[487] // cDa
            3.0 * x.[488] // cDb
            3.0 * x.[489] // cDc
            3.0 * x.[490] // cDd
            3.0 * x.[491] // caA
            3.0 * x.[492] // caB
            3.0 * x.[493] // caC
            3.0 * x.[494] // caD
            3.0 * x.[495] // caa
            3.0 * x.[496] // cab
            3.0 * x.[497] // cac
            3.0 * x.[498] // cad
            3.0 * x.[499] // cbA
            3.0 * x.[500] // cbB
            3.0 * x.[501] // cbC
            3.0 * x.[502] // cbD
            3.0 * x.[503] // cba
            3.0 * x.[504] // cbb
            3.0 * x.[505] // cbc
            3.0 * x.[506] // cbd
            3.0 * x.[507] // ccA
            3.0 * x.[508] // ccB
            3.0 * x.[509] // ccC
            3.0 * x.[510] // ccD
            3.0 * x.[511] // cca
            3.0 * x.[512] // ccb
            3.0 * x.[513] // ccc
            3.0 * x.[514] // ccd
            3.0 * x.[515] // cdA
            3.0 * x.[516] // cdB
            3.0 * x.[517] // cdC
            3.0 * x.[518] // cdD
            3.0 * x.[519] // cda
            3.0 * x.[520] // cdb
            3.0 * x.[521] // cdc
            3.0 * x.[522] // cdd
            3.0 * x.[523] // dAA
            3.0 * x.[524] // dAB
            3.0 * x.[525] // dAC
            3.0 * x.[526] // dAD
            3.0 * x.[527] // dAa
            3.0 * x.[528] // dAb
            3.0 * x.[529] // dAc
            3.0 * x.[530] // dAd
            3.0 * x.[531] // dBA
            3.0 * x.[532] // dBB
            3.0 * x.[533] // dBC
            3.0 * x.[534] // dBD
            3.0 * x.[535] // dBa
            3.0 * x.[536] // dBb
            3.0 * x.[537] // dBc
            3.0 * x.[538] // dBd
            3.0 * x.[539] // dCA
            3.0 * x.[540] // dCB
            3.0 * x.[541] // dCC
            3.0 * x.[542] // dCD
            3.0 * x.[543] // dCa
            3.0 * x.[544] // dCb
            3.0 * x.[545] // dCc
            3.0 * x.[546] // dCd
            3.0 * x.[547] // dDA
            3.0 * x.[548] // dDB
            3.0 * x.[549] // dDC
            3.0 * x.[550] // dDD
            3.0 * x.[551] // dDa
            3.0 * x.[552] // dDb
            3.0 * x.[553] // dDc
            3.0 * x.[554] // dDd
            3.0 * x.[555] // daA
            3.0 * x.[556] // daB
            3.0 * x.[557] // daC
            3.0 * x.[558] // daD
            3.0 * x.[559] // daa
            3.0 * x.[560] // dab
            3.0 * x.[561] // dac
            3.0 * x.[562] // dad
            3.0 * x.[563] // dbA
            3.0 * x.[564] // dbB
            3.0 * x.[565] // dbC
            3.0 * x.[566] // dbD
            3.0 * x.[567] // dba
            3.0 * x.[568] // dbb
            3.0 * x.[569] // dbc
            3.0 * x.[570] // dbd
            3.0 * x.[571] // dcA
            3.0 * x.[572] // dcB
            3.0 * x.[573] // dcC
            3.0 * x.[574] // dcD
            3.0 * x.[575] // dca
            3.0 * x.[576] // dcb
            3.0 * x.[577] // dcc
            3.0 * x.[578] // dcd
            3.0 * x.[579] // ddA
            3.0 * x.[580] // ddB
            3.0 * x.[581] // ddC
            3.0 * x.[582] // ddD
            3.0 * x.[583] // dda
            3.0 * x.[584] // ddb
            3.0 * x.[585] // ddc
            3.0 * x.[586] // ddd
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[3] // A
                    2.0 * x.[11] // AA
                    x.[12] // AB
                    x.[13] // AC
                    x.[14] // AD
                    x.[15] // Aa
                    x.[16] // Ab
                    x.[17] // Ac
                    x.[18] // Ad
                    x.[19] // BA
                    x.[27] // CA
                    x.[35] // DA
                    x.[43] // aA
                    x.[51] // bA
                    x.[59] // cA
                    x.[67] // dA
                    3.0 * x.[75] // AAA
                    2.0 * x.[76] // AAB
                    2.0 * x.[77] // AAC
                    2.0 * x.[78] // AAD
                    2.0 * x.[79] // AAa
                    2.0 * x.[80] // AAb
                    2.0 * x.[81] // AAc
                    2.0 * x.[82] // AAd
                    2.0 * x.[83] // ABA
                    x.[84] // ABB
                    x.[85] // ABC
                    x.[86] // ABD
                    x.[87] // ABa
                    x.[88] // ABb
                    x.[89] // ABc
                    x.[90] // ABd
                    2.0 * x.[91] // ACA
                    x.[92] // ACB
                    x.[93] // ACC
                    x.[94] // ACD
                    x.[95] // ACa
                    x.[96] // ACb
                    x.[97] // ACc
                    x.[98] // ACd
                    2.0 * x.[99] // ADA
                    x.[100] // ADB
                    x.[101] // ADC
                    x.[102] // ADD
                    x.[103] // ADa
                    x.[104] // ADb
                    x.[105] // ADc
                    x.[106] // ADd
                    2.0 * x.[107] // AaA
                    x.[108] // AaB
                    x.[109] // AaC
                    x.[110] // AaD
                    x.[111] // Aaa
                    x.[112] // Aab
                    x.[113] // Aac
                    x.[114] // Aad
                    2.0 * x.[115] // AbA
                    x.[116] // AbB
                    x.[117] // AbC
                    x.[118] // AbD
                    x.[119] // Aba
                    x.[120] // Abb
                    x.[121] // Abc
                    x.[122] // Abd
                    2.0 * x.[123] // AcA
                    x.[124] // AcB
                    x.[125] // AcC
                    x.[126] // AcD
                    x.[127] // Aca
                    x.[128] // Acb
                    x.[129] // Acc
                    x.[130] // Acd
                    2.0 * x.[131] // AdA
                    x.[132] // AdB
                    x.[133] // AdC
                    x.[134] // AdD
                    x.[135] // Ada
                    x.[136] // Adb
                    x.[137] // Adc
                    x.[138] // Add
                    2.0 * x.[139] // BAA
                    x.[140] // BAB
                    x.[141] // BAC
                    x.[142] // BAD
                    x.[143] // BAa
                    x.[144] // BAb
                    x.[145] // BAc
                    x.[146] // BAd
                    x.[147] // BBA
                    x.[155] // BCA
                    x.[163] // BDA
                    x.[171] // BaA
                    x.[179] // BbA
                    x.[187] // BcA
                    x.[195] // BdA
                    2.0 * x.[203] // CAA
                    x.[204] // CAB
                    x.[205] // CAC
                    x.[206] // CAD
                    x.[207] // CAa
                    x.[208] // CAb
                    x.[209] // CAc
                    x.[210] // CAd
                    x.[211] // CBA
                    x.[219] // CCA
                    x.[227] // CDA
                    x.[235] // CaA
                    x.[243] // CbA
                    x.[251] // CcA
                    x.[259] // CdA
                    2.0 * x.[267] // DAA
                    x.[268] // DAB
                    x.[269] // DAC
                    x.[270] // DAD
                    x.[271] // DAa
                    x.[272] // DAb
                    x.[273] // DAc
                    x.[274] // DAd
                    x.[275] // DBA
                    x.[283] // DCA
                    x.[291] // DDA
                    x.[299] // DaA
                    x.[307] // DbA
                    x.[315] // DcA
                    x.[323] // DdA
                    2.0 * x.[331] // aAA
                    x.[332] // aAB
                    x.[333] // aAC
                    x.[334] // aAD
                    x.[335] // aAa
                    x.[336] // aAb
                    x.[337] // aAc
                    x.[338] // aAd
                    x.[339] // aBA
                    x.[347] // aCA
                    x.[355] // aDA
                    x.[363] // aaA
                    x.[371] // abA
                    x.[379] // acA
                    x.[387] // adA
                    2.0 * x.[395] // bAA
                    x.[396] // bAB
                    x.[397] // bAC
                    x.[398] // bAD
                    x.[399] // bAa
                    x.[400] // bAb
                    x.[401] // bAc
                    x.[402] // bAd
                    x.[403] // bBA
                    x.[411] // bCA
                    x.[419] // bDA
                    x.[427] // baA
                    x.[435] // bbA
                    x.[443] // bcA
                    x.[451] // bdA
                    2.0 * x.[459] // cAA
                    x.[460] // cAB
                    x.[461] // cAC
                    x.[462] // cAD
                    x.[463] // cAa
                    x.[464] // cAb
                    x.[465] // cAc
                    x.[466] // cAd
                    x.[467] // cBA
                    x.[475] // cCA
                    x.[483] // cDA
                    x.[491] // caA
                    x.[499] // cbA
                    x.[507] // ccA
                    x.[515] // cdA
                    2.0 * x.[523] // dAA
                    x.[524] // dAB
                    x.[525] // dAC
                    x.[526] // dAD
                    x.[527] // dAa
                    x.[528] // dAb
                    x.[529] // dAc
                    x.[530] // dAd
                    x.[531] // dBA
                    x.[539] // dCA
                    x.[547] // dDA
                    x.[555] // daA
                    x.[563] // dbA
                    x.[571] // dcA
                    x.[579] // ddA
                |]
                |> Array.sum
                ,
                [|
                    x.[7] // a
                    x.[15] // Aa
                    x.[23] // Ba
                    x.[31] // Ca
                    x.[39] // Da
                    x.[43] // aA
                    x.[44] // aB
                    x.[45] // aC
                    x.[46] // aD
                    2.0 * x.[47] // aa
                    x.[48] // ab
                    x.[49] // ac
                    x.[50] // ad
                    x.[55] // ba
                    x.[63] // ca
                    x.[71] // da
                    x.[79] // AAa
                    x.[87] // ABa
                    x.[95] // ACa
                    x.[103] // ADa
                    x.[107] // AaA
                    x.[108] // AaB
                    x.[109] // AaC
                    x.[110] // AaD
                    2.0 * x.[111] // Aaa
                    x.[112] // Aab
                    x.[113] // Aac
                    x.[114] // Aad
                    x.[119] // Aba
                    x.[127] // Aca
                    x.[135] // Ada
                    x.[143] // BAa
                    x.[151] // BBa
                    x.[159] // BCa
                    x.[167] // BDa
                    x.[171] // BaA
                    x.[172] // BaB
                    x.[173] // BaC
                    x.[174] // BaD
                    2.0 * x.[175] // Baa
                    x.[176] // Bab
                    x.[177] // Bac
                    x.[178] // Bad
                    x.[183] // Bba
                    x.[191] // Bca
                    x.[199] // Bda
                    x.[207] // CAa
                    x.[215] // CBa
                    x.[223] // CCa
                    x.[231] // CDa
                    x.[235] // CaA
                    x.[236] // CaB
                    x.[237] // CaC
                    x.[238] // CaD
                    2.0 * x.[239] // Caa
                    x.[240] // Cab
                    x.[241] // Cac
                    x.[242] // Cad
                    x.[247] // Cba
                    x.[255] // Cca
                    x.[263] // Cda
                    x.[271] // DAa
                    x.[279] // DBa
                    x.[287] // DCa
                    x.[295] // DDa
                    x.[299] // DaA
                    x.[300] // DaB
                    x.[301] // DaC
                    x.[302] // DaD
                    2.0 * x.[303] // Daa
                    x.[304] // Dab
                    x.[305] // Dac
                    x.[306] // Dad
                    x.[311] // Dba
                    x.[319] // Dca
                    x.[327] // Dda
                    x.[331] // aAA
                    x.[332] // aAB
                    x.[333] // aAC
                    x.[334] // aAD
                    2.0 * x.[335] // aAa
                    x.[336] // aAb
                    x.[337] // aAc
                    x.[338] // aAd
                    x.[339] // aBA
                    x.[340] // aBB
                    x.[341] // aBC
                    x.[342] // aBD
                    2.0 * x.[343] // aBa
                    x.[344] // aBb
                    x.[345] // aBc
                    x.[346] // aBd
                    x.[347] // aCA
                    x.[348] // aCB
                    x.[349] // aCC
                    x.[350] // aCD
                    2.0 * x.[351] // aCa
                    x.[352] // aCb
                    x.[353] // aCc
                    x.[354] // aCd
                    x.[355] // aDA
                    x.[356] // aDB
                    x.[357] // aDC
                    x.[358] // aDD
                    2.0 * x.[359] // aDa
                    x.[360] // aDb
                    x.[361] // aDc
                    x.[362] // aDd
                    2.0 * x.[363] // aaA
                    2.0 * x.[364] // aaB
                    2.0 * x.[365] // aaC
                    2.0 * x.[366] // aaD
                    3.0 * x.[367] // aaa
                    2.0 * x.[368] // aab
                    2.0 * x.[369] // aac
                    2.0 * x.[370] // aad
                    x.[371] // abA
                    x.[372] // abB
                    x.[373] // abC
                    x.[374] // abD
                    2.0 * x.[375] // aba
                    x.[376] // abb
                    x.[377] // abc
                    x.[378] // abd
                    x.[379] // acA
                    x.[380] // acB
                    x.[381] // acC
                    x.[382] // acD
                    2.0 * x.[383] // aca
                    x.[384] // acb
                    x.[385] // acc
                    x.[386] // acd
                    x.[387] // adA
                    x.[388] // adB
                    x.[389] // adC
                    x.[390] // adD
                    2.0 * x.[391] // ada
                    x.[392] // adb
                    x.[393] // adc
                    x.[394] // add
                    x.[399] // bAa
                    x.[407] // bBa
                    x.[415] // bCa
                    x.[423] // bDa
                    x.[427] // baA
                    x.[428] // baB
                    x.[429] // baC
                    x.[430] // baD
                    2.0 * x.[431] // baa
                    x.[432] // bab
                    x.[433] // bac
                    x.[434] // bad
                    x.[439] // bba
                    x.[447] // bca
                    x.[455] // bda
                    x.[463] // cAa
                    x.[471] // cBa
                    x.[479] // cCa
                    x.[487] // cDa
                    x.[491] // caA
                    x.[492] // caB
                    x.[493] // caC
                    x.[494] // caD
                    2.0 * x.[495] // caa
                    x.[496] // cab
                    x.[497] // cac
                    x.[498] // cad
                    x.[503] // cba
                    x.[511] // cca
                    x.[519] // cda
                    x.[527] // dAa
                    x.[535] // dBa
                    x.[543] // dCa
                    x.[551] // dDa
                    x.[555] // daA
                    x.[556] // daB
                    x.[557] // daC
                    x.[558] // daD
                    2.0 * x.[559] // daa
                    x.[560] // dab
                    x.[561] // dac
                    x.[562] // dad
                    x.[567] // dba
                    x.[575] // dca
                    x.[583] // dda
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[4] // B
                    x.[12] // AB
                    x.[19] // BA
                    2.0 * x.[20] // BB
                    x.[21] // BC
                    x.[22] // BD
                    x.[23] // Ba
                    x.[24] // Bb
                    x.[25] // Bc
                    x.[26] // Bd
                    x.[28] // CB
                    x.[36] // DB
                    x.[44] // aB
                    x.[52] // bB
                    x.[60] // cB
                    x.[68] // dB
                    x.[76] // AAB
                    x.[83] // ABA
                    2.0 * x.[84] // ABB
                    x.[85] // ABC
                    x.[86] // ABD
                    x.[87] // ABa
                    x.[88] // ABb
                    x.[89] // ABc
                    x.[90] // ABd
                    x.[92] // ACB
                    x.[100] // ADB
                    x.[108] // AaB
                    x.[116] // AbB
                    x.[124] // AcB
                    x.[132] // AdB
                    x.[139] // BAA
                    2.0 * x.[140] // BAB
                    x.[141] // BAC
                    x.[142] // BAD
                    x.[143] // BAa
                    x.[144] // BAb
                    x.[145] // BAc
                    x.[146] // BAd
                    2.0 * x.[147] // BBA
                    3.0 * x.[148] // BBB
                    2.0 * x.[149] // BBC
                    2.0 * x.[150] // BBD
                    2.0 * x.[151] // BBa
                    2.0 * x.[152] // BBb
                    2.0 * x.[153] // BBc
                    2.0 * x.[154] // BBd
                    x.[155] // BCA
                    2.0 * x.[156] // BCB
                    x.[157] // BCC
                    x.[158] // BCD
                    x.[159] // BCa
                    x.[160] // BCb
                    x.[161] // BCc
                    x.[162] // BCd
                    x.[163] // BDA
                    2.0 * x.[164] // BDB
                    x.[165] // BDC
                    x.[166] // BDD
                    x.[167] // BDa
                    x.[168] // BDb
                    x.[169] // BDc
                    x.[170] // BDd
                    x.[171] // BaA
                    2.0 * x.[172] // BaB
                    x.[173] // BaC
                    x.[174] // BaD
                    x.[175] // Baa
                    x.[176] // Bab
                    x.[177] // Bac
                    x.[178] // Bad
                    x.[179] // BbA
                    2.0 * x.[180] // BbB
                    x.[181] // BbC
                    x.[182] // BbD
                    x.[183] // Bba
                    x.[184] // Bbb
                    x.[185] // Bbc
                    x.[186] // Bbd
                    x.[187] // BcA
                    2.0 * x.[188] // BcB
                    x.[189] // BcC
                    x.[190] // BcD
                    x.[191] // Bca
                    x.[192] // Bcb
                    x.[193] // Bcc
                    x.[194] // Bcd
                    x.[195] // BdA
                    2.0 * x.[196] // BdB
                    x.[197] // BdC
                    x.[198] // BdD
                    x.[199] // Bda
                    x.[200] // Bdb
                    x.[201] // Bdc
                    x.[202] // Bdd
                    x.[204] // CAB
                    x.[211] // CBA
                    2.0 * x.[212] // CBB
                    x.[213] // CBC
                    x.[214] // CBD
                    x.[215] // CBa
                    x.[216] // CBb
                    x.[217] // CBc
                    x.[218] // CBd
                    x.[220] // CCB
                    x.[228] // CDB
                    x.[236] // CaB
                    x.[244] // CbB
                    x.[252] // CcB
                    x.[260] // CdB
                    x.[268] // DAB
                    x.[275] // DBA
                    2.0 * x.[276] // DBB
                    x.[277] // DBC
                    x.[278] // DBD
                    x.[279] // DBa
                    x.[280] // DBb
                    x.[281] // DBc
                    x.[282] // DBd
                    x.[284] // DCB
                    x.[292] // DDB
                    x.[300] // DaB
                    x.[308] // DbB
                    x.[316] // DcB
                    x.[324] // DdB
                    x.[332] // aAB
                    x.[339] // aBA
                    2.0 * x.[340] // aBB
                    x.[341] // aBC
                    x.[342] // aBD
                    x.[343] // aBa
                    x.[344] // aBb
                    x.[345] // aBc
                    x.[346] // aBd
                    x.[348] // aCB
                    x.[356] // aDB
                    x.[364] // aaB
                    x.[372] // abB
                    x.[380] // acB
                    x.[388] // adB
                    x.[396] // bAB
                    x.[403] // bBA
                    2.0 * x.[404] // bBB
                    x.[405] // bBC
                    x.[406] // bBD
                    x.[407] // bBa
                    x.[408] // bBb
                    x.[409] // bBc
                    x.[410] // bBd
                    x.[412] // bCB
                    x.[420] // bDB
                    x.[428] // baB
                    x.[436] // bbB
                    x.[444] // bcB
                    x.[452] // bdB
                    x.[460] // cAB
                    x.[467] // cBA
                    2.0 * x.[468] // cBB
                    x.[469] // cBC
                    x.[470] // cBD
                    x.[471] // cBa
                    x.[472] // cBb
                    x.[473] // cBc
                    x.[474] // cBd
                    x.[476] // cCB
                    x.[484] // cDB
                    x.[492] // caB
                    x.[500] // cbB
                    x.[508] // ccB
                    x.[516] // cdB
                    x.[524] // dAB
                    x.[531] // dBA
                    2.0 * x.[532] // dBB
                    x.[533] // dBC
                    x.[534] // dBD
                    x.[535] // dBa
                    x.[536] // dBb
                    x.[537] // dBc
                    x.[538] // dBd
                    x.[540] // dCB
                    x.[548] // dDB
                    x.[556] // daB
                    x.[564] // dbB
                    x.[572] // dcB
                    x.[580] // ddB
                |]
                |> Array.sum
                ,
                [|
                    x.[8] // b
                    x.[16] // Ab
                    x.[24] // Bb
                    x.[32] // Cb
                    x.[40] // Db
                    x.[48] // ab
                    x.[51] // bA
                    x.[52] // bB
                    x.[53] // bC
                    x.[54] // bD
                    x.[55] // ba
                    2.0 * x.[56] // bb
                    x.[57] // bc
                    x.[58] // bd
                    x.[64] // cb
                    x.[72] // db
                    x.[80] // AAb
                    x.[88] // ABb
                    x.[96] // ACb
                    x.[104] // ADb
                    x.[112] // Aab
                    x.[115] // AbA
                    x.[116] // AbB
                    x.[117] // AbC
                    x.[118] // AbD
                    x.[119] // Aba
                    2.0 * x.[120] // Abb
                    x.[121] // Abc
                    x.[122] // Abd
                    x.[128] // Acb
                    x.[136] // Adb
                    x.[144] // BAb
                    x.[152] // BBb
                    x.[160] // BCb
                    x.[168] // BDb
                    x.[176] // Bab
                    x.[179] // BbA
                    x.[180] // BbB
                    x.[181] // BbC
                    x.[182] // BbD
                    x.[183] // Bba
                    2.0 * x.[184] // Bbb
                    x.[185] // Bbc
                    x.[186] // Bbd
                    x.[192] // Bcb
                    x.[200] // Bdb
                    x.[208] // CAb
                    x.[216] // CBb
                    x.[224] // CCb
                    x.[232] // CDb
                    x.[240] // Cab
                    x.[243] // CbA
                    x.[244] // CbB
                    x.[245] // CbC
                    x.[246] // CbD
                    x.[247] // Cba
                    2.0 * x.[248] // Cbb
                    x.[249] // Cbc
                    x.[250] // Cbd
                    x.[256] // Ccb
                    x.[264] // Cdb
                    x.[272] // DAb
                    x.[280] // DBb
                    x.[288] // DCb
                    x.[296] // DDb
                    x.[304] // Dab
                    x.[307] // DbA
                    x.[308] // DbB
                    x.[309] // DbC
                    x.[310] // DbD
                    x.[311] // Dba
                    2.0 * x.[312] // Dbb
                    x.[313] // Dbc
                    x.[314] // Dbd
                    x.[320] // Dcb
                    x.[328] // Ddb
                    x.[336] // aAb
                    x.[344] // aBb
                    x.[352] // aCb
                    x.[360] // aDb
                    x.[368] // aab
                    x.[371] // abA
                    x.[372] // abB
                    x.[373] // abC
                    x.[374] // abD
                    x.[375] // aba
                    2.0 * x.[376] // abb
                    x.[377] // abc
                    x.[378] // abd
                    x.[384] // acb
                    x.[392] // adb
                    x.[395] // bAA
                    x.[396] // bAB
                    x.[397] // bAC
                    x.[398] // bAD
                    x.[399] // bAa
                    2.0 * x.[400] // bAb
                    x.[401] // bAc
                    x.[402] // bAd
                    x.[403] // bBA
                    x.[404] // bBB
                    x.[405] // bBC
                    x.[406] // bBD
                    x.[407] // bBa
                    2.0 * x.[408] // bBb
                    x.[409] // bBc
                    x.[410] // bBd
                    x.[411] // bCA
                    x.[412] // bCB
                    x.[413] // bCC
                    x.[414] // bCD
                    x.[415] // bCa
                    2.0 * x.[416] // bCb
                    x.[417] // bCc
                    x.[418] // bCd
                    x.[419] // bDA
                    x.[420] // bDB
                    x.[421] // bDC
                    x.[422] // bDD
                    x.[423] // bDa
                    2.0 * x.[424] // bDb
                    x.[425] // bDc
                    x.[426] // bDd
                    x.[427] // baA
                    x.[428] // baB
                    x.[429] // baC
                    x.[430] // baD
                    x.[431] // baa
                    2.0 * x.[432] // bab
                    x.[433] // bac
                    x.[434] // bad
                    2.0 * x.[435] // bbA
                    2.0 * x.[436] // bbB
                    2.0 * x.[437] // bbC
                    2.0 * x.[438] // bbD
                    2.0 * x.[439] // bba
                    3.0 * x.[440] // bbb
                    2.0 * x.[441] // bbc
                    2.0 * x.[442] // bbd
                    x.[443] // bcA
                    x.[444] // bcB
                    x.[445] // bcC
                    x.[446] // bcD
                    x.[447] // bca
                    2.0 * x.[448] // bcb
                    x.[449] // bcc
                    x.[450] // bcd
                    x.[451] // bdA
                    x.[452] // bdB
                    x.[453] // bdC
                    x.[454] // bdD
                    x.[455] // bda
                    2.0 * x.[456] // bdb
                    x.[457] // bdc
                    x.[458] // bdd
                    x.[464] // cAb
                    x.[472] // cBb
                    x.[480] // cCb
                    x.[488] // cDb
                    x.[496] // cab
                    x.[499] // cbA
                    x.[500] // cbB
                    x.[501] // cbC
                    x.[502] // cbD
                    x.[503] // cba
                    2.0 * x.[504] // cbb
                    x.[505] // cbc
                    x.[506] // cbd
                    x.[512] // ccb
                    x.[520] // cdb
                    x.[528] // dAb
                    x.[536] // dBb
                    x.[544] // dCb
                    x.[552] // dDb
                    x.[560] // dab
                    x.[563] // dbA
                    x.[564] // dbB
                    x.[565] // dbC
                    x.[566] // dbD
                    x.[567] // dba
                    2.0 * x.[568] // dbb
                    x.[569] // dbc
                    x.[570] // dbd
                    x.[576] // dcb
                    x.[584] // ddb
                |]
                |> Array.sum
            )

            // C
            (
                [|
                    x.[5] // C
                    x.[13] // AC
                    x.[21] // BC
                    x.[27] // CA
                    x.[28] // CB
                    2.0 * x.[29] // CC
                    x.[30] // CD
                    x.[31] // Ca
                    x.[32] // Cb
                    x.[33] // Cc
                    x.[34] // Cd
                    x.[37] // DC
                    x.[45] // aC
                    x.[53] // bC
                    x.[61] // cC
                    x.[69] // dC
                    x.[77] // AAC
                    x.[85] // ABC
                    x.[91] // ACA
                    x.[92] // ACB
                    2.0 * x.[93] // ACC
                    x.[94] // ACD
                    x.[95] // ACa
                    x.[96] // ACb
                    x.[97] // ACc
                    x.[98] // ACd
                    x.[101] // ADC
                    x.[109] // AaC
                    x.[117] // AbC
                    x.[125] // AcC
                    x.[133] // AdC
                    x.[141] // BAC
                    x.[149] // BBC
                    x.[155] // BCA
                    x.[156] // BCB
                    2.0 * x.[157] // BCC
                    x.[158] // BCD
                    x.[159] // BCa
                    x.[160] // BCb
                    x.[161] // BCc
                    x.[162] // BCd
                    x.[165] // BDC
                    x.[173] // BaC
                    x.[181] // BbC
                    x.[189] // BcC
                    x.[197] // BdC
                    x.[203] // CAA
                    x.[204] // CAB
                    2.0 * x.[205] // CAC
                    x.[206] // CAD
                    x.[207] // CAa
                    x.[208] // CAb
                    x.[209] // CAc
                    x.[210] // CAd
                    x.[211] // CBA
                    x.[212] // CBB
                    2.0 * x.[213] // CBC
                    x.[214] // CBD
                    x.[215] // CBa
                    x.[216] // CBb
                    x.[217] // CBc
                    x.[218] // CBd
                    2.0 * x.[219] // CCA
                    2.0 * x.[220] // CCB
                    3.0 * x.[221] // CCC
                    2.0 * x.[222] // CCD
                    2.0 * x.[223] // CCa
                    2.0 * x.[224] // CCb
                    2.0 * x.[225] // CCc
                    2.0 * x.[226] // CCd
                    x.[227] // CDA
                    x.[228] // CDB
                    2.0 * x.[229] // CDC
                    x.[230] // CDD
                    x.[231] // CDa
                    x.[232] // CDb
                    x.[233] // CDc
                    x.[234] // CDd
                    x.[235] // CaA
                    x.[236] // CaB
                    2.0 * x.[237] // CaC
                    x.[238] // CaD
                    x.[239] // Caa
                    x.[240] // Cab
                    x.[241] // Cac
                    x.[242] // Cad
                    x.[243] // CbA
                    x.[244] // CbB
                    2.0 * x.[245] // CbC
                    x.[246] // CbD
                    x.[247] // Cba
                    x.[248] // Cbb
                    x.[249] // Cbc
                    x.[250] // Cbd
                    x.[251] // CcA
                    x.[252] // CcB
                    2.0 * x.[253] // CcC
                    x.[254] // CcD
                    x.[255] // Cca
                    x.[256] // Ccb
                    x.[257] // Ccc
                    x.[258] // Ccd
                    x.[259] // CdA
                    x.[260] // CdB
                    2.0 * x.[261] // CdC
                    x.[262] // CdD
                    x.[263] // Cda
                    x.[264] // Cdb
                    x.[265] // Cdc
                    x.[266] // Cdd
                    x.[269] // DAC
                    x.[277] // DBC
                    x.[283] // DCA
                    x.[284] // DCB
                    2.0 * x.[285] // DCC
                    x.[286] // DCD
                    x.[287] // DCa
                    x.[288] // DCb
                    x.[289] // DCc
                    x.[290] // DCd
                    x.[293] // DDC
                    x.[301] // DaC
                    x.[309] // DbC
                    x.[317] // DcC
                    x.[325] // DdC
                    x.[333] // aAC
                    x.[341] // aBC
                    x.[347] // aCA
                    x.[348] // aCB
                    2.0 * x.[349] // aCC
                    x.[350] // aCD
                    x.[351] // aCa
                    x.[352] // aCb
                    x.[353] // aCc
                    x.[354] // aCd
                    x.[357] // aDC
                    x.[365] // aaC
                    x.[373] // abC
                    x.[381] // acC
                    x.[389] // adC
                    x.[397] // bAC
                    x.[405] // bBC
                    x.[411] // bCA
                    x.[412] // bCB
                    2.0 * x.[413] // bCC
                    x.[414] // bCD
                    x.[415] // bCa
                    x.[416] // bCb
                    x.[417] // bCc
                    x.[418] // bCd
                    x.[421] // bDC
                    x.[429] // baC
                    x.[437] // bbC
                    x.[445] // bcC
                    x.[453] // bdC
                    x.[461] // cAC
                    x.[469] // cBC
                    x.[475] // cCA
                    x.[476] // cCB
                    2.0 * x.[477] // cCC
                    x.[478] // cCD
                    x.[479] // cCa
                    x.[480] // cCb
                    x.[481] // cCc
                    x.[482] // cCd
                    x.[485] // cDC
                    x.[493] // caC
                    x.[501] // cbC
                    x.[509] // ccC
                    x.[517] // cdC
                    x.[525] // dAC
                    x.[533] // dBC
                    x.[539] // dCA
                    x.[540] // dCB
                    2.0 * x.[541] // dCC
                    x.[542] // dCD
                    x.[543] // dCa
                    x.[544] // dCb
                    x.[545] // dCc
                    x.[546] // dCd
                    x.[549] // dDC
                    x.[557] // daC
                    x.[565] // dbC
                    x.[573] // dcC
                    x.[581] // ddC
                |]
                |> Array.sum
                ,
                [|
                    x.[9] // c
                    x.[17] // Ac
                    x.[25] // Bc
                    x.[33] // Cc
                    x.[41] // Dc
                    x.[49] // ac
                    x.[57] // bc
                    x.[59] // cA
                    x.[60] // cB
                    x.[61] // cC
                    x.[62] // cD
                    x.[63] // ca
                    x.[64] // cb
                    2.0 * x.[65] // cc
                    x.[66] // cd
                    x.[73] // dc
                    x.[81] // AAc
                    x.[89] // ABc
                    x.[97] // ACc
                    x.[105] // ADc
                    x.[113] // Aac
                    x.[121] // Abc
                    x.[123] // AcA
                    x.[124] // AcB
                    x.[125] // AcC
                    x.[126] // AcD
                    x.[127] // Aca
                    x.[128] // Acb
                    2.0 * x.[129] // Acc
                    x.[130] // Acd
                    x.[137] // Adc
                    x.[145] // BAc
                    x.[153] // BBc
                    x.[161] // BCc
                    x.[169] // BDc
                    x.[177] // Bac
                    x.[185] // Bbc
                    x.[187] // BcA
                    x.[188] // BcB
                    x.[189] // BcC
                    x.[190] // BcD
                    x.[191] // Bca
                    x.[192] // Bcb
                    2.0 * x.[193] // Bcc
                    x.[194] // Bcd
                    x.[201] // Bdc
                    x.[209] // CAc
                    x.[217] // CBc
                    x.[225] // CCc
                    x.[233] // CDc
                    x.[241] // Cac
                    x.[249] // Cbc
                    x.[251] // CcA
                    x.[252] // CcB
                    x.[253] // CcC
                    x.[254] // CcD
                    x.[255] // Cca
                    x.[256] // Ccb
                    2.0 * x.[257] // Ccc
                    x.[258] // Ccd
                    x.[265] // Cdc
                    x.[273] // DAc
                    x.[281] // DBc
                    x.[289] // DCc
                    x.[297] // DDc
                    x.[305] // Dac
                    x.[313] // Dbc
                    x.[315] // DcA
                    x.[316] // DcB
                    x.[317] // DcC
                    x.[318] // DcD
                    x.[319] // Dca
                    x.[320] // Dcb
                    2.0 * x.[321] // Dcc
                    x.[322] // Dcd
                    x.[329] // Ddc
                    x.[337] // aAc
                    x.[345] // aBc
                    x.[353] // aCc
                    x.[361] // aDc
                    x.[369] // aac
                    x.[377] // abc
                    x.[379] // acA
                    x.[380] // acB
                    x.[381] // acC
                    x.[382] // acD
                    x.[383] // aca
                    x.[384] // acb
                    2.0 * x.[385] // acc
                    x.[386] // acd
                    x.[393] // adc
                    x.[401] // bAc
                    x.[409] // bBc
                    x.[417] // bCc
                    x.[425] // bDc
                    x.[433] // bac
                    x.[441] // bbc
                    x.[443] // bcA
                    x.[444] // bcB
                    x.[445] // bcC
                    x.[446] // bcD
                    x.[447] // bca
                    x.[448] // bcb
                    2.0 * x.[449] // bcc
                    x.[450] // bcd
                    x.[457] // bdc
                    x.[459] // cAA
                    x.[460] // cAB
                    x.[461] // cAC
                    x.[462] // cAD
                    x.[463] // cAa
                    x.[464] // cAb
                    2.0 * x.[465] // cAc
                    x.[466] // cAd
                    x.[467] // cBA
                    x.[468] // cBB
                    x.[469] // cBC
                    x.[470] // cBD
                    x.[471] // cBa
                    x.[472] // cBb
                    2.0 * x.[473] // cBc
                    x.[474] // cBd
                    x.[475] // cCA
                    x.[476] // cCB
                    x.[477] // cCC
                    x.[478] // cCD
                    x.[479] // cCa
                    x.[480] // cCb
                    2.0 * x.[481] // cCc
                    x.[482] // cCd
                    x.[483] // cDA
                    x.[484] // cDB
                    x.[485] // cDC
                    x.[486] // cDD
                    x.[487] // cDa
                    x.[488] // cDb
                    2.0 * x.[489] // cDc
                    x.[490] // cDd
                    x.[491] // caA
                    x.[492] // caB
                    x.[493] // caC
                    x.[494] // caD
                    x.[495] // caa
                    x.[496] // cab
                    2.0 * x.[497] // cac
                    x.[498] // cad
                    x.[499] // cbA
                    x.[500] // cbB
                    x.[501] // cbC
                    x.[502] // cbD
                    x.[503] // cba
                    x.[504] // cbb
                    2.0 * x.[505] // cbc
                    x.[506] // cbd
                    2.0 * x.[507] // ccA
                    2.0 * x.[508] // ccB
                    2.0 * x.[509] // ccC
                    2.0 * x.[510] // ccD
                    2.0 * x.[511] // cca
                    2.0 * x.[512] // ccb
                    3.0 * x.[513] // ccc
                    2.0 * x.[514] // ccd
                    x.[515] // cdA
                    x.[516] // cdB
                    x.[517] // cdC
                    x.[518] // cdD
                    x.[519] // cda
                    x.[520] // cdb
                    2.0 * x.[521] // cdc
                    x.[522] // cdd
                    x.[529] // dAc
                    x.[537] // dBc
                    x.[545] // dCc
                    x.[553] // dDc
                    x.[561] // dac
                    x.[569] // dbc
                    x.[571] // dcA
                    x.[572] // dcB
                    x.[573] // dcC
                    x.[574] // dcD
                    x.[575] // dca
                    x.[576] // dcb
                    2.0 * x.[577] // dcc
                    x.[578] // dcd
                    x.[585] // ddc
                |]
                |> Array.sum
            )

            // D
            (
                [|
                    x.[6] // D
                    x.[14] // AD
                    x.[22] // BD
                    x.[30] // CD
                    x.[35] // DA
                    x.[36] // DB
                    x.[37] // DC
                    2.0 * x.[38] // DD
                    x.[39] // Da
                    x.[40] // Db
                    x.[41] // Dc
                    x.[42] // Dd
                    x.[46] // aD
                    x.[54] // bD
                    x.[62] // cD
                    x.[70] // dD
                    x.[78] // AAD
                    x.[86] // ABD
                    x.[94] // ACD
                    x.[99] // ADA
                    x.[100] // ADB
                    x.[101] // ADC
                    2.0 * x.[102] // ADD
                    x.[103] // ADa
                    x.[104] // ADb
                    x.[105] // ADc
                    x.[106] // ADd
                    x.[110] // AaD
                    x.[118] // AbD
                    x.[126] // AcD
                    x.[134] // AdD
                    x.[142] // BAD
                    x.[150] // BBD
                    x.[158] // BCD
                    x.[163] // BDA
                    x.[164] // BDB
                    x.[165] // BDC
                    2.0 * x.[166] // BDD
                    x.[167] // BDa
                    x.[168] // BDb
                    x.[169] // BDc
                    x.[170] // BDd
                    x.[174] // BaD
                    x.[182] // BbD
                    x.[190] // BcD
                    x.[198] // BdD
                    x.[206] // CAD
                    x.[214] // CBD
                    x.[222] // CCD
                    x.[227] // CDA
                    x.[228] // CDB
                    x.[229] // CDC
                    2.0 * x.[230] // CDD
                    x.[231] // CDa
                    x.[232] // CDb
                    x.[233] // CDc
                    x.[234] // CDd
                    x.[238] // CaD
                    x.[246] // CbD
                    x.[254] // CcD
                    x.[262] // CdD
                    x.[267] // DAA
                    x.[268] // DAB
                    x.[269] // DAC
                    2.0 * x.[270] // DAD
                    x.[271] // DAa
                    x.[272] // DAb
                    x.[273] // DAc
                    x.[274] // DAd
                    x.[275] // DBA
                    x.[276] // DBB
                    x.[277] // DBC
                    2.0 * x.[278] // DBD
                    x.[279] // DBa
                    x.[280] // DBb
                    x.[281] // DBc
                    x.[282] // DBd
                    x.[283] // DCA
                    x.[284] // DCB
                    x.[285] // DCC
                    2.0 * x.[286] // DCD
                    x.[287] // DCa
                    x.[288] // DCb
                    x.[289] // DCc
                    x.[290] // DCd
                    2.0 * x.[291] // DDA
                    2.0 * x.[292] // DDB
                    2.0 * x.[293] // DDC
                    3.0 * x.[294] // DDD
                    2.0 * x.[295] // DDa
                    2.0 * x.[296] // DDb
                    2.0 * x.[297] // DDc
                    2.0 * x.[298] // DDd
                    x.[299] // DaA
                    x.[300] // DaB
                    x.[301] // DaC
                    2.0 * x.[302] // DaD
                    x.[303] // Daa
                    x.[304] // Dab
                    x.[305] // Dac
                    x.[306] // Dad
                    x.[307] // DbA
                    x.[308] // DbB
                    x.[309] // DbC
                    2.0 * x.[310] // DbD
                    x.[311] // Dba
                    x.[312] // Dbb
                    x.[313] // Dbc
                    x.[314] // Dbd
                    x.[315] // DcA
                    x.[316] // DcB
                    x.[317] // DcC
                    2.0 * x.[318] // DcD
                    x.[319] // Dca
                    x.[320] // Dcb
                    x.[321] // Dcc
                    x.[322] // Dcd
                    x.[323] // DdA
                    x.[324] // DdB
                    x.[325] // DdC
                    2.0 * x.[326] // DdD
                    x.[327] // Dda
                    x.[328] // Ddb
                    x.[329] // Ddc
                    x.[330] // Ddd
                    x.[334] // aAD
                    x.[342] // aBD
                    x.[350] // aCD
                    x.[355] // aDA
                    x.[356] // aDB
                    x.[357] // aDC
                    2.0 * x.[358] // aDD
                    x.[359] // aDa
                    x.[360] // aDb
                    x.[361] // aDc
                    x.[362] // aDd
                    x.[366] // aaD
                    x.[374] // abD
                    x.[382] // acD
                    x.[390] // adD
                    x.[398] // bAD
                    x.[406] // bBD
                    x.[414] // bCD
                    x.[419] // bDA
                    x.[420] // bDB
                    x.[421] // bDC
                    2.0 * x.[422] // bDD
                    x.[423] // bDa
                    x.[424] // bDb
                    x.[425] // bDc
                    x.[426] // bDd
                    x.[430] // baD
                    x.[438] // bbD
                    x.[446] // bcD
                    x.[454] // bdD
                    x.[462] // cAD
                    x.[470] // cBD
                    x.[478] // cCD
                    x.[483] // cDA
                    x.[484] // cDB
                    x.[485] // cDC
                    2.0 * x.[486] // cDD
                    x.[487] // cDa
                    x.[488] // cDb
                    x.[489] // cDc
                    x.[490] // cDd
                    x.[494] // caD
                    x.[502] // cbD
                    x.[510] // ccD
                    x.[518] // cdD
                    x.[526] // dAD
                    x.[534] // dBD
                    x.[542] // dCD
                    x.[547] // dDA
                    x.[548] // dDB
                    x.[549] // dDC
                    2.0 * x.[550] // dDD
                    x.[551] // dDa
                    x.[552] // dDb
                    x.[553] // dDc
                    x.[554] // dDd
                    x.[558] // daD
                    x.[566] // dbD
                    x.[574] // dcD
                    x.[582] // ddD
                |]
                |> Array.sum
                ,
                [|
                    x.[10] // d
                    x.[18] // Ad
                    x.[26] // Bd
                    x.[34] // Cd
                    x.[42] // Dd
                    x.[50] // ad
                    x.[58] // bd
                    x.[66] // cd
                    x.[67] // dA
                    x.[68] // dB
                    x.[69] // dC
                    x.[70] // dD
                    x.[71] // da
                    x.[72] // db
                    x.[73] // dc
                    2.0 * x.[74] // dd
                    x.[82] // AAd
                    x.[90] // ABd
                    x.[98] // ACd
                    x.[106] // ADd
                    x.[114] // Aad
                    x.[122] // Abd
                    x.[130] // Acd
                    x.[131] // AdA
                    x.[132] // AdB
                    x.[133] // AdC
                    x.[134] // AdD
                    x.[135] // Ada
                    x.[136] // Adb
                    x.[137] // Adc
                    2.0 * x.[138] // Add
                    x.[146] // BAd
                    x.[154] // BBd
                    x.[162] // BCd
                    x.[170] // BDd
                    x.[178] // Bad
                    x.[186] // Bbd
                    x.[194] // Bcd
                    x.[195] // BdA
                    x.[196] // BdB
                    x.[197] // BdC
                    x.[198] // BdD
                    x.[199] // Bda
                    x.[200] // Bdb
                    x.[201] // Bdc
                    2.0 * x.[202] // Bdd
                    x.[210] // CAd
                    x.[218] // CBd
                    x.[226] // CCd
                    x.[234] // CDd
                    x.[242] // Cad
                    x.[250] // Cbd
                    x.[258] // Ccd
                    x.[259] // CdA
                    x.[260] // CdB
                    x.[261] // CdC
                    x.[262] // CdD
                    x.[263] // Cda
                    x.[264] // Cdb
                    x.[265] // Cdc
                    2.0 * x.[266] // Cdd
                    x.[274] // DAd
                    x.[282] // DBd
                    x.[290] // DCd
                    x.[298] // DDd
                    x.[306] // Dad
                    x.[314] // Dbd
                    x.[322] // Dcd
                    x.[323] // DdA
                    x.[324] // DdB
                    x.[325] // DdC
                    x.[326] // DdD
                    x.[327] // Dda
                    x.[328] // Ddb
                    x.[329] // Ddc
                    2.0 * x.[330] // Ddd
                    x.[338] // aAd
                    x.[346] // aBd
                    x.[354] // aCd
                    x.[362] // aDd
                    x.[370] // aad
                    x.[378] // abd
                    x.[386] // acd
                    x.[387] // adA
                    x.[388] // adB
                    x.[389] // adC
                    x.[390] // adD
                    x.[391] // ada
                    x.[392] // adb
                    x.[393] // adc
                    2.0 * x.[394] // add
                    x.[402] // bAd
                    x.[410] // bBd
                    x.[418] // bCd
                    x.[426] // bDd
                    x.[434] // bad
                    x.[442] // bbd
                    x.[450] // bcd
                    x.[451] // bdA
                    x.[452] // bdB
                    x.[453] // bdC
                    x.[454] // bdD
                    x.[455] // bda
                    x.[456] // bdb
                    x.[457] // bdc
                    2.0 * x.[458] // bdd
                    x.[466] // cAd
                    x.[474] // cBd
                    x.[482] // cCd
                    x.[490] // cDd
                    x.[498] // cad
                    x.[506] // cbd
                    x.[514] // ccd
                    x.[515] // cdA
                    x.[516] // cdB
                    x.[517] // cdC
                    x.[518] // cdD
                    x.[519] // cda
                    x.[520] // cdb
                    x.[521] // cdc
                    2.0 * x.[522] // cdd
                    x.[523] // dAA
                    x.[524] // dAB
                    x.[525] // dAC
                    x.[526] // dAD
                    x.[527] // dAa
                    x.[528] // dAb
                    x.[529] // dAc
                    2.0 * x.[530] // dAd
                    x.[531] // dBA
                    x.[532] // dBB
                    x.[533] // dBC
                    x.[534] // dBD
                    x.[535] // dBa
                    x.[536] // dBb
                    x.[537] // dBc
                    2.0 * x.[538] // dBd
                    x.[539] // dCA
                    x.[540] // dCB
                    x.[541] // dCC
                    x.[542] // dCD
                    x.[543] // dCa
                    x.[544] // dCb
                    x.[545] // dCc
                    2.0 * x.[546] // dCd
                    x.[547] // dDA
                    x.[548] // dDB
                    x.[549] // dDC
                    x.[550] // dDD
                    x.[551] // dDa
                    x.[552] // dDb
                    x.[553] // dDc
                    2.0 * x.[554] // dDd
                    x.[555] // daA
                    x.[556] // daB
                    x.[557] // daC
                    x.[558] // daD
                    x.[559] // daa
                    x.[560] // dab
                    x.[561] // dac
                    2.0 * x.[562] // dad
                    x.[563] // dbA
                    x.[564] // dbB
                    x.[565] // dbC
                    x.[566] // dbD
                    x.[567] // dba
                    x.[568] // dbb
                    x.[569] // dbc
                    2.0 * x.[570] // dbd
                    x.[571] // dcA
                    x.[572] // dcB
                    x.[573] // dcC
                    x.[574] // dcD
                    x.[575] // dca
                    x.[576] // dcb
                    x.[577] // dcc
                    2.0 * x.[578] // dcd
                    2.0 * x.[579] // ddA
                    2.0 * x.[580] // ddB
                    2.0 * x.[581] // ddC
                    2.0 * x.[582] // ddD
                    2.0 * x.[583] // dda
                    2.0 * x.[584] // ddb
                    2.0 * x.[585] // ddc
                    3.0 * x.[586] // ddd
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
            0.001 * x.[10] // d | synthesis: Y <-> d
            -0.001 * x.[1] // Y | synthesis: Y <-> d
            0.001 * x.[6] // D | synthesis: Y <-> D
            -0.001 * x.[1] // Y | synthesis: Y <-> D
            0.001 * x.[9] // c | synthesis: Y <-> c
            -0.001 * x.[1] // Y | synthesis: Y <-> c
            0.001 * x.[5] // C | synthesis: Y <-> C
            -0.001 * x.[1] // Y | synthesis: Y <-> C
            0.001 * x.[8] // b | synthesis: Y <-> b
            -0.001 * x.[1] // Y | synthesis: Y <-> b
            0.001 * x.[4] // B | synthesis: Y <-> B
            -0.001 * x.[1] // Y | synthesis: Y <-> B
            0.001 * x.[7] // a | synthesis: Y <-> a
            -0.001 * x.[1] // Y | synthesis: Y <-> a
            0.001 * x.[3] // A | synthesis: Y <-> A
            -0.001 * x.[1] // Y | synthesis: Y <-> A
            0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 2 - Z
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -0.001 * x.[2] // Z | destruction: d <-> Z
            0.001 * x.[10] // d | destruction: d <-> Z
            -0.001 * x.[2] // Z | destruction: D <-> Z
            0.001 * x.[6] // D | destruction: D <-> Z
            -0.001 * x.[2] // Z | destruction: c <-> Z
            0.001 * x.[9] // c | destruction: c <-> Z
            -0.001 * x.[2] // Z | destruction: C <-> Z
            0.001 * x.[5] // C | destruction: C <-> Z
            -0.001 * x.[2] // Z | destruction: b <-> Z
            0.001 * x.[8] // b | destruction: b <-> Z
            -0.001 * x.[2] // Z | destruction: B <-> Z
            0.001 * x.[4] // B | destruction: B <-> Z
            -0.001 * x.[2] // Z | destruction: a <-> Z
            0.001 * x.[7] // a | destruction: a <-> Z
            -0.001 * x.[2] // Z | destruction: A <-> Z
            0.001 * x.[3] // A | destruction: A <-> Z
            -0.1 * x.[2] // Z | recycling: Z -> Y
        |]
        |> Array.sum


    // 3 - A
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[67] // dA | ligation: d + A <-> dA
            -1.0 * x.[10] * x.[3] // d + A | ligation: d + A <-> dA
            1.0 * x.[59] // cA | ligation: c + A <-> cA
            -1.0 * x.[9] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[51] // bA | ligation: b + A <-> bA
            -1.0 * x.[8] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[138] // Add | ligation: A + dd <-> Add
            -1.0 * x.[3] * x.[74] // A + dd | ligation: A + dd <-> Add
            1.0 * x.[137] // Adc | ligation: A + dc <-> Adc
            -1.0 * x.[3] * x.[73] // A + dc | ligation: A + dc <-> Adc
            1.0 * x.[136] // Adb | ligation: A + db <-> Adb
            -1.0 * x.[3] * x.[72] // A + db | ligation: A + db <-> Adb
            1.0 * x.[135] // Ada | ligation: A + da <-> Ada
            -1.0 * x.[3] * x.[71] // A + da | ligation: A + da <-> Ada
            1.0 * x.[134] // AdD | ligation: A + dD <-> AdD
            -1.0 * x.[3] * x.[70] // A + dD | ligation: A + dD <-> AdD
            1.0 * x.[133] // AdC | ligation: A + dC <-> AdC
            -1.0 * x.[3] * x.[69] // A + dC | ligation: A + dC <-> AdC
            1.0 * x.[132] // AdB | ligation: A + dB <-> AdB
            -1.0 * x.[3] * x.[68] // A + dB | ligation: A + dB <-> AdB
            1.0 * x.[131] // AdA | ligation: A + dA <-> AdA
            -1.0 * x.[3] * x.[67] // A + dA | ligation: A + dA <-> AdA
            1.0 * x.[130] // Acd | ligation: A + cd <-> Acd
            -1.0 * x.[3] * x.[66] // A + cd | ligation: A + cd <-> Acd
            1.0 * x.[129] // Acc | ligation: A + cc <-> Acc
            -1.0 * x.[3] * x.[65] // A + cc | ligation: A + cc <-> Acc
            1.0 * x.[128] // Acb | ligation: A + cb <-> Acb
            -1.0 * x.[3] * x.[64] // A + cb | ligation: A + cb <-> Acb
            1.0 * x.[127] // Aca | ligation: A + ca <-> Aca
            -1.0 * x.[3] * x.[63] // A + ca | ligation: A + ca <-> Aca
            1.0 * x.[126] // AcD | ligation: A + cD <-> AcD
            -1.0 * x.[3] * x.[62] // A + cD | ligation: A + cD <-> AcD
            1.0 * x.[125] // AcC | ligation: A + cC <-> AcC
            -1.0 * x.[3] * x.[61] // A + cC | ligation: A + cC <-> AcC
            1.0 * x.[124] // AcB | ligation: A + cB <-> AcB
            -1.0 * x.[3] * x.[60] // A + cB | ligation: A + cB <-> AcB
            1.0 * x.[123] // AcA | ligation: A + cA <-> AcA
            -1.0 * x.[3] * x.[59] // A + cA | ligation: A + cA <-> AcA
            1.0 * x.[122] // Abd | ligation: A + bd <-> Abd
            -1.0 * x.[3] * x.[58] // A + bd | ligation: A + bd <-> Abd
            1.0 * x.[121] // Abc | ligation: A + bc <-> Abc
            -1.0 * x.[3] * x.[57] // A + bc | ligation: A + bc <-> Abc
            1.0 * x.[120] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[56] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[119] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[55] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[118] // AbD | ligation: A + bD <-> AbD
            -1.0 * x.[3] * x.[54] // A + bD | ligation: A + bD <-> AbD
            1.0 * x.[117] // AbC | ligation: A + bC <-> AbC
            -1.0 * x.[3] * x.[53] // A + bC | ligation: A + bC <-> AbC
            1.0 * x.[116] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[52] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[115] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[51] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[114] // Aad | ligation: A + ad <-> Aad
            -1.0 * x.[3] * x.[50] // A + ad | ligation: A + ad <-> Aad
            1.0 * x.[113] // Aac | ligation: A + ac <-> Aac
            -1.0 * x.[3] * x.[49] // A + ac | ligation: A + ac <-> Aac
            1.0 * x.[112] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[48] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[111] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[47] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[110] // AaD | ligation: A + aD <-> AaD
            -1.0 * x.[3] * x.[46] // A + aD | ligation: A + aD <-> AaD
            1.0 * x.[109] // AaC | ligation: A + aC <-> AaC
            -1.0 * x.[3] * x.[45] // A + aC | ligation: A + aC <-> AaC
            1.0 * x.[108] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[44] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[107] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[43] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[106] // ADd | ligation: A + Dd <-> ADd
            -1.0 * x.[3] * x.[42] // A + Dd | ligation: A + Dd <-> ADd
            1.0 * x.[105] // ADc | ligation: A + Dc <-> ADc
            -1.0 * x.[3] * x.[41] // A + Dc | ligation: A + Dc <-> ADc
            1.0 * x.[104] // ADb | ligation: A + Db <-> ADb
            -1.0 * x.[3] * x.[40] // A + Db | ligation: A + Db <-> ADb
            1.0 * x.[103] // ADa | ligation: A + Da <-> ADa
            -1.0 * x.[3] * x.[39] // A + Da | ligation: A + Da <-> ADa
            1.0 * x.[102] // ADD | ligation: A + DD <-> ADD
            -1.0 * x.[3] * x.[38] // A + DD | ligation: A + DD <-> ADD
            1.0 * x.[101] // ADC | ligation: A + DC <-> ADC
            -1.0 * x.[3] * x.[37] // A + DC | ligation: A + DC <-> ADC
            1.0 * x.[100] // ADB | ligation: A + DB <-> ADB
            -1.0 * x.[3] * x.[36] // A + DB | ligation: A + DB <-> ADB
            1.0 * x.[99] // ADA | ligation: A + DA <-> ADA
            -1.0 * x.[3] * x.[35] // A + DA | ligation: A + DA <-> ADA
            1.0 * x.[98] // ACd | ligation: A + Cd <-> ACd
            -1.0 * x.[3] * x.[34] // A + Cd | ligation: A + Cd <-> ACd
            1.0 * x.[97] // ACc | ligation: A + Cc <-> ACc
            -1.0 * x.[3] * x.[33] // A + Cc | ligation: A + Cc <-> ACc
            1.0 * x.[96] // ACb | ligation: A + Cb <-> ACb
            -1.0 * x.[3] * x.[32] // A + Cb | ligation: A + Cb <-> ACb
            1.0 * x.[95] // ACa | ligation: A + Ca <-> ACa
            -1.0 * x.[3] * x.[31] // A + Ca | ligation: A + Ca <-> ACa
            1.0 * x.[94] // ACD | ligation: A + CD <-> ACD
            -1.0 * x.[3] * x.[30] // A + CD | ligation: A + CD <-> ACD
            1.0 * x.[93] // ACC | ligation: A + CC <-> ACC
            -1.0 * x.[3] * x.[29] // A + CC | ligation: A + CC <-> ACC
            1.0 * x.[92] // ACB | ligation: A + CB <-> ACB
            -1.0 * x.[3] * x.[28] // A + CB | ligation: A + CB <-> ACB
            1.0 * x.[91] // ACA | ligation: A + CA <-> ACA
            -1.0 * x.[3] * x.[27] // A + CA | ligation: A + CA <-> ACA
            1.0 * x.[90] // ABd | ligation: A + Bd <-> ABd
            -1.0 * x.[3] * x.[26] // A + Bd | ligation: A + Bd <-> ABd
            1.0 * x.[89] // ABc | ligation: A + Bc <-> ABc
            -1.0 * x.[3] * x.[25] // A + Bc | ligation: A + Bc <-> ABc
            1.0 * x.[88] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[24] // A + Bb | ligation: A + Bb <-> ABb
            1.0 * x.[87] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[23] // A + Ba | ligation: A + Ba <-> ABa
            1.0 * x.[86] // ABD | ligation: A + BD <-> ABD
            -1.0 * x.[3] * x.[22] // A + BD | ligation: A + BD <-> ABD
            1.0 * x.[85] // ABC | ligation: A + BC <-> ABC
            -1.0 * x.[3] * x.[21] // A + BC | ligation: A + BC <-> ABC
            1.0 * x.[84] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[20] // A + BB | ligation: A + BB <-> ABB
            1.0 * x.[83] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[19] // A + BA | ligation: A + BA <-> ABA
            1.0 * x.[82] // AAd | ligation: A + Ad <-> AAd
            -1.0 * x.[3] * x.[18] // A + Ad | ligation: A + Ad <-> AAd
            1.0 * x.[81] // AAc | ligation: A + Ac <-> AAc
            -1.0 * x.[3] * x.[17] // A + Ac | ligation: A + Ac <-> AAc
            1.0 * x.[80] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[16] // A + Ab | ligation: A + Ab <-> AAb
            1.0 * x.[79] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[15] // A + Aa | ligation: A + Aa <-> AAa
            1.0 * x.[78] // AAD | ligation: A + AD <-> AAD
            -1.0 * x.[3] * x.[14] // A + AD | ligation: A + AD <-> AAD
            1.0 * x.[77] // AAC | ligation: A + AC <-> AAC
            -1.0 * x.[3] * x.[13] // A + AC | ligation: A + AC <-> AAC
            1.0 * x.[76] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[12] // A + AB | ligation: A + AB <-> AAB
            1.0 * x.[75] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[11] // A + AA | ligation: A + AA <-> AAA
            1.0 * x.[18] // Ad | ligation: A + d <-> Ad
            -1.0 * x.[3] * x.[10] // A + d | ligation: A + d <-> Ad
            1.0 * x.[17] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[9] // A + c | ligation: A + c <-> Ac
            1.0 * x.[16] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[8] // A + b | ligation: A + b <-> Ab
            1.0 * x.[43] // aA | ligation: a + A <-> aA
            -1.0 * x.[7] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[15] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[7] // A + a | ligation: A + a <-> Aa
            1.0 * x.[14] // AD | ligation: A + D <-> AD
            -1.0 * x.[3] * x.[6] // A + D | ligation: A + D <-> AD
            1.0 * x.[13] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
            1.0 * x.[12] // AB | ligation: A + B <-> AB
            -1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
            1.0 * x.[11] // AA | ligation: A + A <-> AA
            1.0 * x.[11] // AA | ligation: A + A <-> AA
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
            1.0 * x.[68] // dB | ligation: d + B <-> dB
            -1.0 * x.[10] * x.[4] // d + B | ligation: d + B <-> dB
            1.0 * x.[60] // cB | ligation: c + B <-> cB
            -1.0 * x.[9] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[202] // Bdd | ligation: B + dd <-> Bdd
            -1.0 * x.[4] * x.[74] // B + dd | ligation: B + dd <-> Bdd
            1.0 * x.[201] // Bdc | ligation: B + dc <-> Bdc
            -1.0 * x.[4] * x.[73] // B + dc | ligation: B + dc <-> Bdc
            1.0 * x.[200] // Bdb | ligation: B + db <-> Bdb
            -1.0 * x.[4] * x.[72] // B + db | ligation: B + db <-> Bdb
            1.0 * x.[199] // Bda | ligation: B + da <-> Bda
            -1.0 * x.[4] * x.[71] // B + da | ligation: B + da <-> Bda
            1.0 * x.[198] // BdD | ligation: B + dD <-> BdD
            -1.0 * x.[4] * x.[70] // B + dD | ligation: B + dD <-> BdD
            1.0 * x.[197] // BdC | ligation: B + dC <-> BdC
            -1.0 * x.[4] * x.[69] // B + dC | ligation: B + dC <-> BdC
            1.0 * x.[196] // BdB | ligation: B + dB <-> BdB
            -1.0 * x.[4] * x.[68] // B + dB | ligation: B + dB <-> BdB
            1.0 * x.[195] // BdA | ligation: B + dA <-> BdA
            -1.0 * x.[4] * x.[67] // B + dA | ligation: B + dA <-> BdA
            1.0 * x.[194] // Bcd | ligation: B + cd <-> Bcd
            -1.0 * x.[4] * x.[66] // B + cd | ligation: B + cd <-> Bcd
            1.0 * x.[193] // Bcc | ligation: B + cc <-> Bcc
            -1.0 * x.[4] * x.[65] // B + cc | ligation: B + cc <-> Bcc
            1.0 * x.[192] // Bcb | ligation: B + cb <-> Bcb
            -1.0 * x.[4] * x.[64] // B + cb | ligation: B + cb <-> Bcb
            1.0 * x.[191] // Bca | ligation: B + ca <-> Bca
            -1.0 * x.[4] * x.[63] // B + ca | ligation: B + ca <-> Bca
            1.0 * x.[190] // BcD | ligation: B + cD <-> BcD
            -1.0 * x.[4] * x.[62] // B + cD | ligation: B + cD <-> BcD
            1.0 * x.[189] // BcC | ligation: B + cC <-> BcC
            -1.0 * x.[4] * x.[61] // B + cC | ligation: B + cC <-> BcC
            1.0 * x.[188] // BcB | ligation: B + cB <-> BcB
            -1.0 * x.[4] * x.[60] // B + cB | ligation: B + cB <-> BcB
            1.0 * x.[187] // BcA | ligation: B + cA <-> BcA
            -1.0 * x.[4] * x.[59] // B + cA | ligation: B + cA <-> BcA
            1.0 * x.[186] // Bbd | ligation: B + bd <-> Bbd
            -1.0 * x.[4] * x.[58] // B + bd | ligation: B + bd <-> Bbd
            1.0 * x.[185] // Bbc | ligation: B + bc <-> Bbc
            -1.0 * x.[4] * x.[57] // B + bc | ligation: B + bc <-> Bbc
            1.0 * x.[184] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[56] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[183] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[55] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[182] // BbD | ligation: B + bD <-> BbD
            -1.0 * x.[4] * x.[54] // B + bD | ligation: B + bD <-> BbD
            1.0 * x.[181] // BbC | ligation: B + bC <-> BbC
            -1.0 * x.[4] * x.[53] // B + bC | ligation: B + bC <-> BbC
            1.0 * x.[180] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[52] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[179] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[51] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[178] // Bad | ligation: B + ad <-> Bad
            -1.0 * x.[4] * x.[50] // B + ad | ligation: B + ad <-> Bad
            1.0 * x.[177] // Bac | ligation: B + ac <-> Bac
            -1.0 * x.[4] * x.[49] // B + ac | ligation: B + ac <-> Bac
            1.0 * x.[176] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[48] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[175] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[47] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[174] // BaD | ligation: B + aD <-> BaD
            -1.0 * x.[4] * x.[46] // B + aD | ligation: B + aD <-> BaD
            1.0 * x.[173] // BaC | ligation: B + aC <-> BaC
            -1.0 * x.[4] * x.[45] // B + aC | ligation: B + aC <-> BaC
            1.0 * x.[172] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[44] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[171] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[43] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[170] // BDd | ligation: B + Dd <-> BDd
            -1.0 * x.[4] * x.[42] // B + Dd | ligation: B + Dd <-> BDd
            1.0 * x.[169] // BDc | ligation: B + Dc <-> BDc
            -1.0 * x.[4] * x.[41] // B + Dc | ligation: B + Dc <-> BDc
            1.0 * x.[168] // BDb | ligation: B + Db <-> BDb
            -1.0 * x.[4] * x.[40] // B + Db | ligation: B + Db <-> BDb
            1.0 * x.[167] // BDa | ligation: B + Da <-> BDa
            -1.0 * x.[4] * x.[39] // B + Da | ligation: B + Da <-> BDa
            1.0 * x.[166] // BDD | ligation: B + DD <-> BDD
            -1.0 * x.[4] * x.[38] // B + DD | ligation: B + DD <-> BDD
            1.0 * x.[165] // BDC | ligation: B + DC <-> BDC
            -1.0 * x.[4] * x.[37] // B + DC | ligation: B + DC <-> BDC
            1.0 * x.[164] // BDB | ligation: B + DB <-> BDB
            -1.0 * x.[4] * x.[36] // B + DB | ligation: B + DB <-> BDB
            1.0 * x.[163] // BDA | ligation: B + DA <-> BDA
            -1.0 * x.[4] * x.[35] // B + DA | ligation: B + DA <-> BDA
            1.0 * x.[162] // BCd | ligation: B + Cd <-> BCd
            -1.0 * x.[4] * x.[34] // B + Cd | ligation: B + Cd <-> BCd
            1.0 * x.[161] // BCc | ligation: B + Cc <-> BCc
            -1.0 * x.[4] * x.[33] // B + Cc | ligation: B + Cc <-> BCc
            1.0 * x.[160] // BCb | ligation: B + Cb <-> BCb
            -1.0 * x.[4] * x.[32] // B + Cb | ligation: B + Cb <-> BCb
            1.0 * x.[159] // BCa | ligation: B + Ca <-> BCa
            -1.0 * x.[4] * x.[31] // B + Ca | ligation: B + Ca <-> BCa
            1.0 * x.[158] // BCD | ligation: B + CD <-> BCD
            -1.0 * x.[4] * x.[30] // B + CD | ligation: B + CD <-> BCD
            1.0 * x.[157] // BCC | ligation: B + CC <-> BCC
            -1.0 * x.[4] * x.[29] // B + CC | ligation: B + CC <-> BCC
            1.0 * x.[156] // BCB | ligation: B + CB <-> BCB
            -1.0 * x.[4] * x.[28] // B + CB | ligation: B + CB <-> BCB
            1.0 * x.[155] // BCA | ligation: B + CA <-> BCA
            -1.0 * x.[4] * x.[27] // B + CA | ligation: B + CA <-> BCA
            1.0 * x.[154] // BBd | ligation: B + Bd <-> BBd
            -1.0 * x.[4] * x.[26] // B + Bd | ligation: B + Bd <-> BBd
            1.0 * x.[153] // BBc | ligation: B + Bc <-> BBc
            -1.0 * x.[4] * x.[25] // B + Bc | ligation: B + Bc <-> BBc
            1.0 * x.[152] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[24] // B + Bb | ligation: B + Bb <-> BBb
            1.0 * x.[151] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[23] // B + Ba | ligation: B + Ba <-> BBa
            1.0 * x.[150] // BBD | ligation: B + BD <-> BBD
            -1.0 * x.[4] * x.[22] // B + BD | ligation: B + BD <-> BBD
            1.0 * x.[149] // BBC | ligation: B + BC <-> BBC
            -1.0 * x.[4] * x.[21] // B + BC | ligation: B + BC <-> BBC
            1.0 * x.[148] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[20] // B + BB | ligation: B + BB <-> BBB
            1.0 * x.[147] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[19] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[146] // BAd | ligation: B + Ad <-> BAd
            -1.0 * x.[4] * x.[18] // B + Ad | ligation: B + Ad <-> BAd
            1.0 * x.[145] // BAc | ligation: B + Ac <-> BAc
            -1.0 * x.[4] * x.[17] // B + Ac | ligation: B + Ac <-> BAc
            1.0 * x.[144] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[16] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[143] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[15] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[142] // BAD | ligation: B + AD <-> BAD
            -1.0 * x.[4] * x.[14] // B + AD | ligation: B + AD <-> BAD
            1.0 * x.[141] // BAC | ligation: B + AC <-> BAC
            -1.0 * x.[4] * x.[13] // B + AC | ligation: B + AC <-> BAC
            1.0 * x.[140] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[12] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[139] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[11] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[26] // Bd | ligation: B + d <-> Bd
            -1.0 * x.[4] * x.[10] // B + d | ligation: B + d <-> Bd
            1.0 * x.[25] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[9] // B + c | ligation: B + c <-> Bc
            1.0 * x.[52] // bB | ligation: b + B <-> bB
            -1.0 * x.[8] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[24] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[8] // B + b | ligation: B + b <-> Bb
            1.0 * x.[23] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[7] // B + a | ligation: B + a <-> Ba
            1.0 * x.[22] // BD | ligation: B + D <-> BD
            -1.0 * x.[4] * x.[6] // B + D | ligation: B + D <-> BD
            1.0 * x.[21] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[20] // BB | ligation: B + B <-> BB
            1.0 * x.[20] // BB | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            -1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[44] // aB | ligation: a + B <-> aB
            -1.0 * x.[7] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[12] // AB | ligation: A + B <-> AB
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
            1.0 * x.[69] // dC | ligation: d + C <-> dC
            -1.0 * x.[10] * x.[5] // d + C | ligation: d + C <-> dC
            1.0 * x.[266] // Cdd | ligation: C + dd <-> Cdd
            -1.0 * x.[5] * x.[74] // C + dd | ligation: C + dd <-> Cdd
            1.0 * x.[265] // Cdc | ligation: C + dc <-> Cdc
            -1.0 * x.[5] * x.[73] // C + dc | ligation: C + dc <-> Cdc
            1.0 * x.[264] // Cdb | ligation: C + db <-> Cdb
            -1.0 * x.[5] * x.[72] // C + db | ligation: C + db <-> Cdb
            1.0 * x.[263] // Cda | ligation: C + da <-> Cda
            -1.0 * x.[5] * x.[71] // C + da | ligation: C + da <-> Cda
            1.0 * x.[262] // CdD | ligation: C + dD <-> CdD
            -1.0 * x.[5] * x.[70] // C + dD | ligation: C + dD <-> CdD
            1.0 * x.[261] // CdC | ligation: C + dC <-> CdC
            -1.0 * x.[5] * x.[69] // C + dC | ligation: C + dC <-> CdC
            1.0 * x.[260] // CdB | ligation: C + dB <-> CdB
            -1.0 * x.[5] * x.[68] // C + dB | ligation: C + dB <-> CdB
            1.0 * x.[259] // CdA | ligation: C + dA <-> CdA
            -1.0 * x.[5] * x.[67] // C + dA | ligation: C + dA <-> CdA
            1.0 * x.[258] // Ccd | ligation: C + cd <-> Ccd
            -1.0 * x.[5] * x.[66] // C + cd | ligation: C + cd <-> Ccd
            1.0 * x.[257] // Ccc | ligation: C + cc <-> Ccc
            -1.0 * x.[5] * x.[65] // C + cc | ligation: C + cc <-> Ccc
            1.0 * x.[256] // Ccb | ligation: C + cb <-> Ccb
            -1.0 * x.[5] * x.[64] // C + cb | ligation: C + cb <-> Ccb
            1.0 * x.[255] // Cca | ligation: C + ca <-> Cca
            -1.0 * x.[5] * x.[63] // C + ca | ligation: C + ca <-> Cca
            1.0 * x.[254] // CcD | ligation: C + cD <-> CcD
            -1.0 * x.[5] * x.[62] // C + cD | ligation: C + cD <-> CcD
            1.0 * x.[253] // CcC | ligation: C + cC <-> CcC
            -1.0 * x.[5] * x.[61] // C + cC | ligation: C + cC <-> CcC
            1.0 * x.[252] // CcB | ligation: C + cB <-> CcB
            -1.0 * x.[5] * x.[60] // C + cB | ligation: C + cB <-> CcB
            1.0 * x.[251] // CcA | ligation: C + cA <-> CcA
            -1.0 * x.[5] * x.[59] // C + cA | ligation: C + cA <-> CcA
            1.0 * x.[250] // Cbd | ligation: C + bd <-> Cbd
            -1.0 * x.[5] * x.[58] // C + bd | ligation: C + bd <-> Cbd
            1.0 * x.[249] // Cbc | ligation: C + bc <-> Cbc
            -1.0 * x.[5] * x.[57] // C + bc | ligation: C + bc <-> Cbc
            1.0 * x.[248] // Cbb | ligation: C + bb <-> Cbb
            -1.0 * x.[5] * x.[56] // C + bb | ligation: C + bb <-> Cbb
            1.0 * x.[247] // Cba | ligation: C + ba <-> Cba
            -1.0 * x.[5] * x.[55] // C + ba | ligation: C + ba <-> Cba
            1.0 * x.[246] // CbD | ligation: C + bD <-> CbD
            -1.0 * x.[5] * x.[54] // C + bD | ligation: C + bD <-> CbD
            1.0 * x.[245] // CbC | ligation: C + bC <-> CbC
            -1.0 * x.[5] * x.[53] // C + bC | ligation: C + bC <-> CbC
            1.0 * x.[244] // CbB | ligation: C + bB <-> CbB
            -1.0 * x.[5] * x.[52] // C + bB | ligation: C + bB <-> CbB
            1.0 * x.[243] // CbA | ligation: C + bA <-> CbA
            -1.0 * x.[5] * x.[51] // C + bA | ligation: C + bA <-> CbA
            1.0 * x.[242] // Cad | ligation: C + ad <-> Cad
            -1.0 * x.[5] * x.[50] // C + ad | ligation: C + ad <-> Cad
            1.0 * x.[241] // Cac | ligation: C + ac <-> Cac
            -1.0 * x.[5] * x.[49] // C + ac | ligation: C + ac <-> Cac
            1.0 * x.[240] // Cab | ligation: C + ab <-> Cab
            -1.0 * x.[5] * x.[48] // C + ab | ligation: C + ab <-> Cab
            1.0 * x.[239] // Caa | ligation: C + aa <-> Caa
            -1.0 * x.[5] * x.[47] // C + aa | ligation: C + aa <-> Caa
            1.0 * x.[238] // CaD | ligation: C + aD <-> CaD
            -1.0 * x.[5] * x.[46] // C + aD | ligation: C + aD <-> CaD
            1.0 * x.[237] // CaC | ligation: C + aC <-> CaC
            -1.0 * x.[5] * x.[45] // C + aC | ligation: C + aC <-> CaC
            1.0 * x.[236] // CaB | ligation: C + aB <-> CaB
            -1.0 * x.[5] * x.[44] // C + aB | ligation: C + aB <-> CaB
            1.0 * x.[235] // CaA | ligation: C + aA <-> CaA
            -1.0 * x.[5] * x.[43] // C + aA | ligation: C + aA <-> CaA
            1.0 * x.[234] // CDd | ligation: C + Dd <-> CDd
            -1.0 * x.[5] * x.[42] // C + Dd | ligation: C + Dd <-> CDd
            1.0 * x.[233] // CDc | ligation: C + Dc <-> CDc
            -1.0 * x.[5] * x.[41] // C + Dc | ligation: C + Dc <-> CDc
            1.0 * x.[232] // CDb | ligation: C + Db <-> CDb
            -1.0 * x.[5] * x.[40] // C + Db | ligation: C + Db <-> CDb
            1.0 * x.[231] // CDa | ligation: C + Da <-> CDa
            -1.0 * x.[5] * x.[39] // C + Da | ligation: C + Da <-> CDa
            1.0 * x.[230] // CDD | ligation: C + DD <-> CDD
            -1.0 * x.[5] * x.[38] // C + DD | ligation: C + DD <-> CDD
            1.0 * x.[229] // CDC | ligation: C + DC <-> CDC
            -1.0 * x.[5] * x.[37] // C + DC | ligation: C + DC <-> CDC
            1.0 * x.[228] // CDB | ligation: C + DB <-> CDB
            -1.0 * x.[5] * x.[36] // C + DB | ligation: C + DB <-> CDB
            1.0 * x.[227] // CDA | ligation: C + DA <-> CDA
            -1.0 * x.[5] * x.[35] // C + DA | ligation: C + DA <-> CDA
            1.0 * x.[226] // CCd | ligation: C + Cd <-> CCd
            -1.0 * x.[5] * x.[34] // C + Cd | ligation: C + Cd <-> CCd
            1.0 * x.[225] // CCc | ligation: C + Cc <-> CCc
            -1.0 * x.[5] * x.[33] // C + Cc | ligation: C + Cc <-> CCc
            1.0 * x.[224] // CCb | ligation: C + Cb <-> CCb
            -1.0 * x.[5] * x.[32] // C + Cb | ligation: C + Cb <-> CCb
            1.0 * x.[223] // CCa | ligation: C + Ca <-> CCa
            -1.0 * x.[5] * x.[31] // C + Ca | ligation: C + Ca <-> CCa
            1.0 * x.[222] // CCD | ligation: C + CD <-> CCD
            -1.0 * x.[5] * x.[30] // C + CD | ligation: C + CD <-> CCD
            1.0 * x.[221] // CCC | ligation: C + CC <-> CCC
            -1.0 * x.[5] * x.[29] // C + CC | ligation: C + CC <-> CCC
            1.0 * x.[220] // CCB | ligation: C + CB <-> CCB
            -1.0 * x.[5] * x.[28] // C + CB | ligation: C + CB <-> CCB
            1.0 * x.[219] // CCA | ligation: C + CA <-> CCA
            -1.0 * x.[5] * x.[27] // C + CA | ligation: C + CA <-> CCA
            1.0 * x.[218] // CBd | ligation: C + Bd <-> CBd
            -1.0 * x.[5] * x.[26] // C + Bd | ligation: C + Bd <-> CBd
            1.0 * x.[217] // CBc | ligation: C + Bc <-> CBc
            -1.0 * x.[5] * x.[25] // C + Bc | ligation: C + Bc <-> CBc
            1.0 * x.[216] // CBb | ligation: C + Bb <-> CBb
            -1.0 * x.[5] * x.[24] // C + Bb | ligation: C + Bb <-> CBb
            1.0 * x.[215] // CBa | ligation: C + Ba <-> CBa
            -1.0 * x.[5] * x.[23] // C + Ba | ligation: C + Ba <-> CBa
            1.0 * x.[214] // CBD | ligation: C + BD <-> CBD
            -1.0 * x.[5] * x.[22] // C + BD | ligation: C + BD <-> CBD
            1.0 * x.[213] // CBC | ligation: C + BC <-> CBC
            -1.0 * x.[5] * x.[21] // C + BC | ligation: C + BC <-> CBC
            1.0 * x.[212] // CBB | ligation: C + BB <-> CBB
            -1.0 * x.[5] * x.[20] // C + BB | ligation: C + BB <-> CBB
            1.0 * x.[211] // CBA | ligation: C + BA <-> CBA
            -1.0 * x.[5] * x.[19] // C + BA | ligation: C + BA <-> CBA
            1.0 * x.[210] // CAd | ligation: C + Ad <-> CAd
            -1.0 * x.[5] * x.[18] // C + Ad | ligation: C + Ad <-> CAd
            1.0 * x.[209] // CAc | ligation: C + Ac <-> CAc
            -1.0 * x.[5] * x.[17] // C + Ac | ligation: C + Ac <-> CAc
            1.0 * x.[208] // CAb | ligation: C + Ab <-> CAb
            -1.0 * x.[5] * x.[16] // C + Ab | ligation: C + Ab <-> CAb
            1.0 * x.[207] // CAa | ligation: C + Aa <-> CAa
            -1.0 * x.[5] * x.[15] // C + Aa | ligation: C + Aa <-> CAa
            1.0 * x.[206] // CAD | ligation: C + AD <-> CAD
            -1.0 * x.[5] * x.[14] // C + AD | ligation: C + AD <-> CAD
            1.0 * x.[205] // CAC | ligation: C + AC <-> CAC
            -1.0 * x.[5] * x.[13] // C + AC | ligation: C + AC <-> CAC
            1.0 * x.[204] // CAB | ligation: C + AB <-> CAB
            -1.0 * x.[5] * x.[12] // C + AB | ligation: C + AB <-> CAB
            1.0 * x.[203] // CAA | ligation: C + AA <-> CAA
            -1.0 * x.[5] * x.[11] // C + AA | ligation: C + AA <-> CAA
            1.0 * x.[34] // Cd | ligation: C + d <-> Cd
            -1.0 * x.[5] * x.[10] // C + d | ligation: C + d <-> Cd
            1.0 * x.[61] // cC | ligation: c + C <-> cC
            -1.0 * x.[9] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[33] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[9] // C + c | ligation: C + c <-> Cc
            1.0 * x.[32] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[8] // C + b | ligation: C + b <-> Cb
            1.0 * x.[31] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[7] // C + a | ligation: C + a <-> Ca
            1.0 * x.[30] // CD | ligation: C + D <-> CD
            -1.0 * x.[5] * x.[6] // C + D | ligation: C + D <-> CD
            1.0 * x.[29] // CC | ligation: C + C <-> CC
            1.0 * x.[29] // CC | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            -1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            1.0 * x.[53] // bC | ligation: b + C <-> bC
            -1.0 * x.[8] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[21] // BC | ligation: B + C <-> BC
            -1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[45] // aC | ligation: a + C <-> aC
            -1.0 * x.[7] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[13] // AC | ligation: A + C <-> AC
            -1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
            0.001 * x.[2] // Z | destruction: C <-> Z
            -0.001 * x.[5] // C | destruction: C <-> Z
            -0.001 * x.[5] // C | synthesis: Y <-> C
            0.001 * x.[1] // Y | synthesis: Y <-> C
        |]
        |> Array.sum


    // 6 - D
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[330] // Ddd | ligation: D + dd <-> Ddd
            -1.0 * x.[6] * x.[74] // D + dd | ligation: D + dd <-> Ddd
            1.0 * x.[329] // Ddc | ligation: D + dc <-> Ddc
            -1.0 * x.[6] * x.[73] // D + dc | ligation: D + dc <-> Ddc
            1.0 * x.[328] // Ddb | ligation: D + db <-> Ddb
            -1.0 * x.[6] * x.[72] // D + db | ligation: D + db <-> Ddb
            1.0 * x.[327] // Dda | ligation: D + da <-> Dda
            -1.0 * x.[6] * x.[71] // D + da | ligation: D + da <-> Dda
            1.0 * x.[326] // DdD | ligation: D + dD <-> DdD
            -1.0 * x.[6] * x.[70] // D + dD | ligation: D + dD <-> DdD
            1.0 * x.[325] // DdC | ligation: D + dC <-> DdC
            -1.0 * x.[6] * x.[69] // D + dC | ligation: D + dC <-> DdC
            1.0 * x.[324] // DdB | ligation: D + dB <-> DdB
            -1.0 * x.[6] * x.[68] // D + dB | ligation: D + dB <-> DdB
            1.0 * x.[323] // DdA | ligation: D + dA <-> DdA
            -1.0 * x.[6] * x.[67] // D + dA | ligation: D + dA <-> DdA
            1.0 * x.[322] // Dcd | ligation: D + cd <-> Dcd
            -1.0 * x.[6] * x.[66] // D + cd | ligation: D + cd <-> Dcd
            1.0 * x.[321] // Dcc | ligation: D + cc <-> Dcc
            -1.0 * x.[6] * x.[65] // D + cc | ligation: D + cc <-> Dcc
            1.0 * x.[320] // Dcb | ligation: D + cb <-> Dcb
            -1.0 * x.[6] * x.[64] // D + cb | ligation: D + cb <-> Dcb
            1.0 * x.[319] // Dca | ligation: D + ca <-> Dca
            -1.0 * x.[6] * x.[63] // D + ca | ligation: D + ca <-> Dca
            1.0 * x.[318] // DcD | ligation: D + cD <-> DcD
            -1.0 * x.[6] * x.[62] // D + cD | ligation: D + cD <-> DcD
            1.0 * x.[317] // DcC | ligation: D + cC <-> DcC
            -1.0 * x.[6] * x.[61] // D + cC | ligation: D + cC <-> DcC
            1.0 * x.[316] // DcB | ligation: D + cB <-> DcB
            -1.0 * x.[6] * x.[60] // D + cB | ligation: D + cB <-> DcB
            1.0 * x.[315] // DcA | ligation: D + cA <-> DcA
            -1.0 * x.[6] * x.[59] // D + cA | ligation: D + cA <-> DcA
            1.0 * x.[314] // Dbd | ligation: D + bd <-> Dbd
            -1.0 * x.[6] * x.[58] // D + bd | ligation: D + bd <-> Dbd
            1.0 * x.[313] // Dbc | ligation: D + bc <-> Dbc
            -1.0 * x.[6] * x.[57] // D + bc | ligation: D + bc <-> Dbc
            1.0 * x.[312] // Dbb | ligation: D + bb <-> Dbb
            -1.0 * x.[6] * x.[56] // D + bb | ligation: D + bb <-> Dbb
            1.0 * x.[311] // Dba | ligation: D + ba <-> Dba
            -1.0 * x.[6] * x.[55] // D + ba | ligation: D + ba <-> Dba
            1.0 * x.[310] // DbD | ligation: D + bD <-> DbD
            -1.0 * x.[6] * x.[54] // D + bD | ligation: D + bD <-> DbD
            1.0 * x.[309] // DbC | ligation: D + bC <-> DbC
            -1.0 * x.[6] * x.[53] // D + bC | ligation: D + bC <-> DbC
            1.0 * x.[308] // DbB | ligation: D + bB <-> DbB
            -1.0 * x.[6] * x.[52] // D + bB | ligation: D + bB <-> DbB
            1.0 * x.[307] // DbA | ligation: D + bA <-> DbA
            -1.0 * x.[6] * x.[51] // D + bA | ligation: D + bA <-> DbA
            1.0 * x.[306] // Dad | ligation: D + ad <-> Dad
            -1.0 * x.[6] * x.[50] // D + ad | ligation: D + ad <-> Dad
            1.0 * x.[305] // Dac | ligation: D + ac <-> Dac
            -1.0 * x.[6] * x.[49] // D + ac | ligation: D + ac <-> Dac
            1.0 * x.[304] // Dab | ligation: D + ab <-> Dab
            -1.0 * x.[6] * x.[48] // D + ab | ligation: D + ab <-> Dab
            1.0 * x.[303] // Daa | ligation: D + aa <-> Daa
            -1.0 * x.[6] * x.[47] // D + aa | ligation: D + aa <-> Daa
            1.0 * x.[302] // DaD | ligation: D + aD <-> DaD
            -1.0 * x.[6] * x.[46] // D + aD | ligation: D + aD <-> DaD
            1.0 * x.[301] // DaC | ligation: D + aC <-> DaC
            -1.0 * x.[6] * x.[45] // D + aC | ligation: D + aC <-> DaC
            1.0 * x.[300] // DaB | ligation: D + aB <-> DaB
            -1.0 * x.[6] * x.[44] // D + aB | ligation: D + aB <-> DaB
            1.0 * x.[299] // DaA | ligation: D + aA <-> DaA
            -1.0 * x.[6] * x.[43] // D + aA | ligation: D + aA <-> DaA
            1.0 * x.[298] // DDd | ligation: D + Dd <-> DDd
            -1.0 * x.[6] * x.[42] // D + Dd | ligation: D + Dd <-> DDd
            1.0 * x.[297] // DDc | ligation: D + Dc <-> DDc
            -1.0 * x.[6] * x.[41] // D + Dc | ligation: D + Dc <-> DDc
            1.0 * x.[296] // DDb | ligation: D + Db <-> DDb
            -1.0 * x.[6] * x.[40] // D + Db | ligation: D + Db <-> DDb
            1.0 * x.[295] // DDa | ligation: D + Da <-> DDa
            -1.0 * x.[6] * x.[39] // D + Da | ligation: D + Da <-> DDa
            1.0 * x.[294] // DDD | ligation: D + DD <-> DDD
            -1.0 * x.[6] * x.[38] // D + DD | ligation: D + DD <-> DDD
            1.0 * x.[293] // DDC | ligation: D + DC <-> DDC
            -1.0 * x.[6] * x.[37] // D + DC | ligation: D + DC <-> DDC
            1.0 * x.[292] // DDB | ligation: D + DB <-> DDB
            -1.0 * x.[6] * x.[36] // D + DB | ligation: D + DB <-> DDB
            1.0 * x.[291] // DDA | ligation: D + DA <-> DDA
            -1.0 * x.[6] * x.[35] // D + DA | ligation: D + DA <-> DDA
            1.0 * x.[290] // DCd | ligation: D + Cd <-> DCd
            -1.0 * x.[6] * x.[34] // D + Cd | ligation: D + Cd <-> DCd
            1.0 * x.[289] // DCc | ligation: D + Cc <-> DCc
            -1.0 * x.[6] * x.[33] // D + Cc | ligation: D + Cc <-> DCc
            1.0 * x.[288] // DCb | ligation: D + Cb <-> DCb
            -1.0 * x.[6] * x.[32] // D + Cb | ligation: D + Cb <-> DCb
            1.0 * x.[287] // DCa | ligation: D + Ca <-> DCa
            -1.0 * x.[6] * x.[31] // D + Ca | ligation: D + Ca <-> DCa
            1.0 * x.[286] // DCD | ligation: D + CD <-> DCD
            -1.0 * x.[6] * x.[30] // D + CD | ligation: D + CD <-> DCD
            1.0 * x.[285] // DCC | ligation: D + CC <-> DCC
            -1.0 * x.[6] * x.[29] // D + CC | ligation: D + CC <-> DCC
            1.0 * x.[284] // DCB | ligation: D + CB <-> DCB
            -1.0 * x.[6] * x.[28] // D + CB | ligation: D + CB <-> DCB
            1.0 * x.[283] // DCA | ligation: D + CA <-> DCA
            -1.0 * x.[6] * x.[27] // D + CA | ligation: D + CA <-> DCA
            1.0 * x.[282] // DBd | ligation: D + Bd <-> DBd
            -1.0 * x.[6] * x.[26] // D + Bd | ligation: D + Bd <-> DBd
            1.0 * x.[281] // DBc | ligation: D + Bc <-> DBc
            -1.0 * x.[6] * x.[25] // D + Bc | ligation: D + Bc <-> DBc
            1.0 * x.[280] // DBb | ligation: D + Bb <-> DBb
            -1.0 * x.[6] * x.[24] // D + Bb | ligation: D + Bb <-> DBb
            1.0 * x.[279] // DBa | ligation: D + Ba <-> DBa
            -1.0 * x.[6] * x.[23] // D + Ba | ligation: D + Ba <-> DBa
            1.0 * x.[278] // DBD | ligation: D + BD <-> DBD
            -1.0 * x.[6] * x.[22] // D + BD | ligation: D + BD <-> DBD
            1.0 * x.[277] // DBC | ligation: D + BC <-> DBC
            -1.0 * x.[6] * x.[21] // D + BC | ligation: D + BC <-> DBC
            1.0 * x.[276] // DBB | ligation: D + BB <-> DBB
            -1.0 * x.[6] * x.[20] // D + BB | ligation: D + BB <-> DBB
            1.0 * x.[275] // DBA | ligation: D + BA <-> DBA
            -1.0 * x.[6] * x.[19] // D + BA | ligation: D + BA <-> DBA
            1.0 * x.[274] // DAd | ligation: D + Ad <-> DAd
            -1.0 * x.[6] * x.[18] // D + Ad | ligation: D + Ad <-> DAd
            1.0 * x.[273] // DAc | ligation: D + Ac <-> DAc
            -1.0 * x.[6] * x.[17] // D + Ac | ligation: D + Ac <-> DAc
            1.0 * x.[272] // DAb | ligation: D + Ab <-> DAb
            -1.0 * x.[6] * x.[16] // D + Ab | ligation: D + Ab <-> DAb
            1.0 * x.[271] // DAa | ligation: D + Aa <-> DAa
            -1.0 * x.[6] * x.[15] // D + Aa | ligation: D + Aa <-> DAa
            1.0 * x.[270] // DAD | ligation: D + AD <-> DAD
            -1.0 * x.[6] * x.[14] // D + AD | ligation: D + AD <-> DAD
            1.0 * x.[269] // DAC | ligation: D + AC <-> DAC
            -1.0 * x.[6] * x.[13] // D + AC | ligation: D + AC <-> DAC
            1.0 * x.[268] // DAB | ligation: D + AB <-> DAB
            -1.0 * x.[6] * x.[12] // D + AB | ligation: D + AB <-> DAB
            1.0 * x.[267] // DAA | ligation: D + AA <-> DAA
            -1.0 * x.[6] * x.[11] // D + AA | ligation: D + AA <-> DAA
            1.0 * x.[70] // dD | ligation: d + D <-> dD
            -1.0 * x.[10] * x.[6] // d + D | ligation: d + D <-> dD
            1.0 * x.[42] // Dd | ligation: D + d <-> Dd
            -1.0 * x.[6] * x.[10] // D + d | ligation: D + d <-> Dd
            1.0 * x.[41] // Dc | ligation: D + c <-> Dc
            -1.0 * x.[6] * x.[9] // D + c | ligation: D + c <-> Dc
            1.0 * x.[40] // Db | ligation: D + b <-> Db
            -1.0 * x.[6] * x.[8] // D + b | ligation: D + b <-> Db
            1.0 * x.[39] // Da | ligation: D + a <-> Da
            -1.0 * x.[6] * x.[7] // D + a | ligation: D + a <-> Da
            1.0 * x.[38] // DD | ligation: D + D <-> DD
            1.0 * x.[38] // DD | ligation: D + D <-> DD
            -1.0 * x.[6] * x.[6] // D + D | ligation: D + D <-> DD
            -1.0 * x.[6] * x.[6] // D + D | ligation: D + D <-> DD
            1.0 * x.[62] // cD | ligation: c + D <-> cD
            -1.0 * x.[9] * x.[6] // c + D | ligation: c + D <-> cD
            1.0 * x.[30] // CD | ligation: C + D <-> CD
            -1.0 * x.[5] * x.[6] // C + D | ligation: C + D <-> CD
            1.0 * x.[54] // bD | ligation: b + D <-> bD
            -1.0 * x.[8] * x.[6] // b + D | ligation: b + D <-> bD
            1.0 * x.[22] // BD | ligation: B + D <-> BD
            -1.0 * x.[4] * x.[6] // B + D | ligation: B + D <-> BD
            1.0 * x.[46] // aD | ligation: a + D <-> aD
            -1.0 * x.[7] * x.[6] // a + D | ligation: a + D <-> aD
            1.0 * x.[14] // AD | ligation: A + D <-> AD
            -1.0 * x.[3] * x.[6] // A + D | ligation: A + D <-> AD
            0.001 * x.[2] // Z | destruction: D <-> Z
            -0.001 * x.[6] // D | destruction: D <-> Z
            -0.001 * x.[6] // D | synthesis: Y <-> D
            0.001 * x.[1] // Y | synthesis: Y <-> D
        |]
        |> Array.sum


    // 7 - a
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[39] // Da | ligation: D + a <-> Da
            -1.0 * x.[6] * x.[7] // D + a | ligation: D + a <-> Da
            1.0 * x.[31] // Ca | ligation: C + a <-> Ca
            -1.0 * x.[5] * x.[7] // C + a | ligation: C + a <-> Ca
            1.0 * x.[23] // Ba | ligation: B + a <-> Ba
            -1.0 * x.[4] * x.[7] // B + a | ligation: B + a <-> Ba
            1.0 * x.[358] // aDD | ligation: a + DD <-> aDD
            -1.0 * x.[7] * x.[38] // a + DD | ligation: a + DD <-> aDD
            1.0 * x.[357] // aDC | ligation: a + DC <-> aDC
            -1.0 * x.[7] * x.[37] // a + DC | ligation: a + DC <-> aDC
            1.0 * x.[356] // aDB | ligation: a + DB <-> aDB
            -1.0 * x.[7] * x.[36] // a + DB | ligation: a + DB <-> aDB
            1.0 * x.[355] // aDA | ligation: a + DA <-> aDA
            -1.0 * x.[7] * x.[35] // a + DA | ligation: a + DA <-> aDA
            1.0 * x.[362] // aDd | ligation: a + Dd <-> aDd
            -1.0 * x.[7] * x.[42] // a + Dd | ligation: a + Dd <-> aDd
            1.0 * x.[361] // aDc | ligation: a + Dc <-> aDc
            -1.0 * x.[7] * x.[41] // a + Dc | ligation: a + Dc <-> aDc
            1.0 * x.[360] // aDb | ligation: a + Db <-> aDb
            -1.0 * x.[7] * x.[40] // a + Db | ligation: a + Db <-> aDb
            1.0 * x.[359] // aDa | ligation: a + Da <-> aDa
            -1.0 * x.[7] * x.[39] // a + Da | ligation: a + Da <-> aDa
            1.0 * x.[350] // aCD | ligation: a + CD <-> aCD
            -1.0 * x.[7] * x.[30] // a + CD | ligation: a + CD <-> aCD
            1.0 * x.[349] // aCC | ligation: a + CC <-> aCC
            -1.0 * x.[7] * x.[29] // a + CC | ligation: a + CC <-> aCC
            1.0 * x.[348] // aCB | ligation: a + CB <-> aCB
            -1.0 * x.[7] * x.[28] // a + CB | ligation: a + CB <-> aCB
            1.0 * x.[347] // aCA | ligation: a + CA <-> aCA
            -1.0 * x.[7] * x.[27] // a + CA | ligation: a + CA <-> aCA
            1.0 * x.[354] // aCd | ligation: a + Cd <-> aCd
            -1.0 * x.[7] * x.[34] // a + Cd | ligation: a + Cd <-> aCd
            1.0 * x.[353] // aCc | ligation: a + Cc <-> aCc
            -1.0 * x.[7] * x.[33] // a + Cc | ligation: a + Cc <-> aCc
            1.0 * x.[352] // aCb | ligation: a + Cb <-> aCb
            -1.0 * x.[7] * x.[32] // a + Cb | ligation: a + Cb <-> aCb
            1.0 * x.[351] // aCa | ligation: a + Ca <-> aCa
            -1.0 * x.[7] * x.[31] // a + Ca | ligation: a + Ca <-> aCa
            1.0 * x.[342] // aBD | ligation: a + BD <-> aBD
            -1.0 * x.[7] * x.[22] // a + BD | ligation: a + BD <-> aBD
            1.0 * x.[341] // aBC | ligation: a + BC <-> aBC
            -1.0 * x.[7] * x.[21] // a + BC | ligation: a + BC <-> aBC
            1.0 * x.[340] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[7] * x.[20] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[339] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[7] * x.[19] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[346] // aBd | ligation: a + Bd <-> aBd
            -1.0 * x.[7] * x.[26] // a + Bd | ligation: a + Bd <-> aBd
            1.0 * x.[345] // aBc | ligation: a + Bc <-> aBc
            -1.0 * x.[7] * x.[25] // a + Bc | ligation: a + Bc <-> aBc
            1.0 * x.[344] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[7] * x.[24] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[343] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[7] * x.[23] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[334] // aAD | ligation: a + AD <-> aAD
            -1.0 * x.[7] * x.[14] // a + AD | ligation: a + AD <-> aAD
            1.0 * x.[333] // aAC | ligation: a + AC <-> aAC
            -1.0 * x.[7] * x.[13] // a + AC | ligation: a + AC <-> aAC
            1.0 * x.[332] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[7] * x.[12] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[331] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[7] * x.[11] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[338] // aAd | ligation: a + Ad <-> aAd
            -1.0 * x.[7] * x.[18] // a + Ad | ligation: a + Ad <-> aAd
            1.0 * x.[337] // aAc | ligation: a + Ac <-> aAc
            -1.0 * x.[7] * x.[17] // a + Ac | ligation: a + Ac <-> aAc
            1.0 * x.[336] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[7] * x.[16] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[335] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[7] * x.[15] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[390] // adD | ligation: a + dD <-> adD
            -1.0 * x.[7] * x.[70] // a + dD | ligation: a + dD <-> adD
            1.0 * x.[389] // adC | ligation: a + dC <-> adC
            -1.0 * x.[7] * x.[69] // a + dC | ligation: a + dC <-> adC
            1.0 * x.[388] // adB | ligation: a + dB <-> adB
            -1.0 * x.[7] * x.[68] // a + dB | ligation: a + dB <-> adB
            1.0 * x.[387] // adA | ligation: a + dA <-> adA
            -1.0 * x.[7] * x.[67] // a + dA | ligation: a + dA <-> adA
            1.0 * x.[394] // add | ligation: a + dd <-> add
            -1.0 * x.[7] * x.[74] // a + dd | ligation: a + dd <-> add
            1.0 * x.[393] // adc | ligation: a + dc <-> adc
            -1.0 * x.[7] * x.[73] // a + dc | ligation: a + dc <-> adc
            1.0 * x.[392] // adb | ligation: a + db <-> adb
            -1.0 * x.[7] * x.[72] // a + db | ligation: a + db <-> adb
            1.0 * x.[391] // ada | ligation: a + da <-> ada
            -1.0 * x.[7] * x.[71] // a + da | ligation: a + da <-> ada
            1.0 * x.[382] // acD | ligation: a + cD <-> acD
            -1.0 * x.[7] * x.[62] // a + cD | ligation: a + cD <-> acD
            1.0 * x.[381] // acC | ligation: a + cC <-> acC
            -1.0 * x.[7] * x.[61] // a + cC | ligation: a + cC <-> acC
            1.0 * x.[380] // acB | ligation: a + cB <-> acB
            -1.0 * x.[7] * x.[60] // a + cB | ligation: a + cB <-> acB
            1.0 * x.[379] // acA | ligation: a + cA <-> acA
            -1.0 * x.[7] * x.[59] // a + cA | ligation: a + cA <-> acA
            1.0 * x.[386] // acd | ligation: a + cd <-> acd
            -1.0 * x.[7] * x.[66] // a + cd | ligation: a + cd <-> acd
            1.0 * x.[385] // acc | ligation: a + cc <-> acc
            -1.0 * x.[7] * x.[65] // a + cc | ligation: a + cc <-> acc
            1.0 * x.[384] // acb | ligation: a + cb <-> acb
            -1.0 * x.[7] * x.[64] // a + cb | ligation: a + cb <-> acb
            1.0 * x.[383] // aca | ligation: a + ca <-> aca
            -1.0 * x.[7] * x.[63] // a + ca | ligation: a + ca <-> aca
            1.0 * x.[374] // abD | ligation: a + bD <-> abD
            -1.0 * x.[7] * x.[54] // a + bD | ligation: a + bD <-> abD
            1.0 * x.[373] // abC | ligation: a + bC <-> abC
            -1.0 * x.[7] * x.[53] // a + bC | ligation: a + bC <-> abC
            1.0 * x.[372] // abB | ligation: a + bB <-> abB
            -1.0 * x.[7] * x.[52] // a + bB | ligation: a + bB <-> abB
            1.0 * x.[371] // abA | ligation: a + bA <-> abA
            -1.0 * x.[7] * x.[51] // a + bA | ligation: a + bA <-> abA
            1.0 * x.[378] // abd | ligation: a + bd <-> abd
            -1.0 * x.[7] * x.[58] // a + bd | ligation: a + bd <-> abd
            1.0 * x.[377] // abc | ligation: a + bc <-> abc
            -1.0 * x.[7] * x.[57] // a + bc | ligation: a + bc <-> abc
            1.0 * x.[376] // abb | ligation: a + bb <-> abb
            -1.0 * x.[7] * x.[56] // a + bb | ligation: a + bb <-> abb
            1.0 * x.[375] // aba | ligation: a + ba <-> aba
            -1.0 * x.[7] * x.[55] // a + ba | ligation: a + ba <-> aba
            1.0 * x.[366] // aaD | ligation: a + aD <-> aaD
            -1.0 * x.[7] * x.[46] // a + aD | ligation: a + aD <-> aaD
            1.0 * x.[365] // aaC | ligation: a + aC <-> aaC
            -1.0 * x.[7] * x.[45] // a + aC | ligation: a + aC <-> aaC
            1.0 * x.[364] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[7] * x.[44] // a + aB | ligation: a + aB <-> aaB
            1.0 * x.[363] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[7] * x.[43] // a + aA | ligation: a + aA <-> aaA
            1.0 * x.[370] // aad | ligation: a + ad <-> aad
            -1.0 * x.[7] * x.[50] // a + ad | ligation: a + ad <-> aad
            1.0 * x.[369] // aac | ligation: a + ac <-> aac
            -1.0 * x.[7] * x.[49] // a + ac | ligation: a + ac <-> aac
            1.0 * x.[368] // aab | ligation: a + ab <-> aab
            -1.0 * x.[7] * x.[48] // a + ab | ligation: a + ab <-> aab
            1.0 * x.[367] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[7] * x.[47] // a + aa | ligation: a + aa <-> aaa
            1.0 * x.[46] // aD | ligation: a + D <-> aD
            -1.0 * x.[7] * x.[6] // a + D | ligation: a + D <-> aD
            1.0 * x.[45] // aC | ligation: a + C <-> aC
            -1.0 * x.[7] * x.[5] // a + C | ligation: a + C <-> aC
            1.0 * x.[44] // aB | ligation: a + B <-> aB
            -1.0 * x.[7] * x.[4] // a + B | ligation: a + B <-> aB
            1.0 * x.[43] // aA | ligation: a + A <-> aA
            -1.0 * x.[7] * x.[3] // a + A | ligation: a + A <-> aA
            1.0 * x.[15] // Aa | ligation: A + a <-> Aa
            -1.0 * x.[3] * x.[7] // A + a | ligation: A + a <-> Aa
            1.0 * x.[50] // ad | ligation: a + d <-> ad
            -1.0 * x.[7] * x.[10] // a + d | ligation: a + d <-> ad
            1.0 * x.[49] // ac | ligation: a + c <-> ac
            -1.0 * x.[7] * x.[9] // a + c | ligation: a + c <-> ac
            1.0 * x.[48] // ab | ligation: a + b <-> ab
            -1.0 * x.[7] * x.[8] // a + b | ligation: a + b <-> ab
            1.0 * x.[47] // aa | ligation: a + a <-> aa
            1.0 * x.[47] // aa | ligation: a + a <-> aa
            -1.0 * x.[7] * x.[7] // a + a | ligation: a + a <-> aa
            -1.0 * x.[7] * x.[7] // a + a | ligation: a + a <-> aa
            0.001 * x.[2] // Z | destruction: a <-> Z
            -0.001 * x.[7] // a | destruction: a <-> Z
            -0.001 * x.[7] // a | synthesis: Y <-> a
            0.001 * x.[1] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 8 - b
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[40] // Db | ligation: D + b <-> Db
            -1.0 * x.[6] * x.[8] // D + b | ligation: D + b <-> Db
            1.0 * x.[32] // Cb | ligation: C + b <-> Cb
            -1.0 * x.[5] * x.[8] // C + b | ligation: C + b <-> Cb
            1.0 * x.[422] // bDD | ligation: b + DD <-> bDD
            -1.0 * x.[8] * x.[38] // b + DD | ligation: b + DD <-> bDD
            1.0 * x.[421] // bDC | ligation: b + DC <-> bDC
            -1.0 * x.[8] * x.[37] // b + DC | ligation: b + DC <-> bDC
            1.0 * x.[420] // bDB | ligation: b + DB <-> bDB
            -1.0 * x.[8] * x.[36] // b + DB | ligation: b + DB <-> bDB
            1.0 * x.[419] // bDA | ligation: b + DA <-> bDA
            -1.0 * x.[8] * x.[35] // b + DA | ligation: b + DA <-> bDA
            1.0 * x.[426] // bDd | ligation: b + Dd <-> bDd
            -1.0 * x.[8] * x.[42] // b + Dd | ligation: b + Dd <-> bDd
            1.0 * x.[425] // bDc | ligation: b + Dc <-> bDc
            -1.0 * x.[8] * x.[41] // b + Dc | ligation: b + Dc <-> bDc
            1.0 * x.[424] // bDb | ligation: b + Db <-> bDb
            -1.0 * x.[8] * x.[40] // b + Db | ligation: b + Db <-> bDb
            1.0 * x.[423] // bDa | ligation: b + Da <-> bDa
            -1.0 * x.[8] * x.[39] // b + Da | ligation: b + Da <-> bDa
            1.0 * x.[414] // bCD | ligation: b + CD <-> bCD
            -1.0 * x.[8] * x.[30] // b + CD | ligation: b + CD <-> bCD
            1.0 * x.[413] // bCC | ligation: b + CC <-> bCC
            -1.0 * x.[8] * x.[29] // b + CC | ligation: b + CC <-> bCC
            1.0 * x.[412] // bCB | ligation: b + CB <-> bCB
            -1.0 * x.[8] * x.[28] // b + CB | ligation: b + CB <-> bCB
            1.0 * x.[411] // bCA | ligation: b + CA <-> bCA
            -1.0 * x.[8] * x.[27] // b + CA | ligation: b + CA <-> bCA
            1.0 * x.[418] // bCd | ligation: b + Cd <-> bCd
            -1.0 * x.[8] * x.[34] // b + Cd | ligation: b + Cd <-> bCd
            1.0 * x.[417] // bCc | ligation: b + Cc <-> bCc
            -1.0 * x.[8] * x.[33] // b + Cc | ligation: b + Cc <-> bCc
            1.0 * x.[416] // bCb | ligation: b + Cb <-> bCb
            -1.0 * x.[8] * x.[32] // b + Cb | ligation: b + Cb <-> bCb
            1.0 * x.[415] // bCa | ligation: b + Ca <-> bCa
            -1.0 * x.[8] * x.[31] // b + Ca | ligation: b + Ca <-> bCa
            1.0 * x.[406] // bBD | ligation: b + BD <-> bBD
            -1.0 * x.[8] * x.[22] // b + BD | ligation: b + BD <-> bBD
            1.0 * x.[405] // bBC | ligation: b + BC <-> bBC
            -1.0 * x.[8] * x.[21] // b + BC | ligation: b + BC <-> bBC
            1.0 * x.[404] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[8] * x.[20] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[403] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[8] * x.[19] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[410] // bBd | ligation: b + Bd <-> bBd
            -1.0 * x.[8] * x.[26] // b + Bd | ligation: b + Bd <-> bBd
            1.0 * x.[409] // bBc | ligation: b + Bc <-> bBc
            -1.0 * x.[8] * x.[25] // b + Bc | ligation: b + Bc <-> bBc
            1.0 * x.[408] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[8] * x.[24] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[407] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[8] * x.[23] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[398] // bAD | ligation: b + AD <-> bAD
            -1.0 * x.[8] * x.[14] // b + AD | ligation: b + AD <-> bAD
            1.0 * x.[397] // bAC | ligation: b + AC <-> bAC
            -1.0 * x.[8] * x.[13] // b + AC | ligation: b + AC <-> bAC
            1.0 * x.[396] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[8] * x.[12] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[395] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[8] * x.[11] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[402] // bAd | ligation: b + Ad <-> bAd
            -1.0 * x.[8] * x.[18] // b + Ad | ligation: b + Ad <-> bAd
            1.0 * x.[401] // bAc | ligation: b + Ac <-> bAc
            -1.0 * x.[8] * x.[17] // b + Ac | ligation: b + Ac <-> bAc
            1.0 * x.[400] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[8] * x.[16] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[399] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[8] * x.[15] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[454] // bdD | ligation: b + dD <-> bdD
            -1.0 * x.[8] * x.[70] // b + dD | ligation: b + dD <-> bdD
            1.0 * x.[453] // bdC | ligation: b + dC <-> bdC
            -1.0 * x.[8] * x.[69] // b + dC | ligation: b + dC <-> bdC
            1.0 * x.[452] // bdB | ligation: b + dB <-> bdB
            -1.0 * x.[8] * x.[68] // b + dB | ligation: b + dB <-> bdB
            1.0 * x.[451] // bdA | ligation: b + dA <-> bdA
            -1.0 * x.[8] * x.[67] // b + dA | ligation: b + dA <-> bdA
            1.0 * x.[458] // bdd | ligation: b + dd <-> bdd
            -1.0 * x.[8] * x.[74] // b + dd | ligation: b + dd <-> bdd
            1.0 * x.[457] // bdc | ligation: b + dc <-> bdc
            -1.0 * x.[8] * x.[73] // b + dc | ligation: b + dc <-> bdc
            1.0 * x.[456] // bdb | ligation: b + db <-> bdb
            -1.0 * x.[8] * x.[72] // b + db | ligation: b + db <-> bdb
            1.0 * x.[455] // bda | ligation: b + da <-> bda
            -1.0 * x.[8] * x.[71] // b + da | ligation: b + da <-> bda
            1.0 * x.[446] // bcD | ligation: b + cD <-> bcD
            -1.0 * x.[8] * x.[62] // b + cD | ligation: b + cD <-> bcD
            1.0 * x.[445] // bcC | ligation: b + cC <-> bcC
            -1.0 * x.[8] * x.[61] // b + cC | ligation: b + cC <-> bcC
            1.0 * x.[444] // bcB | ligation: b + cB <-> bcB
            -1.0 * x.[8] * x.[60] // b + cB | ligation: b + cB <-> bcB
            1.0 * x.[443] // bcA | ligation: b + cA <-> bcA
            -1.0 * x.[8] * x.[59] // b + cA | ligation: b + cA <-> bcA
            1.0 * x.[450] // bcd | ligation: b + cd <-> bcd
            -1.0 * x.[8] * x.[66] // b + cd | ligation: b + cd <-> bcd
            1.0 * x.[449] // bcc | ligation: b + cc <-> bcc
            -1.0 * x.[8] * x.[65] // b + cc | ligation: b + cc <-> bcc
            1.0 * x.[448] // bcb | ligation: b + cb <-> bcb
            -1.0 * x.[8] * x.[64] // b + cb | ligation: b + cb <-> bcb
            1.0 * x.[447] // bca | ligation: b + ca <-> bca
            -1.0 * x.[8] * x.[63] // b + ca | ligation: b + ca <-> bca
            1.0 * x.[438] // bbD | ligation: b + bD <-> bbD
            -1.0 * x.[8] * x.[54] // b + bD | ligation: b + bD <-> bbD
            1.0 * x.[437] // bbC | ligation: b + bC <-> bbC
            -1.0 * x.[8] * x.[53] // b + bC | ligation: b + bC <-> bbC
            1.0 * x.[436] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[8] * x.[52] // b + bB | ligation: b + bB <-> bbB
            1.0 * x.[435] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[8] * x.[51] // b + bA | ligation: b + bA <-> bbA
            1.0 * x.[442] // bbd | ligation: b + bd <-> bbd
            -1.0 * x.[8] * x.[58] // b + bd | ligation: b + bd <-> bbd
            1.0 * x.[441] // bbc | ligation: b + bc <-> bbc
            -1.0 * x.[8] * x.[57] // b + bc | ligation: b + bc <-> bbc
            1.0 * x.[440] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[8] * x.[56] // b + bb | ligation: b + bb <-> bbb
            1.0 * x.[439] // bba | ligation: b + ba <-> bba
            -1.0 * x.[8] * x.[55] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[430] // baD | ligation: b + aD <-> baD
            -1.0 * x.[8] * x.[46] // b + aD | ligation: b + aD <-> baD
            1.0 * x.[429] // baC | ligation: b + aC <-> baC
            -1.0 * x.[8] * x.[45] // b + aC | ligation: b + aC <-> baC
            1.0 * x.[428] // baB | ligation: b + aB <-> baB
            -1.0 * x.[8] * x.[44] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[427] // baA | ligation: b + aA <-> baA
            -1.0 * x.[8] * x.[43] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[434] // bad | ligation: b + ad <-> bad
            -1.0 * x.[8] * x.[50] // b + ad | ligation: b + ad <-> bad
            1.0 * x.[433] // bac | ligation: b + ac <-> bac
            -1.0 * x.[8] * x.[49] // b + ac | ligation: b + ac <-> bac
            1.0 * x.[432] // bab | ligation: b + ab <-> bab
            -1.0 * x.[8] * x.[48] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[431] // baa | ligation: b + aa <-> baa
            -1.0 * x.[8] * x.[47] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[54] // bD | ligation: b + D <-> bD
            -1.0 * x.[8] * x.[6] // b + D | ligation: b + D <-> bD
            1.0 * x.[53] // bC | ligation: b + C <-> bC
            -1.0 * x.[8] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[52] // bB | ligation: b + B <-> bB
            -1.0 * x.[8] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[24] // Bb | ligation: B + b <-> Bb
            -1.0 * x.[4] * x.[8] // B + b | ligation: B + b <-> Bb
            1.0 * x.[51] // bA | ligation: b + A <-> bA
            -1.0 * x.[8] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[58] // bd | ligation: b + d <-> bd
            -1.0 * x.[8] * x.[10] // b + d | ligation: b + d <-> bd
            1.0 * x.[57] // bc | ligation: b + c <-> bc
            -1.0 * x.[8] * x.[9] // b + c | ligation: b + c <-> bc
            1.0 * x.[56] // bb | ligation: b + b <-> bb
            1.0 * x.[56] // bb | ligation: b + b <-> bb
            -1.0 * x.[8] * x.[8] // b + b | ligation: b + b <-> bb
            -1.0 * x.[8] * x.[8] // b + b | ligation: b + b <-> bb
            1.0 * x.[16] // Ab | ligation: A + b <-> Ab
            -1.0 * x.[3] * x.[8] // A + b | ligation: A + b <-> Ab
            1.0 * x.[48] // ab | ligation: a + b <-> ab
            -1.0 * x.[7] * x.[8] // a + b | ligation: a + b <-> ab
            0.001 * x.[2] // Z | destruction: b <-> Z
            -0.001 * x.[8] // b | destruction: b <-> Z
            -0.001 * x.[8] // b | synthesis: Y <-> b
            0.001 * x.[1] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 9 - c
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[41] // Dc | ligation: D + c <-> Dc
            -1.0 * x.[6] * x.[9] // D + c | ligation: D + c <-> Dc
            1.0 * x.[486] // cDD | ligation: c + DD <-> cDD
            -1.0 * x.[9] * x.[38] // c + DD | ligation: c + DD <-> cDD
            1.0 * x.[485] // cDC | ligation: c + DC <-> cDC
            -1.0 * x.[9] * x.[37] // c + DC | ligation: c + DC <-> cDC
            1.0 * x.[484] // cDB | ligation: c + DB <-> cDB
            -1.0 * x.[9] * x.[36] // c + DB | ligation: c + DB <-> cDB
            1.0 * x.[483] // cDA | ligation: c + DA <-> cDA
            -1.0 * x.[9] * x.[35] // c + DA | ligation: c + DA <-> cDA
            1.0 * x.[490] // cDd | ligation: c + Dd <-> cDd
            -1.0 * x.[9] * x.[42] // c + Dd | ligation: c + Dd <-> cDd
            1.0 * x.[489] // cDc | ligation: c + Dc <-> cDc
            -1.0 * x.[9] * x.[41] // c + Dc | ligation: c + Dc <-> cDc
            1.0 * x.[488] // cDb | ligation: c + Db <-> cDb
            -1.0 * x.[9] * x.[40] // c + Db | ligation: c + Db <-> cDb
            1.0 * x.[487] // cDa | ligation: c + Da <-> cDa
            -1.0 * x.[9] * x.[39] // c + Da | ligation: c + Da <-> cDa
            1.0 * x.[478] // cCD | ligation: c + CD <-> cCD
            -1.0 * x.[9] * x.[30] // c + CD | ligation: c + CD <-> cCD
            1.0 * x.[477] // cCC | ligation: c + CC <-> cCC
            -1.0 * x.[9] * x.[29] // c + CC | ligation: c + CC <-> cCC
            1.0 * x.[476] // cCB | ligation: c + CB <-> cCB
            -1.0 * x.[9] * x.[28] // c + CB | ligation: c + CB <-> cCB
            1.0 * x.[475] // cCA | ligation: c + CA <-> cCA
            -1.0 * x.[9] * x.[27] // c + CA | ligation: c + CA <-> cCA
            1.0 * x.[482] // cCd | ligation: c + Cd <-> cCd
            -1.0 * x.[9] * x.[34] // c + Cd | ligation: c + Cd <-> cCd
            1.0 * x.[481] // cCc | ligation: c + Cc <-> cCc
            -1.0 * x.[9] * x.[33] // c + Cc | ligation: c + Cc <-> cCc
            1.0 * x.[480] // cCb | ligation: c + Cb <-> cCb
            -1.0 * x.[9] * x.[32] // c + Cb | ligation: c + Cb <-> cCb
            1.0 * x.[479] // cCa | ligation: c + Ca <-> cCa
            -1.0 * x.[9] * x.[31] // c + Ca | ligation: c + Ca <-> cCa
            1.0 * x.[470] // cBD | ligation: c + BD <-> cBD
            -1.0 * x.[9] * x.[22] // c + BD | ligation: c + BD <-> cBD
            1.0 * x.[469] // cBC | ligation: c + BC <-> cBC
            -1.0 * x.[9] * x.[21] // c + BC | ligation: c + BC <-> cBC
            1.0 * x.[468] // cBB | ligation: c + BB <-> cBB
            -1.0 * x.[9] * x.[20] // c + BB | ligation: c + BB <-> cBB
            1.0 * x.[467] // cBA | ligation: c + BA <-> cBA
            -1.0 * x.[9] * x.[19] // c + BA | ligation: c + BA <-> cBA
            1.0 * x.[474] // cBd | ligation: c + Bd <-> cBd
            -1.0 * x.[9] * x.[26] // c + Bd | ligation: c + Bd <-> cBd
            1.0 * x.[473] // cBc | ligation: c + Bc <-> cBc
            -1.0 * x.[9] * x.[25] // c + Bc | ligation: c + Bc <-> cBc
            1.0 * x.[472] // cBb | ligation: c + Bb <-> cBb
            -1.0 * x.[9] * x.[24] // c + Bb | ligation: c + Bb <-> cBb
            1.0 * x.[471] // cBa | ligation: c + Ba <-> cBa
            -1.0 * x.[9] * x.[23] // c + Ba | ligation: c + Ba <-> cBa
            1.0 * x.[462] // cAD | ligation: c + AD <-> cAD
            -1.0 * x.[9] * x.[14] // c + AD | ligation: c + AD <-> cAD
            1.0 * x.[461] // cAC | ligation: c + AC <-> cAC
            -1.0 * x.[9] * x.[13] // c + AC | ligation: c + AC <-> cAC
            1.0 * x.[460] // cAB | ligation: c + AB <-> cAB
            -1.0 * x.[9] * x.[12] // c + AB | ligation: c + AB <-> cAB
            1.0 * x.[459] // cAA | ligation: c + AA <-> cAA
            -1.0 * x.[9] * x.[11] // c + AA | ligation: c + AA <-> cAA
            1.0 * x.[466] // cAd | ligation: c + Ad <-> cAd
            -1.0 * x.[9] * x.[18] // c + Ad | ligation: c + Ad <-> cAd
            1.0 * x.[465] // cAc | ligation: c + Ac <-> cAc
            -1.0 * x.[9] * x.[17] // c + Ac | ligation: c + Ac <-> cAc
            1.0 * x.[464] // cAb | ligation: c + Ab <-> cAb
            -1.0 * x.[9] * x.[16] // c + Ab | ligation: c + Ab <-> cAb
            1.0 * x.[463] // cAa | ligation: c + Aa <-> cAa
            -1.0 * x.[9] * x.[15] // c + Aa | ligation: c + Aa <-> cAa
            1.0 * x.[518] // cdD | ligation: c + dD <-> cdD
            -1.0 * x.[9] * x.[70] // c + dD | ligation: c + dD <-> cdD
            1.0 * x.[517] // cdC | ligation: c + dC <-> cdC
            -1.0 * x.[9] * x.[69] // c + dC | ligation: c + dC <-> cdC
            1.0 * x.[516] // cdB | ligation: c + dB <-> cdB
            -1.0 * x.[9] * x.[68] // c + dB | ligation: c + dB <-> cdB
            1.0 * x.[515] // cdA | ligation: c + dA <-> cdA
            -1.0 * x.[9] * x.[67] // c + dA | ligation: c + dA <-> cdA
            1.0 * x.[522] // cdd | ligation: c + dd <-> cdd
            -1.0 * x.[9] * x.[74] // c + dd | ligation: c + dd <-> cdd
            1.0 * x.[521] // cdc | ligation: c + dc <-> cdc
            -1.0 * x.[9] * x.[73] // c + dc | ligation: c + dc <-> cdc
            1.0 * x.[520] // cdb | ligation: c + db <-> cdb
            -1.0 * x.[9] * x.[72] // c + db | ligation: c + db <-> cdb
            1.0 * x.[519] // cda | ligation: c + da <-> cda
            -1.0 * x.[9] * x.[71] // c + da | ligation: c + da <-> cda
            1.0 * x.[510] // ccD | ligation: c + cD <-> ccD
            -1.0 * x.[9] * x.[62] // c + cD | ligation: c + cD <-> ccD
            1.0 * x.[509] // ccC | ligation: c + cC <-> ccC
            -1.0 * x.[9] * x.[61] // c + cC | ligation: c + cC <-> ccC
            1.0 * x.[508] // ccB | ligation: c + cB <-> ccB
            -1.0 * x.[9] * x.[60] // c + cB | ligation: c + cB <-> ccB
            1.0 * x.[507] // ccA | ligation: c + cA <-> ccA
            -1.0 * x.[9] * x.[59] // c + cA | ligation: c + cA <-> ccA
            1.0 * x.[514] // ccd | ligation: c + cd <-> ccd
            -1.0 * x.[9] * x.[66] // c + cd | ligation: c + cd <-> ccd
            1.0 * x.[513] // ccc | ligation: c + cc <-> ccc
            -1.0 * x.[9] * x.[65] // c + cc | ligation: c + cc <-> ccc
            1.0 * x.[512] // ccb | ligation: c + cb <-> ccb
            -1.0 * x.[9] * x.[64] // c + cb | ligation: c + cb <-> ccb
            1.0 * x.[511] // cca | ligation: c + ca <-> cca
            -1.0 * x.[9] * x.[63] // c + ca | ligation: c + ca <-> cca
            1.0 * x.[502] // cbD | ligation: c + bD <-> cbD
            -1.0 * x.[9] * x.[54] // c + bD | ligation: c + bD <-> cbD
            1.0 * x.[501] // cbC | ligation: c + bC <-> cbC
            -1.0 * x.[9] * x.[53] // c + bC | ligation: c + bC <-> cbC
            1.0 * x.[500] // cbB | ligation: c + bB <-> cbB
            -1.0 * x.[9] * x.[52] // c + bB | ligation: c + bB <-> cbB
            1.0 * x.[499] // cbA | ligation: c + bA <-> cbA
            -1.0 * x.[9] * x.[51] // c + bA | ligation: c + bA <-> cbA
            1.0 * x.[506] // cbd | ligation: c + bd <-> cbd
            -1.0 * x.[9] * x.[58] // c + bd | ligation: c + bd <-> cbd
            1.0 * x.[505] // cbc | ligation: c + bc <-> cbc
            -1.0 * x.[9] * x.[57] // c + bc | ligation: c + bc <-> cbc
            1.0 * x.[504] // cbb | ligation: c + bb <-> cbb
            -1.0 * x.[9] * x.[56] // c + bb | ligation: c + bb <-> cbb
            1.0 * x.[503] // cba | ligation: c + ba <-> cba
            -1.0 * x.[9] * x.[55] // c + ba | ligation: c + ba <-> cba
            1.0 * x.[494] // caD | ligation: c + aD <-> caD
            -1.0 * x.[9] * x.[46] // c + aD | ligation: c + aD <-> caD
            1.0 * x.[493] // caC | ligation: c + aC <-> caC
            -1.0 * x.[9] * x.[45] // c + aC | ligation: c + aC <-> caC
            1.0 * x.[492] // caB | ligation: c + aB <-> caB
            -1.0 * x.[9] * x.[44] // c + aB | ligation: c + aB <-> caB
            1.0 * x.[491] // caA | ligation: c + aA <-> caA
            -1.0 * x.[9] * x.[43] // c + aA | ligation: c + aA <-> caA
            1.0 * x.[498] // cad | ligation: c + ad <-> cad
            -1.0 * x.[9] * x.[50] // c + ad | ligation: c + ad <-> cad
            1.0 * x.[497] // cac | ligation: c + ac <-> cac
            -1.0 * x.[9] * x.[49] // c + ac | ligation: c + ac <-> cac
            1.0 * x.[496] // cab | ligation: c + ab <-> cab
            -1.0 * x.[9] * x.[48] // c + ab | ligation: c + ab <-> cab
            1.0 * x.[495] // caa | ligation: c + aa <-> caa
            -1.0 * x.[9] * x.[47] // c + aa | ligation: c + aa <-> caa
            1.0 * x.[62] // cD | ligation: c + D <-> cD
            -1.0 * x.[9] * x.[6] // c + D | ligation: c + D <-> cD
            1.0 * x.[61] // cC | ligation: c + C <-> cC
            -1.0 * x.[9] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[33] // Cc | ligation: C + c <-> Cc
            -1.0 * x.[5] * x.[9] // C + c | ligation: C + c <-> Cc
            1.0 * x.[60] // cB | ligation: c + B <-> cB
            -1.0 * x.[9] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[59] // cA | ligation: c + A <-> cA
            -1.0 * x.[9] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[66] // cd | ligation: c + d <-> cd
            -1.0 * x.[9] * x.[10] // c + d | ligation: c + d <-> cd
            1.0 * x.[65] // cc | ligation: c + c <-> cc
            1.0 * x.[65] // cc | ligation: c + c <-> cc
            -1.0 * x.[9] * x.[9] // c + c | ligation: c + c <-> cc
            -1.0 * x.[9] * x.[9] // c + c | ligation: c + c <-> cc
            1.0 * x.[25] // Bc | ligation: B + c <-> Bc
            -1.0 * x.[4] * x.[9] // B + c | ligation: B + c <-> Bc
            1.0 * x.[57] // bc | ligation: b + c <-> bc
            -1.0 * x.[8] * x.[9] // b + c | ligation: b + c <-> bc
            1.0 * x.[17] // Ac | ligation: A + c <-> Ac
            -1.0 * x.[3] * x.[9] // A + c | ligation: A + c <-> Ac
            1.0 * x.[49] // ac | ligation: a + c <-> ac
            -1.0 * x.[7] * x.[9] // a + c | ligation: a + c <-> ac
            0.001 * x.[2] // Z | destruction: c <-> Z
            -0.001 * x.[9] // c | destruction: c <-> Z
            -0.001 * x.[9] // c | synthesis: Y <-> c
            0.001 * x.[1] // Y | synthesis: Y <-> c
        |]
        |> Array.sum


    // 10 - d
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[550] // dDD | ligation: d + DD <-> dDD
            -1.0 * x.[10] * x.[38] // d + DD | ligation: d + DD <-> dDD
            1.0 * x.[549] // dDC | ligation: d + DC <-> dDC
            -1.0 * x.[10] * x.[37] // d + DC | ligation: d + DC <-> dDC
            1.0 * x.[548] // dDB | ligation: d + DB <-> dDB
            -1.0 * x.[10] * x.[36] // d + DB | ligation: d + DB <-> dDB
            1.0 * x.[547] // dDA | ligation: d + DA <-> dDA
            -1.0 * x.[10] * x.[35] // d + DA | ligation: d + DA <-> dDA
            1.0 * x.[554] // dDd | ligation: d + Dd <-> dDd
            -1.0 * x.[10] * x.[42] // d + Dd | ligation: d + Dd <-> dDd
            1.0 * x.[553] // dDc | ligation: d + Dc <-> dDc
            -1.0 * x.[10] * x.[41] // d + Dc | ligation: d + Dc <-> dDc
            1.0 * x.[552] // dDb | ligation: d + Db <-> dDb
            -1.0 * x.[10] * x.[40] // d + Db | ligation: d + Db <-> dDb
            1.0 * x.[551] // dDa | ligation: d + Da <-> dDa
            -1.0 * x.[10] * x.[39] // d + Da | ligation: d + Da <-> dDa
            1.0 * x.[542] // dCD | ligation: d + CD <-> dCD
            -1.0 * x.[10] * x.[30] // d + CD | ligation: d + CD <-> dCD
            1.0 * x.[541] // dCC | ligation: d + CC <-> dCC
            -1.0 * x.[10] * x.[29] // d + CC | ligation: d + CC <-> dCC
            1.0 * x.[540] // dCB | ligation: d + CB <-> dCB
            -1.0 * x.[10] * x.[28] // d + CB | ligation: d + CB <-> dCB
            1.0 * x.[539] // dCA | ligation: d + CA <-> dCA
            -1.0 * x.[10] * x.[27] // d + CA | ligation: d + CA <-> dCA
            1.0 * x.[546] // dCd | ligation: d + Cd <-> dCd
            -1.0 * x.[10] * x.[34] // d + Cd | ligation: d + Cd <-> dCd
            1.0 * x.[545] // dCc | ligation: d + Cc <-> dCc
            -1.0 * x.[10] * x.[33] // d + Cc | ligation: d + Cc <-> dCc
            1.0 * x.[544] // dCb | ligation: d + Cb <-> dCb
            -1.0 * x.[10] * x.[32] // d + Cb | ligation: d + Cb <-> dCb
            1.0 * x.[543] // dCa | ligation: d + Ca <-> dCa
            -1.0 * x.[10] * x.[31] // d + Ca | ligation: d + Ca <-> dCa
            1.0 * x.[534] // dBD | ligation: d + BD <-> dBD
            -1.0 * x.[10] * x.[22] // d + BD | ligation: d + BD <-> dBD
            1.0 * x.[533] // dBC | ligation: d + BC <-> dBC
            -1.0 * x.[10] * x.[21] // d + BC | ligation: d + BC <-> dBC
            1.0 * x.[532] // dBB | ligation: d + BB <-> dBB
            -1.0 * x.[10] * x.[20] // d + BB | ligation: d + BB <-> dBB
            1.0 * x.[531] // dBA | ligation: d + BA <-> dBA
            -1.0 * x.[10] * x.[19] // d + BA | ligation: d + BA <-> dBA
            1.0 * x.[538] // dBd | ligation: d + Bd <-> dBd
            -1.0 * x.[10] * x.[26] // d + Bd | ligation: d + Bd <-> dBd
            1.0 * x.[537] // dBc | ligation: d + Bc <-> dBc
            -1.0 * x.[10] * x.[25] // d + Bc | ligation: d + Bc <-> dBc
            1.0 * x.[536] // dBb | ligation: d + Bb <-> dBb
            -1.0 * x.[10] * x.[24] // d + Bb | ligation: d + Bb <-> dBb
            1.0 * x.[535] // dBa | ligation: d + Ba <-> dBa
            -1.0 * x.[10] * x.[23] // d + Ba | ligation: d + Ba <-> dBa
            1.0 * x.[526] // dAD | ligation: d + AD <-> dAD
            -1.0 * x.[10] * x.[14] // d + AD | ligation: d + AD <-> dAD
            1.0 * x.[525] // dAC | ligation: d + AC <-> dAC
            -1.0 * x.[10] * x.[13] // d + AC | ligation: d + AC <-> dAC
            1.0 * x.[524] // dAB | ligation: d + AB <-> dAB
            -1.0 * x.[10] * x.[12] // d + AB | ligation: d + AB <-> dAB
            1.0 * x.[523] // dAA | ligation: d + AA <-> dAA
            -1.0 * x.[10] * x.[11] // d + AA | ligation: d + AA <-> dAA
            1.0 * x.[530] // dAd | ligation: d + Ad <-> dAd
            -1.0 * x.[10] * x.[18] // d + Ad | ligation: d + Ad <-> dAd
            1.0 * x.[529] // dAc | ligation: d + Ac <-> dAc
            -1.0 * x.[10] * x.[17] // d + Ac | ligation: d + Ac <-> dAc
            1.0 * x.[528] // dAb | ligation: d + Ab <-> dAb
            -1.0 * x.[10] * x.[16] // d + Ab | ligation: d + Ab <-> dAb
            1.0 * x.[527] // dAa | ligation: d + Aa <-> dAa
            -1.0 * x.[10] * x.[15] // d + Aa | ligation: d + Aa <-> dAa
            1.0 * x.[582] // ddD | ligation: d + dD <-> ddD
            -1.0 * x.[10] * x.[70] // d + dD | ligation: d + dD <-> ddD
            1.0 * x.[581] // ddC | ligation: d + dC <-> ddC
            -1.0 * x.[10] * x.[69] // d + dC | ligation: d + dC <-> ddC
            1.0 * x.[580] // ddB | ligation: d + dB <-> ddB
            -1.0 * x.[10] * x.[68] // d + dB | ligation: d + dB <-> ddB
            1.0 * x.[579] // ddA | ligation: d + dA <-> ddA
            -1.0 * x.[10] * x.[67] // d + dA | ligation: d + dA <-> ddA
            1.0 * x.[586] // ddd | ligation: d + dd <-> ddd
            -1.0 * x.[10] * x.[74] // d + dd | ligation: d + dd <-> ddd
            1.0 * x.[585] // ddc | ligation: d + dc <-> ddc
            -1.0 * x.[10] * x.[73] // d + dc | ligation: d + dc <-> ddc
            1.0 * x.[584] // ddb | ligation: d + db <-> ddb
            -1.0 * x.[10] * x.[72] // d + db | ligation: d + db <-> ddb
            1.0 * x.[583] // dda | ligation: d + da <-> dda
            -1.0 * x.[10] * x.[71] // d + da | ligation: d + da <-> dda
            1.0 * x.[574] // dcD | ligation: d + cD <-> dcD
            -1.0 * x.[10] * x.[62] // d + cD | ligation: d + cD <-> dcD
            1.0 * x.[573] // dcC | ligation: d + cC <-> dcC
            -1.0 * x.[10] * x.[61] // d + cC | ligation: d + cC <-> dcC
            1.0 * x.[572] // dcB | ligation: d + cB <-> dcB
            -1.0 * x.[10] * x.[60] // d + cB | ligation: d + cB <-> dcB
            1.0 * x.[571] // dcA | ligation: d + cA <-> dcA
            -1.0 * x.[10] * x.[59] // d + cA | ligation: d + cA <-> dcA
            1.0 * x.[578] // dcd | ligation: d + cd <-> dcd
            -1.0 * x.[10] * x.[66] // d + cd | ligation: d + cd <-> dcd
            1.0 * x.[577] // dcc | ligation: d + cc <-> dcc
            -1.0 * x.[10] * x.[65] // d + cc | ligation: d + cc <-> dcc
            1.0 * x.[576] // dcb | ligation: d + cb <-> dcb
            -1.0 * x.[10] * x.[64] // d + cb | ligation: d + cb <-> dcb
            1.0 * x.[575] // dca | ligation: d + ca <-> dca
            -1.0 * x.[10] * x.[63] // d + ca | ligation: d + ca <-> dca
            1.0 * x.[566] // dbD | ligation: d + bD <-> dbD
            -1.0 * x.[10] * x.[54] // d + bD | ligation: d + bD <-> dbD
            1.0 * x.[565] // dbC | ligation: d + bC <-> dbC
            -1.0 * x.[10] * x.[53] // d + bC | ligation: d + bC <-> dbC
            1.0 * x.[564] // dbB | ligation: d + bB <-> dbB
            -1.0 * x.[10] * x.[52] // d + bB | ligation: d + bB <-> dbB
            1.0 * x.[563] // dbA | ligation: d + bA <-> dbA
            -1.0 * x.[10] * x.[51] // d + bA | ligation: d + bA <-> dbA
            1.0 * x.[570] // dbd | ligation: d + bd <-> dbd
            -1.0 * x.[10] * x.[58] // d + bd | ligation: d + bd <-> dbd
            1.0 * x.[569] // dbc | ligation: d + bc <-> dbc
            -1.0 * x.[10] * x.[57] // d + bc | ligation: d + bc <-> dbc
            1.0 * x.[568] // dbb | ligation: d + bb <-> dbb
            -1.0 * x.[10] * x.[56] // d + bb | ligation: d + bb <-> dbb
            1.0 * x.[567] // dba | ligation: d + ba <-> dba
            -1.0 * x.[10] * x.[55] // d + ba | ligation: d + ba <-> dba
            1.0 * x.[558] // daD | ligation: d + aD <-> daD
            -1.0 * x.[10] * x.[46] // d + aD | ligation: d + aD <-> daD
            1.0 * x.[557] // daC | ligation: d + aC <-> daC
            -1.0 * x.[10] * x.[45] // d + aC | ligation: d + aC <-> daC
            1.0 * x.[556] // daB | ligation: d + aB <-> daB
            -1.0 * x.[10] * x.[44] // d + aB | ligation: d + aB <-> daB
            1.0 * x.[555] // daA | ligation: d + aA <-> daA
            -1.0 * x.[10] * x.[43] // d + aA | ligation: d + aA <-> daA
            1.0 * x.[562] // dad | ligation: d + ad <-> dad
            -1.0 * x.[10] * x.[50] // d + ad | ligation: d + ad <-> dad
            1.0 * x.[561] // dac | ligation: d + ac <-> dac
            -1.0 * x.[10] * x.[49] // d + ac | ligation: d + ac <-> dac
            1.0 * x.[560] // dab | ligation: d + ab <-> dab
            -1.0 * x.[10] * x.[48] // d + ab | ligation: d + ab <-> dab
            1.0 * x.[559] // daa | ligation: d + aa <-> daa
            -1.0 * x.[10] * x.[47] // d + aa | ligation: d + aa <-> daa
            1.0 * x.[70] // dD | ligation: d + D <-> dD
            -1.0 * x.[10] * x.[6] // d + D | ligation: d + D <-> dD
            1.0 * x.[42] // Dd | ligation: D + d <-> Dd
            -1.0 * x.[6] * x.[10] // D + d | ligation: D + d <-> Dd
            1.0 * x.[69] // dC | ligation: d + C <-> dC
            -1.0 * x.[10] * x.[5] // d + C | ligation: d + C <-> dC
            1.0 * x.[68] // dB | ligation: d + B <-> dB
            -1.0 * x.[10] * x.[4] // d + B | ligation: d + B <-> dB
            1.0 * x.[67] // dA | ligation: d + A <-> dA
            -1.0 * x.[10] * x.[3] // d + A | ligation: d + A <-> dA
            1.0 * x.[74] // dd | ligation: d + d <-> dd
            1.0 * x.[74] // dd | ligation: d + d <-> dd
            -1.0 * x.[10] * x.[10] // d + d | ligation: d + d <-> dd
            -1.0 * x.[10] * x.[10] // d + d | ligation: d + d <-> dd
            1.0 * x.[34] // Cd | ligation: C + d <-> Cd
            -1.0 * x.[5] * x.[10] // C + d | ligation: C + d <-> Cd
            1.0 * x.[66] // cd | ligation: c + d <-> cd
            -1.0 * x.[9] * x.[10] // c + d | ligation: c + d <-> cd
            1.0 * x.[26] // Bd | ligation: B + d <-> Bd
            -1.0 * x.[4] * x.[10] // B + d | ligation: B + d <-> Bd
            1.0 * x.[58] // bd | ligation: b + d <-> bd
            -1.0 * x.[8] * x.[10] // b + d | ligation: b + d <-> bd
            1.0 * x.[18] // Ad | ligation: A + d <-> Ad
            -1.0 * x.[3] * x.[10] // A + d | ligation: A + d <-> Ad
            1.0 * x.[50] // ad | ligation: a + d <-> ad
            -1.0 * x.[7] * x.[10] // a + d | ligation: a + d <-> ad
            0.001 * x.[2] // Z | destruction: d <-> Z
            -0.001 * x.[10] // d | destruction: d <-> Z
            -0.001 * x.[10] // d | synthesis: Y <-> d
            0.001 * x.[1] // Y | synthesis: Y <-> d
        |]
        |> Array.sum


    // 11 - AA
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[523] // dAA | ligation: d + AA <-> dAA
            -1.0 * x.[10] * x.[11] // d + AA | ligation: d + AA <-> dAA
            1.0 * x.[267] // DAA | ligation: D + AA <-> DAA
            -1.0 * x.[6] * x.[11] // D + AA | ligation: D + AA <-> DAA
            1.0 * x.[459] // cAA | ligation: c + AA <-> cAA
            -1.0 * x.[9] * x.[11] // c + AA | ligation: c + AA <-> cAA
            1.0 * x.[203] // CAA | ligation: C + AA <-> CAA
            -1.0 * x.[5] * x.[11] // C + AA | ligation: C + AA <-> CAA
            1.0 * x.[395] // bAA | ligation: b + AA <-> bAA
            -1.0 * x.[8] * x.[11] // b + AA | ligation: b + AA <-> bAA
            1.0 * x.[139] // BAA | ligation: B + AA <-> BAA
            -1.0 * x.[4] * x.[11] // B + AA | ligation: B + AA <-> BAA
            1.0 * x.[331] // aAA | ligation: a + AA <-> aAA
            -1.0 * x.[7] * x.[11] // a + AA | ligation: a + AA <-> aAA
            1.0 * x.[75] // AAA | ligation: A + AA <-> AAA
            -1.0 * x.[3] * x.[11] // A + AA | ligation: A + AA <-> AAA
            -1.0 * x.[11] // AA | ligation: A + A <-> AA
            1.0 * x.[3] * x.[3] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 12 - AB
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[524] // dAB | ligation: d + AB <-> dAB
            -1.0 * x.[10] * x.[12] // d + AB | ligation: d + AB <-> dAB
            1.0 * x.[268] // DAB | ligation: D + AB <-> DAB
            -1.0 * x.[6] * x.[12] // D + AB | ligation: D + AB <-> DAB
            1.0 * x.[460] // cAB | ligation: c + AB <-> cAB
            -1.0 * x.[9] * x.[12] // c + AB | ligation: c + AB <-> cAB
            1.0 * x.[204] // CAB | ligation: C + AB <-> CAB
            -1.0 * x.[5] * x.[12] // C + AB | ligation: C + AB <-> CAB
            1.0 * x.[396] // bAB | ligation: b + AB <-> bAB
            -1.0 * x.[8] * x.[12] // b + AB | ligation: b + AB <-> bAB
            1.0 * x.[140] // BAB | ligation: B + AB <-> BAB
            -1.0 * x.[4] * x.[12] // B + AB | ligation: B + AB <-> BAB
            1.0 * x.[332] // aAB | ligation: a + AB <-> aAB
            -1.0 * x.[7] * x.[12] // a + AB | ligation: a + AB <-> aAB
            1.0 * x.[76] // AAB | ligation: A + AB <-> AAB
            -1.0 * x.[3] * x.[12] // A + AB | ligation: A + AB <-> AAB
            -1.0 * x.[12] // AB | ligation: A + B <-> AB
            1.0 * x.[3] * x.[4] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 13 - AC
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[525] // dAC | ligation: d + AC <-> dAC
            -1.0 * x.[10] * x.[13] // d + AC | ligation: d + AC <-> dAC
            1.0 * x.[269] // DAC | ligation: D + AC <-> DAC
            -1.0 * x.[6] * x.[13] // D + AC | ligation: D + AC <-> DAC
            1.0 * x.[461] // cAC | ligation: c + AC <-> cAC
            -1.0 * x.[9] * x.[13] // c + AC | ligation: c + AC <-> cAC
            1.0 * x.[205] // CAC | ligation: C + AC <-> CAC
            -1.0 * x.[5] * x.[13] // C + AC | ligation: C + AC <-> CAC
            1.0 * x.[397] // bAC | ligation: b + AC <-> bAC
            -1.0 * x.[8] * x.[13] // b + AC | ligation: b + AC <-> bAC
            1.0 * x.[141] // BAC | ligation: B + AC <-> BAC
            -1.0 * x.[4] * x.[13] // B + AC | ligation: B + AC <-> BAC
            1.0 * x.[333] // aAC | ligation: a + AC <-> aAC
            -1.0 * x.[7] * x.[13] // a + AC | ligation: a + AC <-> aAC
            1.0 * x.[77] // AAC | ligation: A + AC <-> AAC
            -1.0 * x.[3] * x.[13] // A + AC | ligation: A + AC <-> AAC
            -1.0 * x.[13] // AC | ligation: A + C <-> AC
            1.0 * x.[3] * x.[5] // A + C | ligation: A + C <-> AC
        |]
        |> Array.sum


    // 14 - AD
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[526] // dAD | ligation: d + AD <-> dAD
            -1.0 * x.[10] * x.[14] // d + AD | ligation: d + AD <-> dAD
            1.0 * x.[270] // DAD | ligation: D + AD <-> DAD
            -1.0 * x.[6] * x.[14] // D + AD | ligation: D + AD <-> DAD
            1.0 * x.[462] // cAD | ligation: c + AD <-> cAD
            -1.0 * x.[9] * x.[14] // c + AD | ligation: c + AD <-> cAD
            1.0 * x.[206] // CAD | ligation: C + AD <-> CAD
            -1.0 * x.[5] * x.[14] // C + AD | ligation: C + AD <-> CAD
            1.0 * x.[398] // bAD | ligation: b + AD <-> bAD
            -1.0 * x.[8] * x.[14] // b + AD | ligation: b + AD <-> bAD
            1.0 * x.[142] // BAD | ligation: B + AD <-> BAD
            -1.0 * x.[4] * x.[14] // B + AD | ligation: B + AD <-> BAD
            1.0 * x.[334] // aAD | ligation: a + AD <-> aAD
            -1.0 * x.[7] * x.[14] // a + AD | ligation: a + AD <-> aAD
            1.0 * x.[78] // AAD | ligation: A + AD <-> AAD
            -1.0 * x.[3] * x.[14] // A + AD | ligation: A + AD <-> AAD
            -1.0 * x.[14] // AD | ligation: A + D <-> AD
            1.0 * x.[3] * x.[6] // A + D | ligation: A + D <-> AD
        |]
        |> Array.sum


    // 15 - Aa
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[527] // dAa | ligation: d + Aa <-> dAa
            -1.0 * x.[10] * x.[15] // d + Aa | ligation: d + Aa <-> dAa
            1.0 * x.[271] // DAa | ligation: D + Aa <-> DAa
            -1.0 * x.[6] * x.[15] // D + Aa | ligation: D + Aa <-> DAa
            1.0 * x.[463] // cAa | ligation: c + Aa <-> cAa
            -1.0 * x.[9] * x.[15] // c + Aa | ligation: c + Aa <-> cAa
            1.0 * x.[207] // CAa | ligation: C + Aa <-> CAa
            -1.0 * x.[5] * x.[15] // C + Aa | ligation: C + Aa <-> CAa
            1.0 * x.[399] // bAa | ligation: b + Aa <-> bAa
            -1.0 * x.[8] * x.[15] // b + Aa | ligation: b + Aa <-> bAa
            1.0 * x.[143] // BAa | ligation: B + Aa <-> BAa
            -1.0 * x.[4] * x.[15] // B + Aa | ligation: B + Aa <-> BAa
            1.0 * x.[335] // aAa | ligation: a + Aa <-> aAa
            -1.0 * x.[7] * x.[15] // a + Aa | ligation: a + Aa <-> aAa
            1.0 * x.[79] // AAa | ligation: A + Aa <-> AAa
            -1.0 * x.[3] * x.[15] // A + Aa | ligation: A + Aa <-> AAa
            -1.0 * x.[15] // Aa | ligation: A + a <-> Aa
            1.0 * x.[3] * x.[7] // A + a | ligation: A + a <-> Aa
        |]
        |> Array.sum


    // 16 - Ab
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[528] // dAb | ligation: d + Ab <-> dAb
            -1.0 * x.[10] * x.[16] // d + Ab | ligation: d + Ab <-> dAb
            1.0 * x.[272] // DAb | ligation: D + Ab <-> DAb
            -1.0 * x.[6] * x.[16] // D + Ab | ligation: D + Ab <-> DAb
            1.0 * x.[464] // cAb | ligation: c + Ab <-> cAb
            -1.0 * x.[9] * x.[16] // c + Ab | ligation: c + Ab <-> cAb
            1.0 * x.[208] // CAb | ligation: C + Ab <-> CAb
            -1.0 * x.[5] * x.[16] // C + Ab | ligation: C + Ab <-> CAb
            1.0 * x.[400] // bAb | ligation: b + Ab <-> bAb
            -1.0 * x.[8] * x.[16] // b + Ab | ligation: b + Ab <-> bAb
            1.0 * x.[144] // BAb | ligation: B + Ab <-> BAb
            -1.0 * x.[4] * x.[16] // B + Ab | ligation: B + Ab <-> BAb
            1.0 * x.[336] // aAb | ligation: a + Ab <-> aAb
            -1.0 * x.[7] * x.[16] // a + Ab | ligation: a + Ab <-> aAb
            1.0 * x.[80] // AAb | ligation: A + Ab <-> AAb
            -1.0 * x.[3] * x.[16] // A + Ab | ligation: A + Ab <-> AAb
            -1.0 * x.[16] // Ab | ligation: A + b <-> Ab
            1.0 * x.[3] * x.[8] // A + b | ligation: A + b <-> Ab
        |]
        |> Array.sum


    // 17 - Ac
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[529] // dAc | ligation: d + Ac <-> dAc
            -1.0 * x.[10] * x.[17] // d + Ac | ligation: d + Ac <-> dAc
            1.0 * x.[273] // DAc | ligation: D + Ac <-> DAc
            -1.0 * x.[6] * x.[17] // D + Ac | ligation: D + Ac <-> DAc
            1.0 * x.[465] // cAc | ligation: c + Ac <-> cAc
            -1.0 * x.[9] * x.[17] // c + Ac | ligation: c + Ac <-> cAc
            1.0 * x.[209] // CAc | ligation: C + Ac <-> CAc
            -1.0 * x.[5] * x.[17] // C + Ac | ligation: C + Ac <-> CAc
            1.0 * x.[401] // bAc | ligation: b + Ac <-> bAc
            -1.0 * x.[8] * x.[17] // b + Ac | ligation: b + Ac <-> bAc
            1.0 * x.[145] // BAc | ligation: B + Ac <-> BAc
            -1.0 * x.[4] * x.[17] // B + Ac | ligation: B + Ac <-> BAc
            1.0 * x.[337] // aAc | ligation: a + Ac <-> aAc
            -1.0 * x.[7] * x.[17] // a + Ac | ligation: a + Ac <-> aAc
            1.0 * x.[81] // AAc | ligation: A + Ac <-> AAc
            -1.0 * x.[3] * x.[17] // A + Ac | ligation: A + Ac <-> AAc
            -1.0 * x.[17] // Ac | ligation: A + c <-> Ac
            1.0 * x.[3] * x.[9] // A + c | ligation: A + c <-> Ac
        |]
        |> Array.sum


    // 18 - Ad
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[530] // dAd | ligation: d + Ad <-> dAd
            -1.0 * x.[10] * x.[18] // d + Ad | ligation: d + Ad <-> dAd
            1.0 * x.[274] // DAd | ligation: D + Ad <-> DAd
            -1.0 * x.[6] * x.[18] // D + Ad | ligation: D + Ad <-> DAd
            1.0 * x.[466] // cAd | ligation: c + Ad <-> cAd
            -1.0 * x.[9] * x.[18] // c + Ad | ligation: c + Ad <-> cAd
            1.0 * x.[210] // CAd | ligation: C + Ad <-> CAd
            -1.0 * x.[5] * x.[18] // C + Ad | ligation: C + Ad <-> CAd
            1.0 * x.[402] // bAd | ligation: b + Ad <-> bAd
            -1.0 * x.[8] * x.[18] // b + Ad | ligation: b + Ad <-> bAd
            1.0 * x.[146] // BAd | ligation: B + Ad <-> BAd
            -1.0 * x.[4] * x.[18] // B + Ad | ligation: B + Ad <-> BAd
            1.0 * x.[338] // aAd | ligation: a + Ad <-> aAd
            -1.0 * x.[7] * x.[18] // a + Ad | ligation: a + Ad <-> aAd
            1.0 * x.[82] // AAd | ligation: A + Ad <-> AAd
            -1.0 * x.[3] * x.[18] // A + Ad | ligation: A + Ad <-> AAd
            -1.0 * x.[18] // Ad | ligation: A + d <-> Ad
            1.0 * x.[3] * x.[10] // A + d | ligation: A + d <-> Ad
        |]
        |> Array.sum


    // 19 - BA
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[531] // dBA | ligation: d + BA <-> dBA
            -1.0 * x.[10] * x.[19] // d + BA | ligation: d + BA <-> dBA
            1.0 * x.[275] // DBA | ligation: D + BA <-> DBA
            -1.0 * x.[6] * x.[19] // D + BA | ligation: D + BA <-> DBA
            1.0 * x.[467] // cBA | ligation: c + BA <-> cBA
            -1.0 * x.[9] * x.[19] // c + BA | ligation: c + BA <-> cBA
            1.0 * x.[211] // CBA | ligation: C + BA <-> CBA
            -1.0 * x.[5] * x.[19] // C + BA | ligation: C + BA <-> CBA
            1.0 * x.[403] // bBA | ligation: b + BA <-> bBA
            -1.0 * x.[8] * x.[19] // b + BA | ligation: b + BA <-> bBA
            1.0 * x.[147] // BBA | ligation: B + BA <-> BBA
            -1.0 * x.[4] * x.[19] // B + BA | ligation: B + BA <-> BBA
            1.0 * x.[339] // aBA | ligation: a + BA <-> aBA
            -1.0 * x.[7] * x.[19] // a + BA | ligation: a + BA <-> aBA
            1.0 * x.[83] // ABA | ligation: A + BA <-> ABA
            -1.0 * x.[3] * x.[19] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 20 - BB
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[532] // dBB | ligation: d + BB <-> dBB
            -1.0 * x.[10] * x.[20] // d + BB | ligation: d + BB <-> dBB
            1.0 * x.[276] // DBB | ligation: D + BB <-> DBB
            -1.0 * x.[6] * x.[20] // D + BB | ligation: D + BB <-> DBB
            1.0 * x.[468] // cBB | ligation: c + BB <-> cBB
            -1.0 * x.[9] * x.[20] // c + BB | ligation: c + BB <-> cBB
            1.0 * x.[212] // CBB | ligation: C + BB <-> CBB
            -1.0 * x.[5] * x.[20] // C + BB | ligation: C + BB <-> CBB
            1.0 * x.[404] // bBB | ligation: b + BB <-> bBB
            -1.0 * x.[8] * x.[20] // b + BB | ligation: b + BB <-> bBB
            1.0 * x.[148] // BBB | ligation: B + BB <-> BBB
            -1.0 * x.[4] * x.[20] // B + BB | ligation: B + BB <-> BBB
            -1.0 * x.[20] // BB | ligation: B + B <-> BB
            1.0 * x.[4] * x.[4] // B + B | ligation: B + B <-> BB
            1.0 * x.[340] // aBB | ligation: a + BB <-> aBB
            -1.0 * x.[7] * x.[20] // a + BB | ligation: a + BB <-> aBB
            1.0 * x.[84] // ABB | ligation: A + BB <-> ABB
            -1.0 * x.[3] * x.[20] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 21 - BC
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[533] // dBC | ligation: d + BC <-> dBC
            -1.0 * x.[10] * x.[21] // d + BC | ligation: d + BC <-> dBC
            1.0 * x.[277] // DBC | ligation: D + BC <-> DBC
            -1.0 * x.[6] * x.[21] // D + BC | ligation: D + BC <-> DBC
            1.0 * x.[469] // cBC | ligation: c + BC <-> cBC
            -1.0 * x.[9] * x.[21] // c + BC | ligation: c + BC <-> cBC
            1.0 * x.[213] // CBC | ligation: C + BC <-> CBC
            -1.0 * x.[5] * x.[21] // C + BC | ligation: C + BC <-> CBC
            1.0 * x.[405] // bBC | ligation: b + BC <-> bBC
            -1.0 * x.[8] * x.[21] // b + BC | ligation: b + BC <-> bBC
            1.0 * x.[149] // BBC | ligation: B + BC <-> BBC
            -1.0 * x.[4] * x.[21] // B + BC | ligation: B + BC <-> BBC
            -1.0 * x.[21] // BC | ligation: B + C <-> BC
            1.0 * x.[4] * x.[5] // B + C | ligation: B + C <-> BC
            1.0 * x.[341] // aBC | ligation: a + BC <-> aBC
            -1.0 * x.[7] * x.[21] // a + BC | ligation: a + BC <-> aBC
            1.0 * x.[85] // ABC | ligation: A + BC <-> ABC
            -1.0 * x.[3] * x.[21] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 22 - BD
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[534] // dBD | ligation: d + BD <-> dBD
            -1.0 * x.[10] * x.[22] // d + BD | ligation: d + BD <-> dBD
            1.0 * x.[278] // DBD | ligation: D + BD <-> DBD
            -1.0 * x.[6] * x.[22] // D + BD | ligation: D + BD <-> DBD
            1.0 * x.[470] // cBD | ligation: c + BD <-> cBD
            -1.0 * x.[9] * x.[22] // c + BD | ligation: c + BD <-> cBD
            1.0 * x.[214] // CBD | ligation: C + BD <-> CBD
            -1.0 * x.[5] * x.[22] // C + BD | ligation: C + BD <-> CBD
            1.0 * x.[406] // bBD | ligation: b + BD <-> bBD
            -1.0 * x.[8] * x.[22] // b + BD | ligation: b + BD <-> bBD
            1.0 * x.[150] // BBD | ligation: B + BD <-> BBD
            -1.0 * x.[4] * x.[22] // B + BD | ligation: B + BD <-> BBD
            -1.0 * x.[22] // BD | ligation: B + D <-> BD
            1.0 * x.[4] * x.[6] // B + D | ligation: B + D <-> BD
            1.0 * x.[342] // aBD | ligation: a + BD <-> aBD
            -1.0 * x.[7] * x.[22] // a + BD | ligation: a + BD <-> aBD
            1.0 * x.[86] // ABD | ligation: A + BD <-> ABD
            -1.0 * x.[3] * x.[22] // A + BD | ligation: A + BD <-> ABD
        |]
        |> Array.sum


    // 23 - Ba
    let d23 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[535] // dBa | ligation: d + Ba <-> dBa
            -1.0 * x.[10] * x.[23] // d + Ba | ligation: d + Ba <-> dBa
            1.0 * x.[279] // DBa | ligation: D + Ba <-> DBa
            -1.0 * x.[6] * x.[23] // D + Ba | ligation: D + Ba <-> DBa
            1.0 * x.[471] // cBa | ligation: c + Ba <-> cBa
            -1.0 * x.[9] * x.[23] // c + Ba | ligation: c + Ba <-> cBa
            1.0 * x.[215] // CBa | ligation: C + Ba <-> CBa
            -1.0 * x.[5] * x.[23] // C + Ba | ligation: C + Ba <-> CBa
            1.0 * x.[407] // bBa | ligation: b + Ba <-> bBa
            -1.0 * x.[8] * x.[23] // b + Ba | ligation: b + Ba <-> bBa
            1.0 * x.[151] // BBa | ligation: B + Ba <-> BBa
            -1.0 * x.[4] * x.[23] // B + Ba | ligation: B + Ba <-> BBa
            -1.0 * x.[23] // Ba | ligation: B + a <-> Ba
            1.0 * x.[4] * x.[7] // B + a | ligation: B + a <-> Ba
            1.0 * x.[343] // aBa | ligation: a + Ba <-> aBa
            -1.0 * x.[7] * x.[23] // a + Ba | ligation: a + Ba <-> aBa
            1.0 * x.[87] // ABa | ligation: A + Ba <-> ABa
            -1.0 * x.[3] * x.[23] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 24 - Bb
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[536] // dBb | ligation: d + Bb <-> dBb
            -1.0 * x.[10] * x.[24] // d + Bb | ligation: d + Bb <-> dBb
            1.0 * x.[280] // DBb | ligation: D + Bb <-> DBb
            -1.0 * x.[6] * x.[24] // D + Bb | ligation: D + Bb <-> DBb
            1.0 * x.[472] // cBb | ligation: c + Bb <-> cBb
            -1.0 * x.[9] * x.[24] // c + Bb | ligation: c + Bb <-> cBb
            1.0 * x.[216] // CBb | ligation: C + Bb <-> CBb
            -1.0 * x.[5] * x.[24] // C + Bb | ligation: C + Bb <-> CBb
            1.0 * x.[408] // bBb | ligation: b + Bb <-> bBb
            -1.0 * x.[8] * x.[24] // b + Bb | ligation: b + Bb <-> bBb
            1.0 * x.[152] // BBb | ligation: B + Bb <-> BBb
            -1.0 * x.[4] * x.[24] // B + Bb | ligation: B + Bb <-> BBb
            -1.0 * x.[24] // Bb | ligation: B + b <-> Bb
            1.0 * x.[4] * x.[8] // B + b | ligation: B + b <-> Bb
            1.0 * x.[344] // aBb | ligation: a + Bb <-> aBb
            -1.0 * x.[7] * x.[24] // a + Bb | ligation: a + Bb <-> aBb
            1.0 * x.[88] // ABb | ligation: A + Bb <-> ABb
            -1.0 * x.[3] * x.[24] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 25 - Bc
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[537] // dBc | ligation: d + Bc <-> dBc
            -1.0 * x.[10] * x.[25] // d + Bc | ligation: d + Bc <-> dBc
            1.0 * x.[281] // DBc | ligation: D + Bc <-> DBc
            -1.0 * x.[6] * x.[25] // D + Bc | ligation: D + Bc <-> DBc
            1.0 * x.[473] // cBc | ligation: c + Bc <-> cBc
            -1.0 * x.[9] * x.[25] // c + Bc | ligation: c + Bc <-> cBc
            1.0 * x.[217] // CBc | ligation: C + Bc <-> CBc
            -1.0 * x.[5] * x.[25] // C + Bc | ligation: C + Bc <-> CBc
            1.0 * x.[409] // bBc | ligation: b + Bc <-> bBc
            -1.0 * x.[8] * x.[25] // b + Bc | ligation: b + Bc <-> bBc
            1.0 * x.[153] // BBc | ligation: B + Bc <-> BBc
            -1.0 * x.[4] * x.[25] // B + Bc | ligation: B + Bc <-> BBc
            -1.0 * x.[25] // Bc | ligation: B + c <-> Bc
            1.0 * x.[4] * x.[9] // B + c | ligation: B + c <-> Bc
            1.0 * x.[345] // aBc | ligation: a + Bc <-> aBc
            -1.0 * x.[7] * x.[25] // a + Bc | ligation: a + Bc <-> aBc
            1.0 * x.[89] // ABc | ligation: A + Bc <-> ABc
            -1.0 * x.[3] * x.[25] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 26 - Bd
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[538] // dBd | ligation: d + Bd <-> dBd
            -1.0 * x.[10] * x.[26] // d + Bd | ligation: d + Bd <-> dBd
            1.0 * x.[282] // DBd | ligation: D + Bd <-> DBd
            -1.0 * x.[6] * x.[26] // D + Bd | ligation: D + Bd <-> DBd
            1.0 * x.[474] // cBd | ligation: c + Bd <-> cBd
            -1.0 * x.[9] * x.[26] // c + Bd | ligation: c + Bd <-> cBd
            1.0 * x.[218] // CBd | ligation: C + Bd <-> CBd
            -1.0 * x.[5] * x.[26] // C + Bd | ligation: C + Bd <-> CBd
            1.0 * x.[410] // bBd | ligation: b + Bd <-> bBd
            -1.0 * x.[8] * x.[26] // b + Bd | ligation: b + Bd <-> bBd
            1.0 * x.[154] // BBd | ligation: B + Bd <-> BBd
            -1.0 * x.[4] * x.[26] // B + Bd | ligation: B + Bd <-> BBd
            -1.0 * x.[26] // Bd | ligation: B + d <-> Bd
            1.0 * x.[4] * x.[10] // B + d | ligation: B + d <-> Bd
            1.0 * x.[346] // aBd | ligation: a + Bd <-> aBd
            -1.0 * x.[7] * x.[26] // a + Bd | ligation: a + Bd <-> aBd
            1.0 * x.[90] // ABd | ligation: A + Bd <-> ABd
            -1.0 * x.[3] * x.[26] // A + Bd | ligation: A + Bd <-> ABd
        |]
        |> Array.sum


    // 27 - CA
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[539] // dCA | ligation: d + CA <-> dCA
            -1.0 * x.[10] * x.[27] // d + CA | ligation: d + CA <-> dCA
            1.0 * x.[283] // DCA | ligation: D + CA <-> DCA
            -1.0 * x.[6] * x.[27] // D + CA | ligation: D + CA <-> DCA
            1.0 * x.[475] // cCA | ligation: c + CA <-> cCA
            -1.0 * x.[9] * x.[27] // c + CA | ligation: c + CA <-> cCA
            1.0 * x.[219] // CCA | ligation: C + CA <-> CCA
            -1.0 * x.[5] * x.[27] // C + CA | ligation: C + CA <-> CCA
            1.0 * x.[411] // bCA | ligation: b + CA <-> bCA
            -1.0 * x.[8] * x.[27] // b + CA | ligation: b + CA <-> bCA
            1.0 * x.[155] // BCA | ligation: B + CA <-> BCA
            -1.0 * x.[4] * x.[27] // B + CA | ligation: B + CA <-> BCA
            1.0 * x.[347] // aCA | ligation: a + CA <-> aCA
            -1.0 * x.[7] * x.[27] // a + CA | ligation: a + CA <-> aCA
            1.0 * x.[91] // ACA | ligation: A + CA <-> ACA
            -1.0 * x.[3] * x.[27] // A + CA | ligation: A + CA <-> ACA
        |]
        |> Array.sum


    // 28 - CB
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[540] // dCB | ligation: d + CB <-> dCB
            -1.0 * x.[10] * x.[28] // d + CB | ligation: d + CB <-> dCB
            1.0 * x.[284] // DCB | ligation: D + CB <-> DCB
            -1.0 * x.[6] * x.[28] // D + CB | ligation: D + CB <-> DCB
            1.0 * x.[476] // cCB | ligation: c + CB <-> cCB
            -1.0 * x.[9] * x.[28] // c + CB | ligation: c + CB <-> cCB
            1.0 * x.[220] // CCB | ligation: C + CB <-> CCB
            -1.0 * x.[5] * x.[28] // C + CB | ligation: C + CB <-> CCB
            1.0 * x.[412] // bCB | ligation: b + CB <-> bCB
            -1.0 * x.[8] * x.[28] // b + CB | ligation: b + CB <-> bCB
            1.0 * x.[156] // BCB | ligation: B + CB <-> BCB
            -1.0 * x.[4] * x.[28] // B + CB | ligation: B + CB <-> BCB
            1.0 * x.[348] // aCB | ligation: a + CB <-> aCB
            -1.0 * x.[7] * x.[28] // a + CB | ligation: a + CB <-> aCB
            1.0 * x.[92] // ACB | ligation: A + CB <-> ACB
            -1.0 * x.[3] * x.[28] // A + CB | ligation: A + CB <-> ACB
        |]
        |> Array.sum


    // 29 - CC
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[541] // dCC | ligation: d + CC <-> dCC
            -1.0 * x.[10] * x.[29] // d + CC | ligation: d + CC <-> dCC
            1.0 * x.[285] // DCC | ligation: D + CC <-> DCC
            -1.0 * x.[6] * x.[29] // D + CC | ligation: D + CC <-> DCC
            1.0 * x.[477] // cCC | ligation: c + CC <-> cCC
            -1.0 * x.[9] * x.[29] // c + CC | ligation: c + CC <-> cCC
            1.0 * x.[221] // CCC | ligation: C + CC <-> CCC
            -1.0 * x.[5] * x.[29] // C + CC | ligation: C + CC <-> CCC
            -1.0 * x.[29] // CC | ligation: C + C <-> CC
            1.0 * x.[5] * x.[5] // C + C | ligation: C + C <-> CC
            1.0 * x.[413] // bCC | ligation: b + CC <-> bCC
            -1.0 * x.[8] * x.[29] // b + CC | ligation: b + CC <-> bCC
            1.0 * x.[157] // BCC | ligation: B + CC <-> BCC
            -1.0 * x.[4] * x.[29] // B + CC | ligation: B + CC <-> BCC
            1.0 * x.[349] // aCC | ligation: a + CC <-> aCC
            -1.0 * x.[7] * x.[29] // a + CC | ligation: a + CC <-> aCC
            1.0 * x.[93] // ACC | ligation: A + CC <-> ACC
            -1.0 * x.[3] * x.[29] // A + CC | ligation: A + CC <-> ACC
        |]
        |> Array.sum


    // 30 - CD
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[542] // dCD | ligation: d + CD <-> dCD
            -1.0 * x.[10] * x.[30] // d + CD | ligation: d + CD <-> dCD
            1.0 * x.[286] // DCD | ligation: D + CD <-> DCD
            -1.0 * x.[6] * x.[30] // D + CD | ligation: D + CD <-> DCD
            1.0 * x.[478] // cCD | ligation: c + CD <-> cCD
            -1.0 * x.[9] * x.[30] // c + CD | ligation: c + CD <-> cCD
            1.0 * x.[222] // CCD | ligation: C + CD <-> CCD
            -1.0 * x.[5] * x.[30] // C + CD | ligation: C + CD <-> CCD
            -1.0 * x.[30] // CD | ligation: C + D <-> CD
            1.0 * x.[5] * x.[6] // C + D | ligation: C + D <-> CD
            1.0 * x.[414] // bCD | ligation: b + CD <-> bCD
            -1.0 * x.[8] * x.[30] // b + CD | ligation: b + CD <-> bCD
            1.0 * x.[158] // BCD | ligation: B + CD <-> BCD
            -1.0 * x.[4] * x.[30] // B + CD | ligation: B + CD <-> BCD
            1.0 * x.[350] // aCD | ligation: a + CD <-> aCD
            -1.0 * x.[7] * x.[30] // a + CD | ligation: a + CD <-> aCD
            1.0 * x.[94] // ACD | ligation: A + CD <-> ACD
            -1.0 * x.[3] * x.[30] // A + CD | ligation: A + CD <-> ACD
        |]
        |> Array.sum


    // 31 - Ca
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[543] // dCa | ligation: d + Ca <-> dCa
            -1.0 * x.[10] * x.[31] // d + Ca | ligation: d + Ca <-> dCa
            1.0 * x.[287] // DCa | ligation: D + Ca <-> DCa
            -1.0 * x.[6] * x.[31] // D + Ca | ligation: D + Ca <-> DCa
            1.0 * x.[479] // cCa | ligation: c + Ca <-> cCa
            -1.0 * x.[9] * x.[31] // c + Ca | ligation: c + Ca <-> cCa
            1.0 * x.[223] // CCa | ligation: C + Ca <-> CCa
            -1.0 * x.[5] * x.[31] // C + Ca | ligation: C + Ca <-> CCa
            -1.0 * x.[31] // Ca | ligation: C + a <-> Ca
            1.0 * x.[5] * x.[7] // C + a | ligation: C + a <-> Ca
            1.0 * x.[415] // bCa | ligation: b + Ca <-> bCa
            -1.0 * x.[8] * x.[31] // b + Ca | ligation: b + Ca <-> bCa
            1.0 * x.[159] // BCa | ligation: B + Ca <-> BCa
            -1.0 * x.[4] * x.[31] // B + Ca | ligation: B + Ca <-> BCa
            1.0 * x.[351] // aCa | ligation: a + Ca <-> aCa
            -1.0 * x.[7] * x.[31] // a + Ca | ligation: a + Ca <-> aCa
            1.0 * x.[95] // ACa | ligation: A + Ca <-> ACa
            -1.0 * x.[3] * x.[31] // A + Ca | ligation: A + Ca <-> ACa
        |]
        |> Array.sum


    // 32 - Cb
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[544] // dCb | ligation: d + Cb <-> dCb
            -1.0 * x.[10] * x.[32] // d + Cb | ligation: d + Cb <-> dCb
            1.0 * x.[288] // DCb | ligation: D + Cb <-> DCb
            -1.0 * x.[6] * x.[32] // D + Cb | ligation: D + Cb <-> DCb
            1.0 * x.[480] // cCb | ligation: c + Cb <-> cCb
            -1.0 * x.[9] * x.[32] // c + Cb | ligation: c + Cb <-> cCb
            1.0 * x.[224] // CCb | ligation: C + Cb <-> CCb
            -1.0 * x.[5] * x.[32] // C + Cb | ligation: C + Cb <-> CCb
            -1.0 * x.[32] // Cb | ligation: C + b <-> Cb
            1.0 * x.[5] * x.[8] // C + b | ligation: C + b <-> Cb
            1.0 * x.[416] // bCb | ligation: b + Cb <-> bCb
            -1.0 * x.[8] * x.[32] // b + Cb | ligation: b + Cb <-> bCb
            1.0 * x.[160] // BCb | ligation: B + Cb <-> BCb
            -1.0 * x.[4] * x.[32] // B + Cb | ligation: B + Cb <-> BCb
            1.0 * x.[352] // aCb | ligation: a + Cb <-> aCb
            -1.0 * x.[7] * x.[32] // a + Cb | ligation: a + Cb <-> aCb
            1.0 * x.[96] // ACb | ligation: A + Cb <-> ACb
            -1.0 * x.[3] * x.[32] // A + Cb | ligation: A + Cb <-> ACb
        |]
        |> Array.sum


    // 33 - Cc
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[545] // dCc | ligation: d + Cc <-> dCc
            -1.0 * x.[10] * x.[33] // d + Cc | ligation: d + Cc <-> dCc
            1.0 * x.[289] // DCc | ligation: D + Cc <-> DCc
            -1.0 * x.[6] * x.[33] // D + Cc | ligation: D + Cc <-> DCc
            1.0 * x.[481] // cCc | ligation: c + Cc <-> cCc
            -1.0 * x.[9] * x.[33] // c + Cc | ligation: c + Cc <-> cCc
            1.0 * x.[225] // CCc | ligation: C + Cc <-> CCc
            -1.0 * x.[5] * x.[33] // C + Cc | ligation: C + Cc <-> CCc
            -1.0 * x.[33] // Cc | ligation: C + c <-> Cc
            1.0 * x.[5] * x.[9] // C + c | ligation: C + c <-> Cc
            1.0 * x.[417] // bCc | ligation: b + Cc <-> bCc
            -1.0 * x.[8] * x.[33] // b + Cc | ligation: b + Cc <-> bCc
            1.0 * x.[161] // BCc | ligation: B + Cc <-> BCc
            -1.0 * x.[4] * x.[33] // B + Cc | ligation: B + Cc <-> BCc
            1.0 * x.[353] // aCc | ligation: a + Cc <-> aCc
            -1.0 * x.[7] * x.[33] // a + Cc | ligation: a + Cc <-> aCc
            1.0 * x.[97] // ACc | ligation: A + Cc <-> ACc
            -1.0 * x.[3] * x.[33] // A + Cc | ligation: A + Cc <-> ACc
        |]
        |> Array.sum


    // 34 - Cd
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[546] // dCd | ligation: d + Cd <-> dCd
            -1.0 * x.[10] * x.[34] // d + Cd | ligation: d + Cd <-> dCd
            1.0 * x.[290] // DCd | ligation: D + Cd <-> DCd
            -1.0 * x.[6] * x.[34] // D + Cd | ligation: D + Cd <-> DCd
            1.0 * x.[482] // cCd | ligation: c + Cd <-> cCd
            -1.0 * x.[9] * x.[34] // c + Cd | ligation: c + Cd <-> cCd
            1.0 * x.[226] // CCd | ligation: C + Cd <-> CCd
            -1.0 * x.[5] * x.[34] // C + Cd | ligation: C + Cd <-> CCd
            -1.0 * x.[34] // Cd | ligation: C + d <-> Cd
            1.0 * x.[5] * x.[10] // C + d | ligation: C + d <-> Cd
            1.0 * x.[418] // bCd | ligation: b + Cd <-> bCd
            -1.0 * x.[8] * x.[34] // b + Cd | ligation: b + Cd <-> bCd
            1.0 * x.[162] // BCd | ligation: B + Cd <-> BCd
            -1.0 * x.[4] * x.[34] // B + Cd | ligation: B + Cd <-> BCd
            1.0 * x.[354] // aCd | ligation: a + Cd <-> aCd
            -1.0 * x.[7] * x.[34] // a + Cd | ligation: a + Cd <-> aCd
            1.0 * x.[98] // ACd | ligation: A + Cd <-> ACd
            -1.0 * x.[3] * x.[34] // A + Cd | ligation: A + Cd <-> ACd
        |]
        |> Array.sum


    // 35 - DA
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[547] // dDA | ligation: d + DA <-> dDA
            -1.0 * x.[10] * x.[35] // d + DA | ligation: d + DA <-> dDA
            1.0 * x.[291] // DDA | ligation: D + DA <-> DDA
            -1.0 * x.[6] * x.[35] // D + DA | ligation: D + DA <-> DDA
            1.0 * x.[483] // cDA | ligation: c + DA <-> cDA
            -1.0 * x.[9] * x.[35] // c + DA | ligation: c + DA <-> cDA
            1.0 * x.[227] // CDA | ligation: C + DA <-> CDA
            -1.0 * x.[5] * x.[35] // C + DA | ligation: C + DA <-> CDA
            1.0 * x.[419] // bDA | ligation: b + DA <-> bDA
            -1.0 * x.[8] * x.[35] // b + DA | ligation: b + DA <-> bDA
            1.0 * x.[163] // BDA | ligation: B + DA <-> BDA
            -1.0 * x.[4] * x.[35] // B + DA | ligation: B + DA <-> BDA
            1.0 * x.[355] // aDA | ligation: a + DA <-> aDA
            -1.0 * x.[7] * x.[35] // a + DA | ligation: a + DA <-> aDA
            1.0 * x.[99] // ADA | ligation: A + DA <-> ADA
            -1.0 * x.[3] * x.[35] // A + DA | ligation: A + DA <-> ADA
        |]
        |> Array.sum


    // 36 - DB
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[548] // dDB | ligation: d + DB <-> dDB
            -1.0 * x.[10] * x.[36] // d + DB | ligation: d + DB <-> dDB
            1.0 * x.[292] // DDB | ligation: D + DB <-> DDB
            -1.0 * x.[6] * x.[36] // D + DB | ligation: D + DB <-> DDB
            1.0 * x.[484] // cDB | ligation: c + DB <-> cDB
            -1.0 * x.[9] * x.[36] // c + DB | ligation: c + DB <-> cDB
            1.0 * x.[228] // CDB | ligation: C + DB <-> CDB
            -1.0 * x.[5] * x.[36] // C + DB | ligation: C + DB <-> CDB
            1.0 * x.[420] // bDB | ligation: b + DB <-> bDB
            -1.0 * x.[8] * x.[36] // b + DB | ligation: b + DB <-> bDB
            1.0 * x.[164] // BDB | ligation: B + DB <-> BDB
            -1.0 * x.[4] * x.[36] // B + DB | ligation: B + DB <-> BDB
            1.0 * x.[356] // aDB | ligation: a + DB <-> aDB
            -1.0 * x.[7] * x.[36] // a + DB | ligation: a + DB <-> aDB
            1.0 * x.[100] // ADB | ligation: A + DB <-> ADB
            -1.0 * x.[3] * x.[36] // A + DB | ligation: A + DB <-> ADB
        |]
        |> Array.sum


    // 37 - DC
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[549] // dDC | ligation: d + DC <-> dDC
            -1.0 * x.[10] * x.[37] // d + DC | ligation: d + DC <-> dDC
            1.0 * x.[293] // DDC | ligation: D + DC <-> DDC
            -1.0 * x.[6] * x.[37] // D + DC | ligation: D + DC <-> DDC
            1.0 * x.[485] // cDC | ligation: c + DC <-> cDC
            -1.0 * x.[9] * x.[37] // c + DC | ligation: c + DC <-> cDC
            1.0 * x.[229] // CDC | ligation: C + DC <-> CDC
            -1.0 * x.[5] * x.[37] // C + DC | ligation: C + DC <-> CDC
            1.0 * x.[421] // bDC | ligation: b + DC <-> bDC
            -1.0 * x.[8] * x.[37] // b + DC | ligation: b + DC <-> bDC
            1.0 * x.[165] // BDC | ligation: B + DC <-> BDC
            -1.0 * x.[4] * x.[37] // B + DC | ligation: B + DC <-> BDC
            1.0 * x.[357] // aDC | ligation: a + DC <-> aDC
            -1.0 * x.[7] * x.[37] // a + DC | ligation: a + DC <-> aDC
            1.0 * x.[101] // ADC | ligation: A + DC <-> ADC
            -1.0 * x.[3] * x.[37] // A + DC | ligation: A + DC <-> ADC
        |]
        |> Array.sum


    // 38 - DD
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[550] // dDD | ligation: d + DD <-> dDD
            -1.0 * x.[10] * x.[38] // d + DD | ligation: d + DD <-> dDD
            1.0 * x.[294] // DDD | ligation: D + DD <-> DDD
            -1.0 * x.[6] * x.[38] // D + DD | ligation: D + DD <-> DDD
            -1.0 * x.[38] // DD | ligation: D + D <-> DD
            1.0 * x.[6] * x.[6] // D + D | ligation: D + D <-> DD
            1.0 * x.[486] // cDD | ligation: c + DD <-> cDD
            -1.0 * x.[9] * x.[38] // c + DD | ligation: c + DD <-> cDD
            1.0 * x.[230] // CDD | ligation: C + DD <-> CDD
            -1.0 * x.[5] * x.[38] // C + DD | ligation: C + DD <-> CDD
            1.0 * x.[422] // bDD | ligation: b + DD <-> bDD
            -1.0 * x.[8] * x.[38] // b + DD | ligation: b + DD <-> bDD
            1.0 * x.[166] // BDD | ligation: B + DD <-> BDD
            -1.0 * x.[4] * x.[38] // B + DD | ligation: B + DD <-> BDD
            1.0 * x.[358] // aDD | ligation: a + DD <-> aDD
            -1.0 * x.[7] * x.[38] // a + DD | ligation: a + DD <-> aDD
            1.0 * x.[102] // ADD | ligation: A + DD <-> ADD
            -1.0 * x.[3] * x.[38] // A + DD | ligation: A + DD <-> ADD
        |]
        |> Array.sum


    // 39 - Da
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[551] // dDa | ligation: d + Da <-> dDa
            -1.0 * x.[10] * x.[39] // d + Da | ligation: d + Da <-> dDa
            1.0 * x.[295] // DDa | ligation: D + Da <-> DDa
            -1.0 * x.[6] * x.[39] // D + Da | ligation: D + Da <-> DDa
            -1.0 * x.[39] // Da | ligation: D + a <-> Da
            1.0 * x.[6] * x.[7] // D + a | ligation: D + a <-> Da
            1.0 * x.[487] // cDa | ligation: c + Da <-> cDa
            -1.0 * x.[9] * x.[39] // c + Da | ligation: c + Da <-> cDa
            1.0 * x.[231] // CDa | ligation: C + Da <-> CDa
            -1.0 * x.[5] * x.[39] // C + Da | ligation: C + Da <-> CDa
            1.0 * x.[423] // bDa | ligation: b + Da <-> bDa
            -1.0 * x.[8] * x.[39] // b + Da | ligation: b + Da <-> bDa
            1.0 * x.[167] // BDa | ligation: B + Da <-> BDa
            -1.0 * x.[4] * x.[39] // B + Da | ligation: B + Da <-> BDa
            1.0 * x.[359] // aDa | ligation: a + Da <-> aDa
            -1.0 * x.[7] * x.[39] // a + Da | ligation: a + Da <-> aDa
            1.0 * x.[103] // ADa | ligation: A + Da <-> ADa
            -1.0 * x.[3] * x.[39] // A + Da | ligation: A + Da <-> ADa
        |]
        |> Array.sum


    // 40 - Db
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[552] // dDb | ligation: d + Db <-> dDb
            -1.0 * x.[10] * x.[40] // d + Db | ligation: d + Db <-> dDb
            1.0 * x.[296] // DDb | ligation: D + Db <-> DDb
            -1.0 * x.[6] * x.[40] // D + Db | ligation: D + Db <-> DDb
            -1.0 * x.[40] // Db | ligation: D + b <-> Db
            1.0 * x.[6] * x.[8] // D + b | ligation: D + b <-> Db
            1.0 * x.[488] // cDb | ligation: c + Db <-> cDb
            -1.0 * x.[9] * x.[40] // c + Db | ligation: c + Db <-> cDb
            1.0 * x.[232] // CDb | ligation: C + Db <-> CDb
            -1.0 * x.[5] * x.[40] // C + Db | ligation: C + Db <-> CDb
            1.0 * x.[424] // bDb | ligation: b + Db <-> bDb
            -1.0 * x.[8] * x.[40] // b + Db | ligation: b + Db <-> bDb
            1.0 * x.[168] // BDb | ligation: B + Db <-> BDb
            -1.0 * x.[4] * x.[40] // B + Db | ligation: B + Db <-> BDb
            1.0 * x.[360] // aDb | ligation: a + Db <-> aDb
            -1.0 * x.[7] * x.[40] // a + Db | ligation: a + Db <-> aDb
            1.0 * x.[104] // ADb | ligation: A + Db <-> ADb
            -1.0 * x.[3] * x.[40] // A + Db | ligation: A + Db <-> ADb
        |]
        |> Array.sum


    // 41 - Dc
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[553] // dDc | ligation: d + Dc <-> dDc
            -1.0 * x.[10] * x.[41] // d + Dc | ligation: d + Dc <-> dDc
            1.0 * x.[297] // DDc | ligation: D + Dc <-> DDc
            -1.0 * x.[6] * x.[41] // D + Dc | ligation: D + Dc <-> DDc
            -1.0 * x.[41] // Dc | ligation: D + c <-> Dc
            1.0 * x.[6] * x.[9] // D + c | ligation: D + c <-> Dc
            1.0 * x.[489] // cDc | ligation: c + Dc <-> cDc
            -1.0 * x.[9] * x.[41] // c + Dc | ligation: c + Dc <-> cDc
            1.0 * x.[233] // CDc | ligation: C + Dc <-> CDc
            -1.0 * x.[5] * x.[41] // C + Dc | ligation: C + Dc <-> CDc
            1.0 * x.[425] // bDc | ligation: b + Dc <-> bDc
            -1.0 * x.[8] * x.[41] // b + Dc | ligation: b + Dc <-> bDc
            1.0 * x.[169] // BDc | ligation: B + Dc <-> BDc
            -1.0 * x.[4] * x.[41] // B + Dc | ligation: B + Dc <-> BDc
            1.0 * x.[361] // aDc | ligation: a + Dc <-> aDc
            -1.0 * x.[7] * x.[41] // a + Dc | ligation: a + Dc <-> aDc
            1.0 * x.[105] // ADc | ligation: A + Dc <-> ADc
            -1.0 * x.[3] * x.[41] // A + Dc | ligation: A + Dc <-> ADc
        |]
        |> Array.sum


    // 42 - Dd
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[554] // dDd | ligation: d + Dd <-> dDd
            -1.0 * x.[10] * x.[42] // d + Dd | ligation: d + Dd <-> dDd
            1.0 * x.[298] // DDd | ligation: D + Dd <-> DDd
            -1.0 * x.[6] * x.[42] // D + Dd | ligation: D + Dd <-> DDd
            -1.0 * x.[42] // Dd | ligation: D + d <-> Dd
            1.0 * x.[6] * x.[10] // D + d | ligation: D + d <-> Dd
            1.0 * x.[490] // cDd | ligation: c + Dd <-> cDd
            -1.0 * x.[9] * x.[42] // c + Dd | ligation: c + Dd <-> cDd
            1.0 * x.[234] // CDd | ligation: C + Dd <-> CDd
            -1.0 * x.[5] * x.[42] // C + Dd | ligation: C + Dd <-> CDd
            1.0 * x.[426] // bDd | ligation: b + Dd <-> bDd
            -1.0 * x.[8] * x.[42] // b + Dd | ligation: b + Dd <-> bDd
            1.0 * x.[170] // BDd | ligation: B + Dd <-> BDd
            -1.0 * x.[4] * x.[42] // B + Dd | ligation: B + Dd <-> BDd
            1.0 * x.[362] // aDd | ligation: a + Dd <-> aDd
            -1.0 * x.[7] * x.[42] // a + Dd | ligation: a + Dd <-> aDd
            1.0 * x.[106] // ADd | ligation: A + Dd <-> ADd
            -1.0 * x.[3] * x.[42] // A + Dd | ligation: A + Dd <-> ADd
        |]
        |> Array.sum


    // 43 - aA
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[299] // DaA | ligation: D + aA <-> DaA
            -1.0 * x.[6] * x.[43] // D + aA | ligation: D + aA <-> DaA
            1.0 * x.[555] // daA | ligation: d + aA <-> daA
            -1.0 * x.[10] * x.[43] // d + aA | ligation: d + aA <-> daA
            1.0 * x.[235] // CaA | ligation: C + aA <-> CaA
            -1.0 * x.[5] * x.[43] // C + aA | ligation: C + aA <-> CaA
            1.0 * x.[491] // caA | ligation: c + aA <-> caA
            -1.0 * x.[9] * x.[43] // c + aA | ligation: c + aA <-> caA
            1.0 * x.[171] // BaA | ligation: B + aA <-> BaA
            -1.0 * x.[4] * x.[43] // B + aA | ligation: B + aA <-> BaA
            1.0 * x.[427] // baA | ligation: b + aA <-> baA
            -1.0 * x.[8] * x.[43] // b + aA | ligation: b + aA <-> baA
            1.0 * x.[107] // AaA | ligation: A + aA <-> AaA
            -1.0 * x.[3] * x.[43] // A + aA | ligation: A + aA <-> AaA
            1.0 * x.[363] // aaA | ligation: a + aA <-> aaA
            -1.0 * x.[7] * x.[43] // a + aA | ligation: a + aA <-> aaA
            -1.0 * x.[43] // aA | ligation: a + A <-> aA
            1.0 * x.[7] * x.[3] // a + A | ligation: a + A <-> aA
        |]
        |> Array.sum


    // 44 - aB
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[300] // DaB | ligation: D + aB <-> DaB
            -1.0 * x.[6] * x.[44] // D + aB | ligation: D + aB <-> DaB
            1.0 * x.[556] // daB | ligation: d + aB <-> daB
            -1.0 * x.[10] * x.[44] // d + aB | ligation: d + aB <-> daB
            1.0 * x.[236] // CaB | ligation: C + aB <-> CaB
            -1.0 * x.[5] * x.[44] // C + aB | ligation: C + aB <-> CaB
            1.0 * x.[492] // caB | ligation: c + aB <-> caB
            -1.0 * x.[9] * x.[44] // c + aB | ligation: c + aB <-> caB
            1.0 * x.[172] // BaB | ligation: B + aB <-> BaB
            -1.0 * x.[4] * x.[44] // B + aB | ligation: B + aB <-> BaB
            1.0 * x.[428] // baB | ligation: b + aB <-> baB
            -1.0 * x.[8] * x.[44] // b + aB | ligation: b + aB <-> baB
            1.0 * x.[108] // AaB | ligation: A + aB <-> AaB
            -1.0 * x.[3] * x.[44] // A + aB | ligation: A + aB <-> AaB
            1.0 * x.[364] // aaB | ligation: a + aB <-> aaB
            -1.0 * x.[7] * x.[44] // a + aB | ligation: a + aB <-> aaB
            -1.0 * x.[44] // aB | ligation: a + B <-> aB
            1.0 * x.[7] * x.[4] // a + B | ligation: a + B <-> aB
        |]
        |> Array.sum


    // 45 - aC
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[301] // DaC | ligation: D + aC <-> DaC
            -1.0 * x.[6] * x.[45] // D + aC | ligation: D + aC <-> DaC
            1.0 * x.[557] // daC | ligation: d + aC <-> daC
            -1.0 * x.[10] * x.[45] // d + aC | ligation: d + aC <-> daC
            1.0 * x.[237] // CaC | ligation: C + aC <-> CaC
            -1.0 * x.[5] * x.[45] // C + aC | ligation: C + aC <-> CaC
            1.0 * x.[493] // caC | ligation: c + aC <-> caC
            -1.0 * x.[9] * x.[45] // c + aC | ligation: c + aC <-> caC
            1.0 * x.[173] // BaC | ligation: B + aC <-> BaC
            -1.0 * x.[4] * x.[45] // B + aC | ligation: B + aC <-> BaC
            1.0 * x.[429] // baC | ligation: b + aC <-> baC
            -1.0 * x.[8] * x.[45] // b + aC | ligation: b + aC <-> baC
            1.0 * x.[109] // AaC | ligation: A + aC <-> AaC
            -1.0 * x.[3] * x.[45] // A + aC | ligation: A + aC <-> AaC
            1.0 * x.[365] // aaC | ligation: a + aC <-> aaC
            -1.0 * x.[7] * x.[45] // a + aC | ligation: a + aC <-> aaC
            -1.0 * x.[45] // aC | ligation: a + C <-> aC
            1.0 * x.[7] * x.[5] // a + C | ligation: a + C <-> aC
        |]
        |> Array.sum


    // 46 - aD
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[302] // DaD | ligation: D + aD <-> DaD
            -1.0 * x.[6] * x.[46] // D + aD | ligation: D + aD <-> DaD
            1.0 * x.[558] // daD | ligation: d + aD <-> daD
            -1.0 * x.[10] * x.[46] // d + aD | ligation: d + aD <-> daD
            1.0 * x.[238] // CaD | ligation: C + aD <-> CaD
            -1.0 * x.[5] * x.[46] // C + aD | ligation: C + aD <-> CaD
            1.0 * x.[494] // caD | ligation: c + aD <-> caD
            -1.0 * x.[9] * x.[46] // c + aD | ligation: c + aD <-> caD
            1.0 * x.[174] // BaD | ligation: B + aD <-> BaD
            -1.0 * x.[4] * x.[46] // B + aD | ligation: B + aD <-> BaD
            1.0 * x.[430] // baD | ligation: b + aD <-> baD
            -1.0 * x.[8] * x.[46] // b + aD | ligation: b + aD <-> baD
            1.0 * x.[110] // AaD | ligation: A + aD <-> AaD
            -1.0 * x.[3] * x.[46] // A + aD | ligation: A + aD <-> AaD
            1.0 * x.[366] // aaD | ligation: a + aD <-> aaD
            -1.0 * x.[7] * x.[46] // a + aD | ligation: a + aD <-> aaD
            -1.0 * x.[46] // aD | ligation: a + D <-> aD
            1.0 * x.[7] * x.[6] // a + D | ligation: a + D <-> aD
        |]
        |> Array.sum


    // 47 - aa
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[303] // Daa | ligation: D + aa <-> Daa
            -1.0 * x.[6] * x.[47] // D + aa | ligation: D + aa <-> Daa
            1.0 * x.[559] // daa | ligation: d + aa <-> daa
            -1.0 * x.[10] * x.[47] // d + aa | ligation: d + aa <-> daa
            1.0 * x.[239] // Caa | ligation: C + aa <-> Caa
            -1.0 * x.[5] * x.[47] // C + aa | ligation: C + aa <-> Caa
            1.0 * x.[495] // caa | ligation: c + aa <-> caa
            -1.0 * x.[9] * x.[47] // c + aa | ligation: c + aa <-> caa
            1.0 * x.[175] // Baa | ligation: B + aa <-> Baa
            -1.0 * x.[4] * x.[47] // B + aa | ligation: B + aa <-> Baa
            1.0 * x.[431] // baa | ligation: b + aa <-> baa
            -1.0 * x.[8] * x.[47] // b + aa | ligation: b + aa <-> baa
            1.0 * x.[111] // Aaa | ligation: A + aa <-> Aaa
            -1.0 * x.[3] * x.[47] // A + aa | ligation: A + aa <-> Aaa
            1.0 * x.[367] // aaa | ligation: a + aa <-> aaa
            -1.0 * x.[7] * x.[47] // a + aa | ligation: a + aa <-> aaa
            -1.0 * x.[47] // aa | ligation: a + a <-> aa
            1.0 * x.[7] * x.[7] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 48 - ab
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[304] // Dab | ligation: D + ab <-> Dab
            -1.0 * x.[6] * x.[48] // D + ab | ligation: D + ab <-> Dab
            1.0 * x.[560] // dab | ligation: d + ab <-> dab
            -1.0 * x.[10] * x.[48] // d + ab | ligation: d + ab <-> dab
            1.0 * x.[240] // Cab | ligation: C + ab <-> Cab
            -1.0 * x.[5] * x.[48] // C + ab | ligation: C + ab <-> Cab
            1.0 * x.[496] // cab | ligation: c + ab <-> cab
            -1.0 * x.[9] * x.[48] // c + ab | ligation: c + ab <-> cab
            1.0 * x.[176] // Bab | ligation: B + ab <-> Bab
            -1.0 * x.[4] * x.[48] // B + ab | ligation: B + ab <-> Bab
            1.0 * x.[432] // bab | ligation: b + ab <-> bab
            -1.0 * x.[8] * x.[48] // b + ab | ligation: b + ab <-> bab
            1.0 * x.[112] // Aab | ligation: A + ab <-> Aab
            -1.0 * x.[3] * x.[48] // A + ab | ligation: A + ab <-> Aab
            1.0 * x.[368] // aab | ligation: a + ab <-> aab
            -1.0 * x.[7] * x.[48] // a + ab | ligation: a + ab <-> aab
            -1.0 * x.[48] // ab | ligation: a + b <-> ab
            1.0 * x.[7] * x.[8] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 49 - ac
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[305] // Dac | ligation: D + ac <-> Dac
            -1.0 * x.[6] * x.[49] // D + ac | ligation: D + ac <-> Dac
            1.0 * x.[561] // dac | ligation: d + ac <-> dac
            -1.0 * x.[10] * x.[49] // d + ac | ligation: d + ac <-> dac
            1.0 * x.[241] // Cac | ligation: C + ac <-> Cac
            -1.0 * x.[5] * x.[49] // C + ac | ligation: C + ac <-> Cac
            1.0 * x.[497] // cac | ligation: c + ac <-> cac
            -1.0 * x.[9] * x.[49] // c + ac | ligation: c + ac <-> cac
            1.0 * x.[177] // Bac | ligation: B + ac <-> Bac
            -1.0 * x.[4] * x.[49] // B + ac | ligation: B + ac <-> Bac
            1.0 * x.[433] // bac | ligation: b + ac <-> bac
            -1.0 * x.[8] * x.[49] // b + ac | ligation: b + ac <-> bac
            1.0 * x.[113] // Aac | ligation: A + ac <-> Aac
            -1.0 * x.[3] * x.[49] // A + ac | ligation: A + ac <-> Aac
            1.0 * x.[369] // aac | ligation: a + ac <-> aac
            -1.0 * x.[7] * x.[49] // a + ac | ligation: a + ac <-> aac
            -1.0 * x.[49] // ac | ligation: a + c <-> ac
            1.0 * x.[7] * x.[9] // a + c | ligation: a + c <-> ac
        |]
        |> Array.sum


    // 50 - ad
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[306] // Dad | ligation: D + ad <-> Dad
            -1.0 * x.[6] * x.[50] // D + ad | ligation: D + ad <-> Dad
            1.0 * x.[562] // dad | ligation: d + ad <-> dad
            -1.0 * x.[10] * x.[50] // d + ad | ligation: d + ad <-> dad
            1.0 * x.[242] // Cad | ligation: C + ad <-> Cad
            -1.0 * x.[5] * x.[50] // C + ad | ligation: C + ad <-> Cad
            1.0 * x.[498] // cad | ligation: c + ad <-> cad
            -1.0 * x.[9] * x.[50] // c + ad | ligation: c + ad <-> cad
            1.0 * x.[178] // Bad | ligation: B + ad <-> Bad
            -1.0 * x.[4] * x.[50] // B + ad | ligation: B + ad <-> Bad
            1.0 * x.[434] // bad | ligation: b + ad <-> bad
            -1.0 * x.[8] * x.[50] // b + ad | ligation: b + ad <-> bad
            1.0 * x.[114] // Aad | ligation: A + ad <-> Aad
            -1.0 * x.[3] * x.[50] // A + ad | ligation: A + ad <-> Aad
            1.0 * x.[370] // aad | ligation: a + ad <-> aad
            -1.0 * x.[7] * x.[50] // a + ad | ligation: a + ad <-> aad
            -1.0 * x.[50] // ad | ligation: a + d <-> ad
            1.0 * x.[7] * x.[10] // a + d | ligation: a + d <-> ad
        |]
        |> Array.sum


    // 51 - bA
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[307] // DbA | ligation: D + bA <-> DbA
            -1.0 * x.[6] * x.[51] // D + bA | ligation: D + bA <-> DbA
            1.0 * x.[563] // dbA | ligation: d + bA <-> dbA
            -1.0 * x.[10] * x.[51] // d + bA | ligation: d + bA <-> dbA
            1.0 * x.[243] // CbA | ligation: C + bA <-> CbA
            -1.0 * x.[5] * x.[51] // C + bA | ligation: C + bA <-> CbA
            1.0 * x.[499] // cbA | ligation: c + bA <-> cbA
            -1.0 * x.[9] * x.[51] // c + bA | ligation: c + bA <-> cbA
            1.0 * x.[179] // BbA | ligation: B + bA <-> BbA
            -1.0 * x.[4] * x.[51] // B + bA | ligation: B + bA <-> BbA
            1.0 * x.[435] // bbA | ligation: b + bA <-> bbA
            -1.0 * x.[8] * x.[51] // b + bA | ligation: b + bA <-> bbA
            -1.0 * x.[51] // bA | ligation: b + A <-> bA
            1.0 * x.[8] * x.[3] // b + A | ligation: b + A <-> bA
            1.0 * x.[115] // AbA | ligation: A + bA <-> AbA
            -1.0 * x.[3] * x.[51] // A + bA | ligation: A + bA <-> AbA
            1.0 * x.[371] // abA | ligation: a + bA <-> abA
            -1.0 * x.[7] * x.[51] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 52 - bB
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[308] // DbB | ligation: D + bB <-> DbB
            -1.0 * x.[6] * x.[52] // D + bB | ligation: D + bB <-> DbB
            1.0 * x.[564] // dbB | ligation: d + bB <-> dbB
            -1.0 * x.[10] * x.[52] // d + bB | ligation: d + bB <-> dbB
            1.0 * x.[244] // CbB | ligation: C + bB <-> CbB
            -1.0 * x.[5] * x.[52] // C + bB | ligation: C + bB <-> CbB
            1.0 * x.[500] // cbB | ligation: c + bB <-> cbB
            -1.0 * x.[9] * x.[52] // c + bB | ligation: c + bB <-> cbB
            1.0 * x.[180] // BbB | ligation: B + bB <-> BbB
            -1.0 * x.[4] * x.[52] // B + bB | ligation: B + bB <-> BbB
            1.0 * x.[436] // bbB | ligation: b + bB <-> bbB
            -1.0 * x.[8] * x.[52] // b + bB | ligation: b + bB <-> bbB
            -1.0 * x.[52] // bB | ligation: b + B <-> bB
            1.0 * x.[8] * x.[4] // b + B | ligation: b + B <-> bB
            1.0 * x.[116] // AbB | ligation: A + bB <-> AbB
            -1.0 * x.[3] * x.[52] // A + bB | ligation: A + bB <-> AbB
            1.0 * x.[372] // abB | ligation: a + bB <-> abB
            -1.0 * x.[7] * x.[52] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 53 - bC
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[309] // DbC | ligation: D + bC <-> DbC
            -1.0 * x.[6] * x.[53] // D + bC | ligation: D + bC <-> DbC
            1.0 * x.[565] // dbC | ligation: d + bC <-> dbC
            -1.0 * x.[10] * x.[53] // d + bC | ligation: d + bC <-> dbC
            1.0 * x.[245] // CbC | ligation: C + bC <-> CbC
            -1.0 * x.[5] * x.[53] // C + bC | ligation: C + bC <-> CbC
            1.0 * x.[501] // cbC | ligation: c + bC <-> cbC
            -1.0 * x.[9] * x.[53] // c + bC | ligation: c + bC <-> cbC
            1.0 * x.[181] // BbC | ligation: B + bC <-> BbC
            -1.0 * x.[4] * x.[53] // B + bC | ligation: B + bC <-> BbC
            1.0 * x.[437] // bbC | ligation: b + bC <-> bbC
            -1.0 * x.[8] * x.[53] // b + bC | ligation: b + bC <-> bbC
            -1.0 * x.[53] // bC | ligation: b + C <-> bC
            1.0 * x.[8] * x.[5] // b + C | ligation: b + C <-> bC
            1.0 * x.[117] // AbC | ligation: A + bC <-> AbC
            -1.0 * x.[3] * x.[53] // A + bC | ligation: A + bC <-> AbC
            1.0 * x.[373] // abC | ligation: a + bC <-> abC
            -1.0 * x.[7] * x.[53] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 54 - bD
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[310] // DbD | ligation: D + bD <-> DbD
            -1.0 * x.[6] * x.[54] // D + bD | ligation: D + bD <-> DbD
            1.0 * x.[566] // dbD | ligation: d + bD <-> dbD
            -1.0 * x.[10] * x.[54] // d + bD | ligation: d + bD <-> dbD
            1.0 * x.[246] // CbD | ligation: C + bD <-> CbD
            -1.0 * x.[5] * x.[54] // C + bD | ligation: C + bD <-> CbD
            1.0 * x.[502] // cbD | ligation: c + bD <-> cbD
            -1.0 * x.[9] * x.[54] // c + bD | ligation: c + bD <-> cbD
            1.0 * x.[182] // BbD | ligation: B + bD <-> BbD
            -1.0 * x.[4] * x.[54] // B + bD | ligation: B + bD <-> BbD
            1.0 * x.[438] // bbD | ligation: b + bD <-> bbD
            -1.0 * x.[8] * x.[54] // b + bD | ligation: b + bD <-> bbD
            -1.0 * x.[54] // bD | ligation: b + D <-> bD
            1.0 * x.[8] * x.[6] // b + D | ligation: b + D <-> bD
            1.0 * x.[118] // AbD | ligation: A + bD <-> AbD
            -1.0 * x.[3] * x.[54] // A + bD | ligation: A + bD <-> AbD
            1.0 * x.[374] // abD | ligation: a + bD <-> abD
            -1.0 * x.[7] * x.[54] // a + bD | ligation: a + bD <-> abD
        |]
        |> Array.sum


    // 55 - ba
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[311] // Dba | ligation: D + ba <-> Dba
            -1.0 * x.[6] * x.[55] // D + ba | ligation: D + ba <-> Dba
            1.0 * x.[567] // dba | ligation: d + ba <-> dba
            -1.0 * x.[10] * x.[55] // d + ba | ligation: d + ba <-> dba
            1.0 * x.[247] // Cba | ligation: C + ba <-> Cba
            -1.0 * x.[5] * x.[55] // C + ba | ligation: C + ba <-> Cba
            1.0 * x.[503] // cba | ligation: c + ba <-> cba
            -1.0 * x.[9] * x.[55] // c + ba | ligation: c + ba <-> cba
            1.0 * x.[183] // Bba | ligation: B + ba <-> Bba
            -1.0 * x.[4] * x.[55] // B + ba | ligation: B + ba <-> Bba
            1.0 * x.[439] // bba | ligation: b + ba <-> bba
            -1.0 * x.[8] * x.[55] // b + ba | ligation: b + ba <-> bba
            1.0 * x.[119] // Aba | ligation: A + ba <-> Aba
            -1.0 * x.[3] * x.[55] // A + ba | ligation: A + ba <-> Aba
            1.0 * x.[375] // aba | ligation: a + ba <-> aba
            -1.0 * x.[7] * x.[55] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 56 - bb
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[312] // Dbb | ligation: D + bb <-> Dbb
            -1.0 * x.[6] * x.[56] // D + bb | ligation: D + bb <-> Dbb
            1.0 * x.[568] // dbb | ligation: d + bb <-> dbb
            -1.0 * x.[10] * x.[56] // d + bb | ligation: d + bb <-> dbb
            1.0 * x.[248] // Cbb | ligation: C + bb <-> Cbb
            -1.0 * x.[5] * x.[56] // C + bb | ligation: C + bb <-> Cbb
            1.0 * x.[504] // cbb | ligation: c + bb <-> cbb
            -1.0 * x.[9] * x.[56] // c + bb | ligation: c + bb <-> cbb
            1.0 * x.[184] // Bbb | ligation: B + bb <-> Bbb
            -1.0 * x.[4] * x.[56] // B + bb | ligation: B + bb <-> Bbb
            1.0 * x.[440] // bbb | ligation: b + bb <-> bbb
            -1.0 * x.[8] * x.[56] // b + bb | ligation: b + bb <-> bbb
            -1.0 * x.[56] // bb | ligation: b + b <-> bb
            1.0 * x.[8] * x.[8] // b + b | ligation: b + b <-> bb
            1.0 * x.[120] // Abb | ligation: A + bb <-> Abb
            -1.0 * x.[3] * x.[56] // A + bb | ligation: A + bb <-> Abb
            1.0 * x.[376] // abb | ligation: a + bb <-> abb
            -1.0 * x.[7] * x.[56] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 57 - bc
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[313] // Dbc | ligation: D + bc <-> Dbc
            -1.0 * x.[6] * x.[57] // D + bc | ligation: D + bc <-> Dbc
            1.0 * x.[569] // dbc | ligation: d + bc <-> dbc
            -1.0 * x.[10] * x.[57] // d + bc | ligation: d + bc <-> dbc
            1.0 * x.[249] // Cbc | ligation: C + bc <-> Cbc
            -1.0 * x.[5] * x.[57] // C + bc | ligation: C + bc <-> Cbc
            1.0 * x.[505] // cbc | ligation: c + bc <-> cbc
            -1.0 * x.[9] * x.[57] // c + bc | ligation: c + bc <-> cbc
            1.0 * x.[185] // Bbc | ligation: B + bc <-> Bbc
            -1.0 * x.[4] * x.[57] // B + bc | ligation: B + bc <-> Bbc
            1.0 * x.[441] // bbc | ligation: b + bc <-> bbc
            -1.0 * x.[8] * x.[57] // b + bc | ligation: b + bc <-> bbc
            -1.0 * x.[57] // bc | ligation: b + c <-> bc
            1.0 * x.[8] * x.[9] // b + c | ligation: b + c <-> bc
            1.0 * x.[121] // Abc | ligation: A + bc <-> Abc
            -1.0 * x.[3] * x.[57] // A + bc | ligation: A + bc <-> Abc
            1.0 * x.[377] // abc | ligation: a + bc <-> abc
            -1.0 * x.[7] * x.[57] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 58 - bd
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[314] // Dbd | ligation: D + bd <-> Dbd
            -1.0 * x.[6] * x.[58] // D + bd | ligation: D + bd <-> Dbd
            1.0 * x.[570] // dbd | ligation: d + bd <-> dbd
            -1.0 * x.[10] * x.[58] // d + bd | ligation: d + bd <-> dbd
            1.0 * x.[250] // Cbd | ligation: C + bd <-> Cbd
            -1.0 * x.[5] * x.[58] // C + bd | ligation: C + bd <-> Cbd
            1.0 * x.[506] // cbd | ligation: c + bd <-> cbd
            -1.0 * x.[9] * x.[58] // c + bd | ligation: c + bd <-> cbd
            1.0 * x.[186] // Bbd | ligation: B + bd <-> Bbd
            -1.0 * x.[4] * x.[58] // B + bd | ligation: B + bd <-> Bbd
            1.0 * x.[442] // bbd | ligation: b + bd <-> bbd
            -1.0 * x.[8] * x.[58] // b + bd | ligation: b + bd <-> bbd
            -1.0 * x.[58] // bd | ligation: b + d <-> bd
            1.0 * x.[8] * x.[10] // b + d | ligation: b + d <-> bd
            1.0 * x.[122] // Abd | ligation: A + bd <-> Abd
            -1.0 * x.[3] * x.[58] // A + bd | ligation: A + bd <-> Abd
            1.0 * x.[378] // abd | ligation: a + bd <-> abd
            -1.0 * x.[7] * x.[58] // a + bd | ligation: a + bd <-> abd
        |]
        |> Array.sum


    // 59 - cA
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[315] // DcA | ligation: D + cA <-> DcA
            -1.0 * x.[6] * x.[59] // D + cA | ligation: D + cA <-> DcA
            1.0 * x.[571] // dcA | ligation: d + cA <-> dcA
            -1.0 * x.[10] * x.[59] // d + cA | ligation: d + cA <-> dcA
            1.0 * x.[251] // CcA | ligation: C + cA <-> CcA
            -1.0 * x.[5] * x.[59] // C + cA | ligation: C + cA <-> CcA
            1.0 * x.[507] // ccA | ligation: c + cA <-> ccA
            -1.0 * x.[9] * x.[59] // c + cA | ligation: c + cA <-> ccA
            -1.0 * x.[59] // cA | ligation: c + A <-> cA
            1.0 * x.[9] * x.[3] // c + A | ligation: c + A <-> cA
            1.0 * x.[187] // BcA | ligation: B + cA <-> BcA
            -1.0 * x.[4] * x.[59] // B + cA | ligation: B + cA <-> BcA
            1.0 * x.[443] // bcA | ligation: b + cA <-> bcA
            -1.0 * x.[8] * x.[59] // b + cA | ligation: b + cA <-> bcA
            1.0 * x.[123] // AcA | ligation: A + cA <-> AcA
            -1.0 * x.[3] * x.[59] // A + cA | ligation: A + cA <-> AcA
            1.0 * x.[379] // acA | ligation: a + cA <-> acA
            -1.0 * x.[7] * x.[59] // a + cA | ligation: a + cA <-> acA
        |]
        |> Array.sum


    // 60 - cB
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[316] // DcB | ligation: D + cB <-> DcB
            -1.0 * x.[6] * x.[60] // D + cB | ligation: D + cB <-> DcB
            1.0 * x.[572] // dcB | ligation: d + cB <-> dcB
            -1.0 * x.[10] * x.[60] // d + cB | ligation: d + cB <-> dcB
            1.0 * x.[252] // CcB | ligation: C + cB <-> CcB
            -1.0 * x.[5] * x.[60] // C + cB | ligation: C + cB <-> CcB
            1.0 * x.[508] // ccB | ligation: c + cB <-> ccB
            -1.0 * x.[9] * x.[60] // c + cB | ligation: c + cB <-> ccB
            -1.0 * x.[60] // cB | ligation: c + B <-> cB
            1.0 * x.[9] * x.[4] // c + B | ligation: c + B <-> cB
            1.0 * x.[188] // BcB | ligation: B + cB <-> BcB
            -1.0 * x.[4] * x.[60] // B + cB | ligation: B + cB <-> BcB
            1.0 * x.[444] // bcB | ligation: b + cB <-> bcB
            -1.0 * x.[8] * x.[60] // b + cB | ligation: b + cB <-> bcB
            1.0 * x.[124] // AcB | ligation: A + cB <-> AcB
            -1.0 * x.[3] * x.[60] // A + cB | ligation: A + cB <-> AcB
            1.0 * x.[380] // acB | ligation: a + cB <-> acB
            -1.0 * x.[7] * x.[60] // a + cB | ligation: a + cB <-> acB
        |]
        |> Array.sum


    // 61 - cC
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[317] // DcC | ligation: D + cC <-> DcC
            -1.0 * x.[6] * x.[61] // D + cC | ligation: D + cC <-> DcC
            1.0 * x.[573] // dcC | ligation: d + cC <-> dcC
            -1.0 * x.[10] * x.[61] // d + cC | ligation: d + cC <-> dcC
            1.0 * x.[253] // CcC | ligation: C + cC <-> CcC
            -1.0 * x.[5] * x.[61] // C + cC | ligation: C + cC <-> CcC
            1.0 * x.[509] // ccC | ligation: c + cC <-> ccC
            -1.0 * x.[9] * x.[61] // c + cC | ligation: c + cC <-> ccC
            -1.0 * x.[61] // cC | ligation: c + C <-> cC
            1.0 * x.[9] * x.[5] // c + C | ligation: c + C <-> cC
            1.0 * x.[189] // BcC | ligation: B + cC <-> BcC
            -1.0 * x.[4] * x.[61] // B + cC | ligation: B + cC <-> BcC
            1.0 * x.[445] // bcC | ligation: b + cC <-> bcC
            -1.0 * x.[8] * x.[61] // b + cC | ligation: b + cC <-> bcC
            1.0 * x.[125] // AcC | ligation: A + cC <-> AcC
            -1.0 * x.[3] * x.[61] // A + cC | ligation: A + cC <-> AcC
            1.0 * x.[381] // acC | ligation: a + cC <-> acC
            -1.0 * x.[7] * x.[61] // a + cC | ligation: a + cC <-> acC
        |]
        |> Array.sum


    // 62 - cD
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[318] // DcD | ligation: D + cD <-> DcD
            -1.0 * x.[6] * x.[62] // D + cD | ligation: D + cD <-> DcD
            1.0 * x.[574] // dcD | ligation: d + cD <-> dcD
            -1.0 * x.[10] * x.[62] // d + cD | ligation: d + cD <-> dcD
            1.0 * x.[254] // CcD | ligation: C + cD <-> CcD
            -1.0 * x.[5] * x.[62] // C + cD | ligation: C + cD <-> CcD
            1.0 * x.[510] // ccD | ligation: c + cD <-> ccD
            -1.0 * x.[9] * x.[62] // c + cD | ligation: c + cD <-> ccD
            -1.0 * x.[62] // cD | ligation: c + D <-> cD
            1.0 * x.[9] * x.[6] // c + D | ligation: c + D <-> cD
            1.0 * x.[190] // BcD | ligation: B + cD <-> BcD
            -1.0 * x.[4] * x.[62] // B + cD | ligation: B + cD <-> BcD
            1.0 * x.[446] // bcD | ligation: b + cD <-> bcD
            -1.0 * x.[8] * x.[62] // b + cD | ligation: b + cD <-> bcD
            1.0 * x.[126] // AcD | ligation: A + cD <-> AcD
            -1.0 * x.[3] * x.[62] // A + cD | ligation: A + cD <-> AcD
            1.0 * x.[382] // acD | ligation: a + cD <-> acD
            -1.0 * x.[7] * x.[62] // a + cD | ligation: a + cD <-> acD
        |]
        |> Array.sum


    // 63 - ca
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[319] // Dca | ligation: D + ca <-> Dca
            -1.0 * x.[6] * x.[63] // D + ca | ligation: D + ca <-> Dca
            1.0 * x.[575] // dca | ligation: d + ca <-> dca
            -1.0 * x.[10] * x.[63] // d + ca | ligation: d + ca <-> dca
            1.0 * x.[255] // Cca | ligation: C + ca <-> Cca
            -1.0 * x.[5] * x.[63] // C + ca | ligation: C + ca <-> Cca
            1.0 * x.[511] // cca | ligation: c + ca <-> cca
            -1.0 * x.[9] * x.[63] // c + ca | ligation: c + ca <-> cca
            1.0 * x.[191] // Bca | ligation: B + ca <-> Bca
            -1.0 * x.[4] * x.[63] // B + ca | ligation: B + ca <-> Bca
            1.0 * x.[447] // bca | ligation: b + ca <-> bca
            -1.0 * x.[8] * x.[63] // b + ca | ligation: b + ca <-> bca
            1.0 * x.[127] // Aca | ligation: A + ca <-> Aca
            -1.0 * x.[3] * x.[63] // A + ca | ligation: A + ca <-> Aca
            1.0 * x.[383] // aca | ligation: a + ca <-> aca
            -1.0 * x.[7] * x.[63] // a + ca | ligation: a + ca <-> aca
        |]
        |> Array.sum


    // 64 - cb
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[320] // Dcb | ligation: D + cb <-> Dcb
            -1.0 * x.[6] * x.[64] // D + cb | ligation: D + cb <-> Dcb
            1.0 * x.[576] // dcb | ligation: d + cb <-> dcb
            -1.0 * x.[10] * x.[64] // d + cb | ligation: d + cb <-> dcb
            1.0 * x.[256] // Ccb | ligation: C + cb <-> Ccb
            -1.0 * x.[5] * x.[64] // C + cb | ligation: C + cb <-> Ccb
            1.0 * x.[512] // ccb | ligation: c + cb <-> ccb
            -1.0 * x.[9] * x.[64] // c + cb | ligation: c + cb <-> ccb
            1.0 * x.[192] // Bcb | ligation: B + cb <-> Bcb
            -1.0 * x.[4] * x.[64] // B + cb | ligation: B + cb <-> Bcb
            1.0 * x.[448] // bcb | ligation: b + cb <-> bcb
            -1.0 * x.[8] * x.[64] // b + cb | ligation: b + cb <-> bcb
            1.0 * x.[128] // Acb | ligation: A + cb <-> Acb
            -1.0 * x.[3] * x.[64] // A + cb | ligation: A + cb <-> Acb
            1.0 * x.[384] // acb | ligation: a + cb <-> acb
            -1.0 * x.[7] * x.[64] // a + cb | ligation: a + cb <-> acb
        |]
        |> Array.sum


    // 65 - cc
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[321] // Dcc | ligation: D + cc <-> Dcc
            -1.0 * x.[6] * x.[65] // D + cc | ligation: D + cc <-> Dcc
            1.0 * x.[577] // dcc | ligation: d + cc <-> dcc
            -1.0 * x.[10] * x.[65] // d + cc | ligation: d + cc <-> dcc
            1.0 * x.[257] // Ccc | ligation: C + cc <-> Ccc
            -1.0 * x.[5] * x.[65] // C + cc | ligation: C + cc <-> Ccc
            1.0 * x.[513] // ccc | ligation: c + cc <-> ccc
            -1.0 * x.[9] * x.[65] // c + cc | ligation: c + cc <-> ccc
            -1.0 * x.[65] // cc | ligation: c + c <-> cc
            1.0 * x.[9] * x.[9] // c + c | ligation: c + c <-> cc
            1.0 * x.[193] // Bcc | ligation: B + cc <-> Bcc
            -1.0 * x.[4] * x.[65] // B + cc | ligation: B + cc <-> Bcc
            1.0 * x.[449] // bcc | ligation: b + cc <-> bcc
            -1.0 * x.[8] * x.[65] // b + cc | ligation: b + cc <-> bcc
            1.0 * x.[129] // Acc | ligation: A + cc <-> Acc
            -1.0 * x.[3] * x.[65] // A + cc | ligation: A + cc <-> Acc
            1.0 * x.[385] // acc | ligation: a + cc <-> acc
            -1.0 * x.[7] * x.[65] // a + cc | ligation: a + cc <-> acc
        |]
        |> Array.sum


    // 66 - cd
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[322] // Dcd | ligation: D + cd <-> Dcd
            -1.0 * x.[6] * x.[66] // D + cd | ligation: D + cd <-> Dcd
            1.0 * x.[578] // dcd | ligation: d + cd <-> dcd
            -1.0 * x.[10] * x.[66] // d + cd | ligation: d + cd <-> dcd
            1.0 * x.[258] // Ccd | ligation: C + cd <-> Ccd
            -1.0 * x.[5] * x.[66] // C + cd | ligation: C + cd <-> Ccd
            1.0 * x.[514] // ccd | ligation: c + cd <-> ccd
            -1.0 * x.[9] * x.[66] // c + cd | ligation: c + cd <-> ccd
            -1.0 * x.[66] // cd | ligation: c + d <-> cd
            1.0 * x.[9] * x.[10] // c + d | ligation: c + d <-> cd
            1.0 * x.[194] // Bcd | ligation: B + cd <-> Bcd
            -1.0 * x.[4] * x.[66] // B + cd | ligation: B + cd <-> Bcd
            1.0 * x.[450] // bcd | ligation: b + cd <-> bcd
            -1.0 * x.[8] * x.[66] // b + cd | ligation: b + cd <-> bcd
            1.0 * x.[130] // Acd | ligation: A + cd <-> Acd
            -1.0 * x.[3] * x.[66] // A + cd | ligation: A + cd <-> Acd
            1.0 * x.[386] // acd | ligation: a + cd <-> acd
            -1.0 * x.[7] * x.[66] // a + cd | ligation: a + cd <-> acd
        |]
        |> Array.sum


    // 67 - dA
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[323] // DdA | ligation: D + dA <-> DdA
            -1.0 * x.[6] * x.[67] // D + dA | ligation: D + dA <-> DdA
            1.0 * x.[579] // ddA | ligation: d + dA <-> ddA
            -1.0 * x.[10] * x.[67] // d + dA | ligation: d + dA <-> ddA
            -1.0 * x.[67] // dA | ligation: d + A <-> dA
            1.0 * x.[10] * x.[3] // d + A | ligation: d + A <-> dA
            1.0 * x.[259] // CdA | ligation: C + dA <-> CdA
            -1.0 * x.[5] * x.[67] // C + dA | ligation: C + dA <-> CdA
            1.0 * x.[515] // cdA | ligation: c + dA <-> cdA
            -1.0 * x.[9] * x.[67] // c + dA | ligation: c + dA <-> cdA
            1.0 * x.[195] // BdA | ligation: B + dA <-> BdA
            -1.0 * x.[4] * x.[67] // B + dA | ligation: B + dA <-> BdA
            1.0 * x.[451] // bdA | ligation: b + dA <-> bdA
            -1.0 * x.[8] * x.[67] // b + dA | ligation: b + dA <-> bdA
            1.0 * x.[131] // AdA | ligation: A + dA <-> AdA
            -1.0 * x.[3] * x.[67] // A + dA | ligation: A + dA <-> AdA
            1.0 * x.[387] // adA | ligation: a + dA <-> adA
            -1.0 * x.[7] * x.[67] // a + dA | ligation: a + dA <-> adA
        |]
        |> Array.sum


    // 68 - dB
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[324] // DdB | ligation: D + dB <-> DdB
            -1.0 * x.[6] * x.[68] // D + dB | ligation: D + dB <-> DdB
            1.0 * x.[580] // ddB | ligation: d + dB <-> ddB
            -1.0 * x.[10] * x.[68] // d + dB | ligation: d + dB <-> ddB
            -1.0 * x.[68] // dB | ligation: d + B <-> dB
            1.0 * x.[10] * x.[4] // d + B | ligation: d + B <-> dB
            1.0 * x.[260] // CdB | ligation: C + dB <-> CdB
            -1.0 * x.[5] * x.[68] // C + dB | ligation: C + dB <-> CdB
            1.0 * x.[516] // cdB | ligation: c + dB <-> cdB
            -1.0 * x.[9] * x.[68] // c + dB | ligation: c + dB <-> cdB
            1.0 * x.[196] // BdB | ligation: B + dB <-> BdB
            -1.0 * x.[4] * x.[68] // B + dB | ligation: B + dB <-> BdB
            1.0 * x.[452] // bdB | ligation: b + dB <-> bdB
            -1.0 * x.[8] * x.[68] // b + dB | ligation: b + dB <-> bdB
            1.0 * x.[132] // AdB | ligation: A + dB <-> AdB
            -1.0 * x.[3] * x.[68] // A + dB | ligation: A + dB <-> AdB
            1.0 * x.[388] // adB | ligation: a + dB <-> adB
            -1.0 * x.[7] * x.[68] // a + dB | ligation: a + dB <-> adB
        |]
        |> Array.sum


    // 69 - dC
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[325] // DdC | ligation: D + dC <-> DdC
            -1.0 * x.[6] * x.[69] // D + dC | ligation: D + dC <-> DdC
            1.0 * x.[581] // ddC | ligation: d + dC <-> ddC
            -1.0 * x.[10] * x.[69] // d + dC | ligation: d + dC <-> ddC
            -1.0 * x.[69] // dC | ligation: d + C <-> dC
            1.0 * x.[10] * x.[5] // d + C | ligation: d + C <-> dC
            1.0 * x.[261] // CdC | ligation: C + dC <-> CdC
            -1.0 * x.[5] * x.[69] // C + dC | ligation: C + dC <-> CdC
            1.0 * x.[517] // cdC | ligation: c + dC <-> cdC
            -1.0 * x.[9] * x.[69] // c + dC | ligation: c + dC <-> cdC
            1.0 * x.[197] // BdC | ligation: B + dC <-> BdC
            -1.0 * x.[4] * x.[69] // B + dC | ligation: B + dC <-> BdC
            1.0 * x.[453] // bdC | ligation: b + dC <-> bdC
            -1.0 * x.[8] * x.[69] // b + dC | ligation: b + dC <-> bdC
            1.0 * x.[133] // AdC | ligation: A + dC <-> AdC
            -1.0 * x.[3] * x.[69] // A + dC | ligation: A + dC <-> AdC
            1.0 * x.[389] // adC | ligation: a + dC <-> adC
            -1.0 * x.[7] * x.[69] // a + dC | ligation: a + dC <-> adC
        |]
        |> Array.sum


    // 70 - dD
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[326] // DdD | ligation: D + dD <-> DdD
            -1.0 * x.[6] * x.[70] // D + dD | ligation: D + dD <-> DdD
            1.0 * x.[582] // ddD | ligation: d + dD <-> ddD
            -1.0 * x.[10] * x.[70] // d + dD | ligation: d + dD <-> ddD
            -1.0 * x.[70] // dD | ligation: d + D <-> dD
            1.0 * x.[10] * x.[6] // d + D | ligation: d + D <-> dD
            1.0 * x.[262] // CdD | ligation: C + dD <-> CdD
            -1.0 * x.[5] * x.[70] // C + dD | ligation: C + dD <-> CdD
            1.0 * x.[518] // cdD | ligation: c + dD <-> cdD
            -1.0 * x.[9] * x.[70] // c + dD | ligation: c + dD <-> cdD
            1.0 * x.[198] // BdD | ligation: B + dD <-> BdD
            -1.0 * x.[4] * x.[70] // B + dD | ligation: B + dD <-> BdD
            1.0 * x.[454] // bdD | ligation: b + dD <-> bdD
            -1.0 * x.[8] * x.[70] // b + dD | ligation: b + dD <-> bdD
            1.0 * x.[134] // AdD | ligation: A + dD <-> AdD
            -1.0 * x.[3] * x.[70] // A + dD | ligation: A + dD <-> AdD
            1.0 * x.[390] // adD | ligation: a + dD <-> adD
            -1.0 * x.[7] * x.[70] // a + dD | ligation: a + dD <-> adD
        |]
        |> Array.sum


    // 71 - da
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[327] // Dda | ligation: D + da <-> Dda
            -1.0 * x.[6] * x.[71] // D + da | ligation: D + da <-> Dda
            1.0 * x.[583] // dda | ligation: d + da <-> dda
            -1.0 * x.[10] * x.[71] // d + da | ligation: d + da <-> dda
            1.0 * x.[263] // Cda | ligation: C + da <-> Cda
            -1.0 * x.[5] * x.[71] // C + da | ligation: C + da <-> Cda
            1.0 * x.[519] // cda | ligation: c + da <-> cda
            -1.0 * x.[9] * x.[71] // c + da | ligation: c + da <-> cda
            1.0 * x.[199] // Bda | ligation: B + da <-> Bda
            -1.0 * x.[4] * x.[71] // B + da | ligation: B + da <-> Bda
            1.0 * x.[455] // bda | ligation: b + da <-> bda
            -1.0 * x.[8] * x.[71] // b + da | ligation: b + da <-> bda
            1.0 * x.[135] // Ada | ligation: A + da <-> Ada
            -1.0 * x.[3] * x.[71] // A + da | ligation: A + da <-> Ada
            1.0 * x.[391] // ada | ligation: a + da <-> ada
            -1.0 * x.[7] * x.[71] // a + da | ligation: a + da <-> ada
        |]
        |> Array.sum


    // 72 - db
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[328] // Ddb | ligation: D + db <-> Ddb
            -1.0 * x.[6] * x.[72] // D + db | ligation: D + db <-> Ddb
            1.0 * x.[584] // ddb | ligation: d + db <-> ddb
            -1.0 * x.[10] * x.[72] // d + db | ligation: d + db <-> ddb
            1.0 * x.[264] // Cdb | ligation: C + db <-> Cdb
            -1.0 * x.[5] * x.[72] // C + db | ligation: C + db <-> Cdb
            1.0 * x.[520] // cdb | ligation: c + db <-> cdb
            -1.0 * x.[9] * x.[72] // c + db | ligation: c + db <-> cdb
            1.0 * x.[200] // Bdb | ligation: B + db <-> Bdb
            -1.0 * x.[4] * x.[72] // B + db | ligation: B + db <-> Bdb
            1.0 * x.[456] // bdb | ligation: b + db <-> bdb
            -1.0 * x.[8] * x.[72] // b + db | ligation: b + db <-> bdb
            1.0 * x.[136] // Adb | ligation: A + db <-> Adb
            -1.0 * x.[3] * x.[72] // A + db | ligation: A + db <-> Adb
            1.0 * x.[392] // adb | ligation: a + db <-> adb
            -1.0 * x.[7] * x.[72] // a + db | ligation: a + db <-> adb
        |]
        |> Array.sum


    // 73 - dc
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[329] // Ddc | ligation: D + dc <-> Ddc
            -1.0 * x.[6] * x.[73] // D + dc | ligation: D + dc <-> Ddc
            1.0 * x.[585] // ddc | ligation: d + dc <-> ddc
            -1.0 * x.[10] * x.[73] // d + dc | ligation: d + dc <-> ddc
            1.0 * x.[265] // Cdc | ligation: C + dc <-> Cdc
            -1.0 * x.[5] * x.[73] // C + dc | ligation: C + dc <-> Cdc
            1.0 * x.[521] // cdc | ligation: c + dc <-> cdc
            -1.0 * x.[9] * x.[73] // c + dc | ligation: c + dc <-> cdc
            1.0 * x.[201] // Bdc | ligation: B + dc <-> Bdc
            -1.0 * x.[4] * x.[73] // B + dc | ligation: B + dc <-> Bdc
            1.0 * x.[457] // bdc | ligation: b + dc <-> bdc
            -1.0 * x.[8] * x.[73] // b + dc | ligation: b + dc <-> bdc
            1.0 * x.[137] // Adc | ligation: A + dc <-> Adc
            -1.0 * x.[3] * x.[73] // A + dc | ligation: A + dc <-> Adc
            1.0 * x.[393] // adc | ligation: a + dc <-> adc
            -1.0 * x.[7] * x.[73] // a + dc | ligation: a + dc <-> adc
        |]
        |> Array.sum


    // 74 - dd
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            1.0 * x.[330] // Ddd | ligation: D + dd <-> Ddd
            -1.0 * x.[6] * x.[74] // D + dd | ligation: D + dd <-> Ddd
            1.0 * x.[586] // ddd | ligation: d + dd <-> ddd
            -1.0 * x.[10] * x.[74] // d + dd | ligation: d + dd <-> ddd
            -1.0 * x.[74] // dd | ligation: d + d <-> dd
            1.0 * x.[10] * x.[10] // d + d | ligation: d + d <-> dd
            1.0 * x.[266] // Cdd | ligation: C + dd <-> Cdd
            -1.0 * x.[5] * x.[74] // C + dd | ligation: C + dd <-> Cdd
            1.0 * x.[522] // cdd | ligation: c + dd <-> cdd
            -1.0 * x.[9] * x.[74] // c + dd | ligation: c + dd <-> cdd
            1.0 * x.[202] // Bdd | ligation: B + dd <-> Bdd
            -1.0 * x.[4] * x.[74] // B + dd | ligation: B + dd <-> Bdd
            1.0 * x.[458] // bdd | ligation: b + dd <-> bdd
            -1.0 * x.[8] * x.[74] // b + dd | ligation: b + dd <-> bdd
            1.0 * x.[138] // Add | ligation: A + dd <-> Add
            -1.0 * x.[3] * x.[74] // A + dd | ligation: A + dd <-> Add
            1.0 * x.[394] // add | ligation: a + dd <-> add
            -1.0 * x.[7] * x.[74] // a + dd | ligation: a + dd <-> add
        |]
        |> Array.sum


    // 75 - AAA
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[75] // AAA | ligation: A + AA <-> AAA
            1.0 * x.[3] * x.[11] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 76 - AAB
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[76] // AAB | ligation: A + AB <-> AAB
            1.0 * x.[3] * x.[12] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 77 - AAC
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[77] // AAC | ligation: A + AC <-> AAC
            1.0 * x.[3] * x.[13] // A + AC | ligation: A + AC <-> AAC
        |]
        |> Array.sum


    // 78 - AAD
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[78] // AAD | ligation: A + AD <-> AAD
            1.0 * x.[3] * x.[14] // A + AD | ligation: A + AD <-> AAD
        |]
        |> Array.sum


    // 79 - AAa
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[79] // AAa | ligation: A + Aa <-> AAa
            1.0 * x.[3] * x.[15] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 80 - AAb
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[80] // AAb | ligation: A + Ab <-> AAb
            1.0 * x.[3] * x.[16] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 81 - AAc
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[81] // AAc | ligation: A + Ac <-> AAc
            1.0 * x.[3] * x.[17] // A + Ac | ligation: A + Ac <-> AAc
        |]
        |> Array.sum


    // 82 - AAd
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[82] // AAd | ligation: A + Ad <-> AAd
            1.0 * x.[3] * x.[18] // A + Ad | ligation: A + Ad <-> AAd
        |]
        |> Array.sum


    // 83 - ABA
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[83] // ABA | ligation: A + BA <-> ABA
            1.0 * x.[3] * x.[19] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 84 - ABB
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[84] // ABB | ligation: A + BB <-> ABB
            1.0 * x.[3] * x.[20] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 85 - ABC
    let d85 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[85] // ABC | ligation: A + BC <-> ABC
            1.0 * x.[3] * x.[21] // A + BC | ligation: A + BC <-> ABC
        |]
        |> Array.sum


    // 86 - ABD
    let d86 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[86] // ABD | ligation: A + BD <-> ABD
            1.0 * x.[3] * x.[22] // A + BD | ligation: A + BD <-> ABD
        |]
        |> Array.sum


    // 87 - ABa
    let d87 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[87] // ABa | ligation: A + Ba <-> ABa
            1.0 * x.[3] * x.[23] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 88 - ABb
    let d88 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[88] // ABb | ligation: A + Bb <-> ABb
            1.0 * x.[3] * x.[24] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 89 - ABc
    let d89 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[89] // ABc | ligation: A + Bc <-> ABc
            1.0 * x.[3] * x.[25] // A + Bc | ligation: A + Bc <-> ABc
        |]
        |> Array.sum


    // 90 - ABd
    let d90 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[90] // ABd | ligation: A + Bd <-> ABd
            1.0 * x.[3] * x.[26] // A + Bd | ligation: A + Bd <-> ABd
        |]
        |> Array.sum


    // 91 - ACA
    let d91 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[91] // ACA | ligation: A + CA <-> ACA
            1.0 * x.[3] * x.[27] // A + CA | ligation: A + CA <-> ACA
        |]
        |> Array.sum


    // 92 - ACB
    let d92 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[92] // ACB | ligation: A + CB <-> ACB
            1.0 * x.[3] * x.[28] // A + CB | ligation: A + CB <-> ACB
        |]
        |> Array.sum


    // 93 - ACC
    let d93 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[93] // ACC | ligation: A + CC <-> ACC
            1.0 * x.[3] * x.[29] // A + CC | ligation: A + CC <-> ACC
        |]
        |> Array.sum


    // 94 - ACD
    let d94 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[94] // ACD | ligation: A + CD <-> ACD
            1.0 * x.[3] * x.[30] // A + CD | ligation: A + CD <-> ACD
        |]
        |> Array.sum


    // 95 - ACa
    let d95 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[95] // ACa | ligation: A + Ca <-> ACa
            1.0 * x.[3] * x.[31] // A + Ca | ligation: A + Ca <-> ACa
        |]
        |> Array.sum


    // 96 - ACb
    let d96 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[96] // ACb | ligation: A + Cb <-> ACb
            1.0 * x.[3] * x.[32] // A + Cb | ligation: A + Cb <-> ACb
        |]
        |> Array.sum


    // 97 - ACc
    let d97 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[97] // ACc | ligation: A + Cc <-> ACc
            1.0 * x.[3] * x.[33] // A + Cc | ligation: A + Cc <-> ACc
        |]
        |> Array.sum


    // 98 - ACd
    let d98 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[98] // ACd | ligation: A + Cd <-> ACd
            1.0 * x.[3] * x.[34] // A + Cd | ligation: A + Cd <-> ACd
        |]
        |> Array.sum


    // 99 - ADA
    let d99 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[99] // ADA | ligation: A + DA <-> ADA
            1.0 * x.[3] * x.[35] // A + DA | ligation: A + DA <-> ADA
        |]
        |> Array.sum


    // 100 - ADB
    let d100 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[100] // ADB | ligation: A + DB <-> ADB
            1.0 * x.[3] * x.[36] // A + DB | ligation: A + DB <-> ADB
        |]
        |> Array.sum


    // 101 - ADC
    let d101 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[101] // ADC | ligation: A + DC <-> ADC
            1.0 * x.[3] * x.[37] // A + DC | ligation: A + DC <-> ADC
        |]
        |> Array.sum


    // 102 - ADD
    let d102 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[102] // ADD | ligation: A + DD <-> ADD
            1.0 * x.[3] * x.[38] // A + DD | ligation: A + DD <-> ADD
        |]
        |> Array.sum


    // 103 - ADa
    let d103 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[103] // ADa | ligation: A + Da <-> ADa
            1.0 * x.[3] * x.[39] // A + Da | ligation: A + Da <-> ADa
        |]
        |> Array.sum


    // 104 - ADb
    let d104 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[104] // ADb | ligation: A + Db <-> ADb
            1.0 * x.[3] * x.[40] // A + Db | ligation: A + Db <-> ADb
        |]
        |> Array.sum


    // 105 - ADc
    let d105 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[105] // ADc | ligation: A + Dc <-> ADc
            1.0 * x.[3] * x.[41] // A + Dc | ligation: A + Dc <-> ADc
        |]
        |> Array.sum


    // 106 - ADd
    let d106 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[106] // ADd | ligation: A + Dd <-> ADd
            1.0 * x.[3] * x.[42] // A + Dd | ligation: A + Dd <-> ADd
        |]
        |> Array.sum


    // 107 - AaA
    let d107 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[107] // AaA | ligation: A + aA <-> AaA
            1.0 * x.[3] * x.[43] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 108 - AaB
    let d108 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[108] // AaB | ligation: A + aB <-> AaB
            1.0 * x.[3] * x.[44] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 109 - AaC
    let d109 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[109] // AaC | ligation: A + aC <-> AaC
            1.0 * x.[3] * x.[45] // A + aC | ligation: A + aC <-> AaC
        |]
        |> Array.sum


    // 110 - AaD
    let d110 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[110] // AaD | ligation: A + aD <-> AaD
            1.0 * x.[3] * x.[46] // A + aD | ligation: A + aD <-> AaD
        |]
        |> Array.sum


    // 111 - Aaa
    let d111 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[111] // Aaa | ligation: A + aa <-> Aaa
            1.0 * x.[3] * x.[47] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 112 - Aab
    let d112 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[112] // Aab | ligation: A + ab <-> Aab
            1.0 * x.[3] * x.[48] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 113 - Aac
    let d113 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[113] // Aac | ligation: A + ac <-> Aac
            1.0 * x.[3] * x.[49] // A + ac | ligation: A + ac <-> Aac
        |]
        |> Array.sum


    // 114 - Aad
    let d114 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[114] // Aad | ligation: A + ad <-> Aad
            1.0 * x.[3] * x.[50] // A + ad | ligation: A + ad <-> Aad
        |]
        |> Array.sum


    // 115 - AbA
    let d115 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[115] // AbA | ligation: A + bA <-> AbA
            1.0 * x.[3] * x.[51] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 116 - AbB
    let d116 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[116] // AbB | ligation: A + bB <-> AbB
            1.0 * x.[3] * x.[52] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 117 - AbC
    let d117 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[117] // AbC | ligation: A + bC <-> AbC
            1.0 * x.[3] * x.[53] // A + bC | ligation: A + bC <-> AbC
        |]
        |> Array.sum


    // 118 - AbD
    let d118 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[118] // AbD | ligation: A + bD <-> AbD
            1.0 * x.[3] * x.[54] // A + bD | ligation: A + bD <-> AbD
        |]
        |> Array.sum


    // 119 - Aba
    let d119 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[119] // Aba | ligation: A + ba <-> Aba
            1.0 * x.[3] * x.[55] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 120 - Abb
    let d120 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[120] // Abb | ligation: A + bb <-> Abb
            1.0 * x.[3] * x.[56] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 121 - Abc
    let d121 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[121] // Abc | ligation: A + bc <-> Abc
            1.0 * x.[3] * x.[57] // A + bc | ligation: A + bc <-> Abc
        |]
        |> Array.sum


    // 122 - Abd
    let d122 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[122] // Abd | ligation: A + bd <-> Abd
            1.0 * x.[3] * x.[58] // A + bd | ligation: A + bd <-> Abd
        |]
        |> Array.sum


    // 123 - AcA
    let d123 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[123] // AcA | ligation: A + cA <-> AcA
            1.0 * x.[3] * x.[59] // A + cA | ligation: A + cA <-> AcA
        |]
        |> Array.sum


    // 124 - AcB
    let d124 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[124] // AcB | ligation: A + cB <-> AcB
            1.0 * x.[3] * x.[60] // A + cB | ligation: A + cB <-> AcB
        |]
        |> Array.sum


    // 125 - AcC
    let d125 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[125] // AcC | ligation: A + cC <-> AcC
            1.0 * x.[3] * x.[61] // A + cC | ligation: A + cC <-> AcC
        |]
        |> Array.sum


    // 126 - AcD
    let d126 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[126] // AcD | ligation: A + cD <-> AcD
            1.0 * x.[3] * x.[62] // A + cD | ligation: A + cD <-> AcD
        |]
        |> Array.sum


    // 127 - Aca
    let d127 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[127] // Aca | ligation: A + ca <-> Aca
            1.0 * x.[3] * x.[63] // A + ca | ligation: A + ca <-> Aca
        |]
        |> Array.sum


    // 128 - Acb
    let d128 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[128] // Acb | ligation: A + cb <-> Acb
            1.0 * x.[3] * x.[64] // A + cb | ligation: A + cb <-> Acb
        |]
        |> Array.sum


    // 129 - Acc
    let d129 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[129] // Acc | ligation: A + cc <-> Acc
            1.0 * x.[3] * x.[65] // A + cc | ligation: A + cc <-> Acc
        |]
        |> Array.sum


    // 130 - Acd
    let d130 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[130] // Acd | ligation: A + cd <-> Acd
            1.0 * x.[3] * x.[66] // A + cd | ligation: A + cd <-> Acd
        |]
        |> Array.sum


    // 131 - AdA
    let d131 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[131] // AdA | ligation: A + dA <-> AdA
            1.0 * x.[3] * x.[67] // A + dA | ligation: A + dA <-> AdA
        |]
        |> Array.sum


    // 132 - AdB
    let d132 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[132] // AdB | ligation: A + dB <-> AdB
            1.0 * x.[3] * x.[68] // A + dB | ligation: A + dB <-> AdB
        |]
        |> Array.sum


    // 133 - AdC
    let d133 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[133] // AdC | ligation: A + dC <-> AdC
            1.0 * x.[3] * x.[69] // A + dC | ligation: A + dC <-> AdC
        |]
        |> Array.sum


    // 134 - AdD
    let d134 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[134] // AdD | ligation: A + dD <-> AdD
            1.0 * x.[3] * x.[70] // A + dD | ligation: A + dD <-> AdD
        |]
        |> Array.sum


    // 135 - Ada
    let d135 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[135] // Ada | ligation: A + da <-> Ada
            1.0 * x.[3] * x.[71] // A + da | ligation: A + da <-> Ada
        |]
        |> Array.sum


    // 136 - Adb
    let d136 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[136] // Adb | ligation: A + db <-> Adb
            1.0 * x.[3] * x.[72] // A + db | ligation: A + db <-> Adb
        |]
        |> Array.sum


    // 137 - Adc
    let d137 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[137] // Adc | ligation: A + dc <-> Adc
            1.0 * x.[3] * x.[73] // A + dc | ligation: A + dc <-> Adc
        |]
        |> Array.sum


    // 138 - Add
    let d138 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[138] // Add | ligation: A + dd <-> Add
            1.0 * x.[3] * x.[74] // A + dd | ligation: A + dd <-> Add
        |]
        |> Array.sum


    // 139 - BAA
    let d139 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[139] // BAA | ligation: B + AA <-> BAA
            1.0 * x.[4] * x.[11] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 140 - BAB
    let d140 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[140] // BAB | ligation: B + AB <-> BAB
            1.0 * x.[4] * x.[12] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 141 - BAC
    let d141 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[141] // BAC | ligation: B + AC <-> BAC
            1.0 * x.[4] * x.[13] // B + AC | ligation: B + AC <-> BAC
        |]
        |> Array.sum


    // 142 - BAD
    let d142 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[142] // BAD | ligation: B + AD <-> BAD
            1.0 * x.[4] * x.[14] // B + AD | ligation: B + AD <-> BAD
        |]
        |> Array.sum


    // 143 - BAa
    let d143 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[143] // BAa | ligation: B + Aa <-> BAa
            1.0 * x.[4] * x.[15] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 144 - BAb
    let d144 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[144] // BAb | ligation: B + Ab <-> BAb
            1.0 * x.[4] * x.[16] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 145 - BAc
    let d145 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[145] // BAc | ligation: B + Ac <-> BAc
            1.0 * x.[4] * x.[17] // B + Ac | ligation: B + Ac <-> BAc
        |]
        |> Array.sum


    // 146 - BAd
    let d146 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[146] // BAd | ligation: B + Ad <-> BAd
            1.0 * x.[4] * x.[18] // B + Ad | ligation: B + Ad <-> BAd
        |]
        |> Array.sum


    // 147 - BBA
    let d147 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[147] // BBA | ligation: B + BA <-> BBA
            1.0 * x.[4] * x.[19] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 148 - BBB
    let d148 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[148] // BBB | ligation: B + BB <-> BBB
            1.0 * x.[4] * x.[20] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 149 - BBC
    let d149 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[149] // BBC | ligation: B + BC <-> BBC
            1.0 * x.[4] * x.[21] // B + BC | ligation: B + BC <-> BBC
        |]
        |> Array.sum


    // 150 - BBD
    let d150 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[150] // BBD | ligation: B + BD <-> BBD
            1.0 * x.[4] * x.[22] // B + BD | ligation: B + BD <-> BBD
        |]
        |> Array.sum


    // 151 - BBa
    let d151 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[151] // BBa | ligation: B + Ba <-> BBa
            1.0 * x.[4] * x.[23] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 152 - BBb
    let d152 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[152] // BBb | ligation: B + Bb <-> BBb
            1.0 * x.[4] * x.[24] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 153 - BBc
    let d153 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[153] // BBc | ligation: B + Bc <-> BBc
            1.0 * x.[4] * x.[25] // B + Bc | ligation: B + Bc <-> BBc
        |]
        |> Array.sum


    // 154 - BBd
    let d154 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[154] // BBd | ligation: B + Bd <-> BBd
            1.0 * x.[4] * x.[26] // B + Bd | ligation: B + Bd <-> BBd
        |]
        |> Array.sum


    // 155 - BCA
    let d155 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[155] // BCA | ligation: B + CA <-> BCA
            1.0 * x.[4] * x.[27] // B + CA | ligation: B + CA <-> BCA
        |]
        |> Array.sum


    // 156 - BCB
    let d156 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[156] // BCB | ligation: B + CB <-> BCB
            1.0 * x.[4] * x.[28] // B + CB | ligation: B + CB <-> BCB
        |]
        |> Array.sum


    // 157 - BCC
    let d157 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[157] // BCC | ligation: B + CC <-> BCC
            1.0 * x.[4] * x.[29] // B + CC | ligation: B + CC <-> BCC
        |]
        |> Array.sum


    // 158 - BCD
    let d158 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[158] // BCD | ligation: B + CD <-> BCD
            1.0 * x.[4] * x.[30] // B + CD | ligation: B + CD <-> BCD
        |]
        |> Array.sum


    // 159 - BCa
    let d159 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[159] // BCa | ligation: B + Ca <-> BCa
            1.0 * x.[4] * x.[31] // B + Ca | ligation: B + Ca <-> BCa
        |]
        |> Array.sum


    // 160 - BCb
    let d160 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[160] // BCb | ligation: B + Cb <-> BCb
            1.0 * x.[4] * x.[32] // B + Cb | ligation: B + Cb <-> BCb
        |]
        |> Array.sum


    // 161 - BCc
    let d161 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[161] // BCc | ligation: B + Cc <-> BCc
            1.0 * x.[4] * x.[33] // B + Cc | ligation: B + Cc <-> BCc
        |]
        |> Array.sum


    // 162 - BCd
    let d162 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[162] // BCd | ligation: B + Cd <-> BCd
            1.0 * x.[4] * x.[34] // B + Cd | ligation: B + Cd <-> BCd
        |]
        |> Array.sum


    // 163 - BDA
    let d163 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[163] // BDA | ligation: B + DA <-> BDA
            1.0 * x.[4] * x.[35] // B + DA | ligation: B + DA <-> BDA
        |]
        |> Array.sum


    // 164 - BDB
    let d164 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[164] // BDB | ligation: B + DB <-> BDB
            1.0 * x.[4] * x.[36] // B + DB | ligation: B + DB <-> BDB
        |]
        |> Array.sum


    // 165 - BDC
    let d165 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[165] // BDC | ligation: B + DC <-> BDC
            1.0 * x.[4] * x.[37] // B + DC | ligation: B + DC <-> BDC
        |]
        |> Array.sum


    // 166 - BDD
    let d166 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[166] // BDD | ligation: B + DD <-> BDD
            1.0 * x.[4] * x.[38] // B + DD | ligation: B + DD <-> BDD
        |]
        |> Array.sum


    // 167 - BDa
    let d167 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[167] // BDa | ligation: B + Da <-> BDa
            1.0 * x.[4] * x.[39] // B + Da | ligation: B + Da <-> BDa
        |]
        |> Array.sum


    // 168 - BDb
    let d168 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[168] // BDb | ligation: B + Db <-> BDb
            1.0 * x.[4] * x.[40] // B + Db | ligation: B + Db <-> BDb
        |]
        |> Array.sum


    // 169 - BDc
    let d169 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[169] // BDc | ligation: B + Dc <-> BDc
            1.0 * x.[4] * x.[41] // B + Dc | ligation: B + Dc <-> BDc
        |]
        |> Array.sum


    // 170 - BDd
    let d170 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[170] // BDd | ligation: B + Dd <-> BDd
            1.0 * x.[4] * x.[42] // B + Dd | ligation: B + Dd <-> BDd
        |]
        |> Array.sum


    // 171 - BaA
    let d171 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[171] // BaA | ligation: B + aA <-> BaA
            1.0 * x.[4] * x.[43] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 172 - BaB
    let d172 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[172] // BaB | ligation: B + aB <-> BaB
            1.0 * x.[4] * x.[44] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 173 - BaC
    let d173 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[173] // BaC | ligation: B + aC <-> BaC
            1.0 * x.[4] * x.[45] // B + aC | ligation: B + aC <-> BaC
        |]
        |> Array.sum


    // 174 - BaD
    let d174 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[174] // BaD | ligation: B + aD <-> BaD
            1.0 * x.[4] * x.[46] // B + aD | ligation: B + aD <-> BaD
        |]
        |> Array.sum


    // 175 - Baa
    let d175 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[175] // Baa | ligation: B + aa <-> Baa
            1.0 * x.[4] * x.[47] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 176 - Bab
    let d176 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[176] // Bab | ligation: B + ab <-> Bab
            1.0 * x.[4] * x.[48] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 177 - Bac
    let d177 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[177] // Bac | ligation: B + ac <-> Bac
            1.0 * x.[4] * x.[49] // B + ac | ligation: B + ac <-> Bac
        |]
        |> Array.sum


    // 178 - Bad
    let d178 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[178] // Bad | ligation: B + ad <-> Bad
            1.0 * x.[4] * x.[50] // B + ad | ligation: B + ad <-> Bad
        |]
        |> Array.sum


    // 179 - BbA
    let d179 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[179] // BbA | ligation: B + bA <-> BbA
            1.0 * x.[4] * x.[51] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 180 - BbB
    let d180 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[180] // BbB | ligation: B + bB <-> BbB
            1.0 * x.[4] * x.[52] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 181 - BbC
    let d181 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[181] // BbC | ligation: B + bC <-> BbC
            1.0 * x.[4] * x.[53] // B + bC | ligation: B + bC <-> BbC
        |]
        |> Array.sum


    // 182 - BbD
    let d182 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[182] // BbD | ligation: B + bD <-> BbD
            1.0 * x.[4] * x.[54] // B + bD | ligation: B + bD <-> BbD
        |]
        |> Array.sum


    // 183 - Bba
    let d183 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[183] // Bba | ligation: B + ba <-> Bba
            1.0 * x.[4] * x.[55] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 184 - Bbb
    let d184 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[184] // Bbb | ligation: B + bb <-> Bbb
            1.0 * x.[4] * x.[56] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 185 - Bbc
    let d185 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[185] // Bbc | ligation: B + bc <-> Bbc
            1.0 * x.[4] * x.[57] // B + bc | ligation: B + bc <-> Bbc
        |]
        |> Array.sum


    // 186 - Bbd
    let d186 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[186] // Bbd | ligation: B + bd <-> Bbd
            1.0 * x.[4] * x.[58] // B + bd | ligation: B + bd <-> Bbd
        |]
        |> Array.sum


    // 187 - BcA
    let d187 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[187] // BcA | ligation: B + cA <-> BcA
            1.0 * x.[4] * x.[59] // B + cA | ligation: B + cA <-> BcA
        |]
        |> Array.sum


    // 188 - BcB
    let d188 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[188] // BcB | ligation: B + cB <-> BcB
            1.0 * x.[4] * x.[60] // B + cB | ligation: B + cB <-> BcB
        |]
        |> Array.sum


    // 189 - BcC
    let d189 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[189] // BcC | ligation: B + cC <-> BcC
            1.0 * x.[4] * x.[61] // B + cC | ligation: B + cC <-> BcC
        |]
        |> Array.sum


    // 190 - BcD
    let d190 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[190] // BcD | ligation: B + cD <-> BcD
            1.0 * x.[4] * x.[62] // B + cD | ligation: B + cD <-> BcD
        |]
        |> Array.sum


    // 191 - Bca
    let d191 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[191] // Bca | ligation: B + ca <-> Bca
            1.0 * x.[4] * x.[63] // B + ca | ligation: B + ca <-> Bca
        |]
        |> Array.sum


    // 192 - Bcb
    let d192 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[192] // Bcb | ligation: B + cb <-> Bcb
            1.0 * x.[4] * x.[64] // B + cb | ligation: B + cb <-> Bcb
        |]
        |> Array.sum


    // 193 - Bcc
    let d193 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[193] // Bcc | ligation: B + cc <-> Bcc
            1.0 * x.[4] * x.[65] // B + cc | ligation: B + cc <-> Bcc
        |]
        |> Array.sum


    // 194 - Bcd
    let d194 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[194] // Bcd | ligation: B + cd <-> Bcd
            1.0 * x.[4] * x.[66] // B + cd | ligation: B + cd <-> Bcd
        |]
        |> Array.sum


    // 195 - BdA
    let d195 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[195] // BdA | ligation: B + dA <-> BdA
            1.0 * x.[4] * x.[67] // B + dA | ligation: B + dA <-> BdA
        |]
        |> Array.sum


    // 196 - BdB
    let d196 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[196] // BdB | ligation: B + dB <-> BdB
            1.0 * x.[4] * x.[68] // B + dB | ligation: B + dB <-> BdB
        |]
        |> Array.sum


    // 197 - BdC
    let d197 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[197] // BdC | ligation: B + dC <-> BdC
            1.0 * x.[4] * x.[69] // B + dC | ligation: B + dC <-> BdC
        |]
        |> Array.sum


    // 198 - BdD
    let d198 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[198] // BdD | ligation: B + dD <-> BdD
            1.0 * x.[4] * x.[70] // B + dD | ligation: B + dD <-> BdD
        |]
        |> Array.sum


    // 199 - Bda
    let d199 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[199] // Bda | ligation: B + da <-> Bda
            1.0 * x.[4] * x.[71] // B + da | ligation: B + da <-> Bda
        |]
        |> Array.sum


    // 200 - Bdb
    let d200 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[200] // Bdb | ligation: B + db <-> Bdb
            1.0 * x.[4] * x.[72] // B + db | ligation: B + db <-> Bdb
        |]
        |> Array.sum


    // 201 - Bdc
    let d201 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[201] // Bdc | ligation: B + dc <-> Bdc
            1.0 * x.[4] * x.[73] // B + dc | ligation: B + dc <-> Bdc
        |]
        |> Array.sum


    // 202 - Bdd
    let d202 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[202] // Bdd | ligation: B + dd <-> Bdd
            1.0 * x.[4] * x.[74] // B + dd | ligation: B + dd <-> Bdd
        |]
        |> Array.sum


    // 203 - CAA
    let d203 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[203] // CAA | ligation: C + AA <-> CAA
            1.0 * x.[5] * x.[11] // C + AA | ligation: C + AA <-> CAA
        |]
        |> Array.sum


    // 204 - CAB
    let d204 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[204] // CAB | ligation: C + AB <-> CAB
            1.0 * x.[5] * x.[12] // C + AB | ligation: C + AB <-> CAB
        |]
        |> Array.sum


    // 205 - CAC
    let d205 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[205] // CAC | ligation: C + AC <-> CAC
            1.0 * x.[5] * x.[13] // C + AC | ligation: C + AC <-> CAC
        |]
        |> Array.sum


    // 206 - CAD
    let d206 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[206] // CAD | ligation: C + AD <-> CAD
            1.0 * x.[5] * x.[14] // C + AD | ligation: C + AD <-> CAD
        |]
        |> Array.sum


    // 207 - CAa
    let d207 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[207] // CAa | ligation: C + Aa <-> CAa
            1.0 * x.[5] * x.[15] // C + Aa | ligation: C + Aa <-> CAa
        |]
        |> Array.sum


    // 208 - CAb
    let d208 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[208] // CAb | ligation: C + Ab <-> CAb
            1.0 * x.[5] * x.[16] // C + Ab | ligation: C + Ab <-> CAb
        |]
        |> Array.sum


    // 209 - CAc
    let d209 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[209] // CAc | ligation: C + Ac <-> CAc
            1.0 * x.[5] * x.[17] // C + Ac | ligation: C + Ac <-> CAc
        |]
        |> Array.sum


    // 210 - CAd
    let d210 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[210] // CAd | ligation: C + Ad <-> CAd
            1.0 * x.[5] * x.[18] // C + Ad | ligation: C + Ad <-> CAd
        |]
        |> Array.sum


    // 211 - CBA
    let d211 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[211] // CBA | ligation: C + BA <-> CBA
            1.0 * x.[5] * x.[19] // C + BA | ligation: C + BA <-> CBA
        |]
        |> Array.sum


    // 212 - CBB
    let d212 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[212] // CBB | ligation: C + BB <-> CBB
            1.0 * x.[5] * x.[20] // C + BB | ligation: C + BB <-> CBB
        |]
        |> Array.sum


    // 213 - CBC
    let d213 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[213] // CBC | ligation: C + BC <-> CBC
            1.0 * x.[5] * x.[21] // C + BC | ligation: C + BC <-> CBC
        |]
        |> Array.sum


    // 214 - CBD
    let d214 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[214] // CBD | ligation: C + BD <-> CBD
            1.0 * x.[5] * x.[22] // C + BD | ligation: C + BD <-> CBD
        |]
        |> Array.sum


    // 215 - CBa
    let d215 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[215] // CBa | ligation: C + Ba <-> CBa
            1.0 * x.[5] * x.[23] // C + Ba | ligation: C + Ba <-> CBa
        |]
        |> Array.sum


    // 216 - CBb
    let d216 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[216] // CBb | ligation: C + Bb <-> CBb
            1.0 * x.[5] * x.[24] // C + Bb | ligation: C + Bb <-> CBb
        |]
        |> Array.sum


    // 217 - CBc
    let d217 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[217] // CBc | ligation: C + Bc <-> CBc
            1.0 * x.[5] * x.[25] // C + Bc | ligation: C + Bc <-> CBc
        |]
        |> Array.sum


    // 218 - CBd
    let d218 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[218] // CBd | ligation: C + Bd <-> CBd
            1.0 * x.[5] * x.[26] // C + Bd | ligation: C + Bd <-> CBd
        |]
        |> Array.sum


    // 219 - CCA
    let d219 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[219] // CCA | ligation: C + CA <-> CCA
            1.0 * x.[5] * x.[27] // C + CA | ligation: C + CA <-> CCA
        |]
        |> Array.sum


    // 220 - CCB
    let d220 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[220] // CCB | ligation: C + CB <-> CCB
            1.0 * x.[5] * x.[28] // C + CB | ligation: C + CB <-> CCB
        |]
        |> Array.sum


    // 221 - CCC
    let d221 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[221] // CCC | ligation: C + CC <-> CCC
            1.0 * x.[5] * x.[29] // C + CC | ligation: C + CC <-> CCC
        |]
        |> Array.sum


    // 222 - CCD
    let d222 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[222] // CCD | ligation: C + CD <-> CCD
            1.0 * x.[5] * x.[30] // C + CD | ligation: C + CD <-> CCD
        |]
        |> Array.sum


    // 223 - CCa
    let d223 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[223] // CCa | ligation: C + Ca <-> CCa
            1.0 * x.[5] * x.[31] // C + Ca | ligation: C + Ca <-> CCa
        |]
        |> Array.sum


    // 224 - CCb
    let d224 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[224] // CCb | ligation: C + Cb <-> CCb
            1.0 * x.[5] * x.[32] // C + Cb | ligation: C + Cb <-> CCb
        |]
        |> Array.sum


    // 225 - CCc
    let d225 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[225] // CCc | ligation: C + Cc <-> CCc
            1.0 * x.[5] * x.[33] // C + Cc | ligation: C + Cc <-> CCc
        |]
        |> Array.sum


    // 226 - CCd
    let d226 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[226] // CCd | ligation: C + Cd <-> CCd
            1.0 * x.[5] * x.[34] // C + Cd | ligation: C + Cd <-> CCd
        |]
        |> Array.sum


    // 227 - CDA
    let d227 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[227] // CDA | ligation: C + DA <-> CDA
            1.0 * x.[5] * x.[35] // C + DA | ligation: C + DA <-> CDA
        |]
        |> Array.sum


    // 228 - CDB
    let d228 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[228] // CDB | ligation: C + DB <-> CDB
            1.0 * x.[5] * x.[36] // C + DB | ligation: C + DB <-> CDB
        |]
        |> Array.sum


    // 229 - CDC
    let d229 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[229] // CDC | ligation: C + DC <-> CDC
            1.0 * x.[5] * x.[37] // C + DC | ligation: C + DC <-> CDC
        |]
        |> Array.sum


    // 230 - CDD
    let d230 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[230] // CDD | ligation: C + DD <-> CDD
            1.0 * x.[5] * x.[38] // C + DD | ligation: C + DD <-> CDD
        |]
        |> Array.sum


    // 231 - CDa
    let d231 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[231] // CDa | ligation: C + Da <-> CDa
            1.0 * x.[5] * x.[39] // C + Da | ligation: C + Da <-> CDa
        |]
        |> Array.sum


    // 232 - CDb
    let d232 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[232] // CDb | ligation: C + Db <-> CDb
            1.0 * x.[5] * x.[40] // C + Db | ligation: C + Db <-> CDb
        |]
        |> Array.sum


    // 233 - CDc
    let d233 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[233] // CDc | ligation: C + Dc <-> CDc
            1.0 * x.[5] * x.[41] // C + Dc | ligation: C + Dc <-> CDc
        |]
        |> Array.sum


    // 234 - CDd
    let d234 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[234] // CDd | ligation: C + Dd <-> CDd
            1.0 * x.[5] * x.[42] // C + Dd | ligation: C + Dd <-> CDd
        |]
        |> Array.sum


    // 235 - CaA
    let d235 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[235] // CaA | ligation: C + aA <-> CaA
            1.0 * x.[5] * x.[43] // C + aA | ligation: C + aA <-> CaA
        |]
        |> Array.sum


    // 236 - CaB
    let d236 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[236] // CaB | ligation: C + aB <-> CaB
            1.0 * x.[5] * x.[44] // C + aB | ligation: C + aB <-> CaB
        |]
        |> Array.sum


    // 237 - CaC
    let d237 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[237] // CaC | ligation: C + aC <-> CaC
            1.0 * x.[5] * x.[45] // C + aC | ligation: C + aC <-> CaC
        |]
        |> Array.sum


    // 238 - CaD
    let d238 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[238] // CaD | ligation: C + aD <-> CaD
            1.0 * x.[5] * x.[46] // C + aD | ligation: C + aD <-> CaD
        |]
        |> Array.sum


    // 239 - Caa
    let d239 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[239] // Caa | ligation: C + aa <-> Caa
            1.0 * x.[5] * x.[47] // C + aa | ligation: C + aa <-> Caa
        |]
        |> Array.sum


    // 240 - Cab
    let d240 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[240] // Cab | ligation: C + ab <-> Cab
            1.0 * x.[5] * x.[48] // C + ab | ligation: C + ab <-> Cab
        |]
        |> Array.sum


    // 241 - Cac
    let d241 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[241] // Cac | ligation: C + ac <-> Cac
            1.0 * x.[5] * x.[49] // C + ac | ligation: C + ac <-> Cac
        |]
        |> Array.sum


    // 242 - Cad
    let d242 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[242] // Cad | ligation: C + ad <-> Cad
            1.0 * x.[5] * x.[50] // C + ad | ligation: C + ad <-> Cad
        |]
        |> Array.sum


    // 243 - CbA
    let d243 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[243] // CbA | ligation: C + bA <-> CbA
            1.0 * x.[5] * x.[51] // C + bA | ligation: C + bA <-> CbA
        |]
        |> Array.sum


    // 244 - CbB
    let d244 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[244] // CbB | ligation: C + bB <-> CbB
            1.0 * x.[5] * x.[52] // C + bB | ligation: C + bB <-> CbB
        |]
        |> Array.sum


    // 245 - CbC
    let d245 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[245] // CbC | ligation: C + bC <-> CbC
            1.0 * x.[5] * x.[53] // C + bC | ligation: C + bC <-> CbC
        |]
        |> Array.sum


    // 246 - CbD
    let d246 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[246] // CbD | ligation: C + bD <-> CbD
            1.0 * x.[5] * x.[54] // C + bD | ligation: C + bD <-> CbD
        |]
        |> Array.sum


    // 247 - Cba
    let d247 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[247] // Cba | ligation: C + ba <-> Cba
            1.0 * x.[5] * x.[55] // C + ba | ligation: C + ba <-> Cba
        |]
        |> Array.sum


    // 248 - Cbb
    let d248 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[248] // Cbb | ligation: C + bb <-> Cbb
            1.0 * x.[5] * x.[56] // C + bb | ligation: C + bb <-> Cbb
        |]
        |> Array.sum


    // 249 - Cbc
    let d249 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[249] // Cbc | ligation: C + bc <-> Cbc
            1.0 * x.[5] * x.[57] // C + bc | ligation: C + bc <-> Cbc
        |]
        |> Array.sum


    // 250 - Cbd
    let d250 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[250] // Cbd | ligation: C + bd <-> Cbd
            1.0 * x.[5] * x.[58] // C + bd | ligation: C + bd <-> Cbd
        |]
        |> Array.sum


    // 251 - CcA
    let d251 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[251] // CcA | ligation: C + cA <-> CcA
            1.0 * x.[5] * x.[59] // C + cA | ligation: C + cA <-> CcA
        |]
        |> Array.sum


    // 252 - CcB
    let d252 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[252] // CcB | ligation: C + cB <-> CcB
            1.0 * x.[5] * x.[60] // C + cB | ligation: C + cB <-> CcB
        |]
        |> Array.sum


    // 253 - CcC
    let d253 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[253] // CcC | ligation: C + cC <-> CcC
            1.0 * x.[5] * x.[61] // C + cC | ligation: C + cC <-> CcC
        |]
        |> Array.sum


    // 254 - CcD
    let d254 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[254] // CcD | ligation: C + cD <-> CcD
            1.0 * x.[5] * x.[62] // C + cD | ligation: C + cD <-> CcD
        |]
        |> Array.sum


    // 255 - Cca
    let d255 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[255] // Cca | ligation: C + ca <-> Cca
            1.0 * x.[5] * x.[63] // C + ca | ligation: C + ca <-> Cca
        |]
        |> Array.sum


    // 256 - Ccb
    let d256 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[256] // Ccb | ligation: C + cb <-> Ccb
            1.0 * x.[5] * x.[64] // C + cb | ligation: C + cb <-> Ccb
        |]
        |> Array.sum


    // 257 - Ccc
    let d257 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[257] // Ccc | ligation: C + cc <-> Ccc
            1.0 * x.[5] * x.[65] // C + cc | ligation: C + cc <-> Ccc
        |]
        |> Array.sum


    // 258 - Ccd
    let d258 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[258] // Ccd | ligation: C + cd <-> Ccd
            1.0 * x.[5] * x.[66] // C + cd | ligation: C + cd <-> Ccd
        |]
        |> Array.sum


    // 259 - CdA
    let d259 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[259] // CdA | ligation: C + dA <-> CdA
            1.0 * x.[5] * x.[67] // C + dA | ligation: C + dA <-> CdA
        |]
        |> Array.sum


    // 260 - CdB
    let d260 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[260] // CdB | ligation: C + dB <-> CdB
            1.0 * x.[5] * x.[68] // C + dB | ligation: C + dB <-> CdB
        |]
        |> Array.sum


    // 261 - CdC
    let d261 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[261] // CdC | ligation: C + dC <-> CdC
            1.0 * x.[5] * x.[69] // C + dC | ligation: C + dC <-> CdC
        |]
        |> Array.sum


    // 262 - CdD
    let d262 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[262] // CdD | ligation: C + dD <-> CdD
            1.0 * x.[5] * x.[70] // C + dD | ligation: C + dD <-> CdD
        |]
        |> Array.sum


    // 263 - Cda
    let d263 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[263] // Cda | ligation: C + da <-> Cda
            1.0 * x.[5] * x.[71] // C + da | ligation: C + da <-> Cda
        |]
        |> Array.sum


    // 264 - Cdb
    let d264 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[264] // Cdb | ligation: C + db <-> Cdb
            1.0 * x.[5] * x.[72] // C + db | ligation: C + db <-> Cdb
        |]
        |> Array.sum


    // 265 - Cdc
    let d265 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[265] // Cdc | ligation: C + dc <-> Cdc
            1.0 * x.[5] * x.[73] // C + dc | ligation: C + dc <-> Cdc
        |]
        |> Array.sum


    // 266 - Cdd
    let d266 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[266] // Cdd | ligation: C + dd <-> Cdd
            1.0 * x.[5] * x.[74] // C + dd | ligation: C + dd <-> Cdd
        |]
        |> Array.sum


    // 267 - DAA
    let d267 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[267] // DAA | ligation: D + AA <-> DAA
            1.0 * x.[6] * x.[11] // D + AA | ligation: D + AA <-> DAA
        |]
        |> Array.sum


    // 268 - DAB
    let d268 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[268] // DAB | ligation: D + AB <-> DAB
            1.0 * x.[6] * x.[12] // D + AB | ligation: D + AB <-> DAB
        |]
        |> Array.sum


    // 269 - DAC
    let d269 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[269] // DAC | ligation: D + AC <-> DAC
            1.0 * x.[6] * x.[13] // D + AC | ligation: D + AC <-> DAC
        |]
        |> Array.sum


    // 270 - DAD
    let d270 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[270] // DAD | ligation: D + AD <-> DAD
            1.0 * x.[6] * x.[14] // D + AD | ligation: D + AD <-> DAD
        |]
        |> Array.sum


    // 271 - DAa
    let d271 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[271] // DAa | ligation: D + Aa <-> DAa
            1.0 * x.[6] * x.[15] // D + Aa | ligation: D + Aa <-> DAa
        |]
        |> Array.sum


    // 272 - DAb
    let d272 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[272] // DAb | ligation: D + Ab <-> DAb
            1.0 * x.[6] * x.[16] // D + Ab | ligation: D + Ab <-> DAb
        |]
        |> Array.sum


    // 273 - DAc
    let d273 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[273] // DAc | ligation: D + Ac <-> DAc
            1.0 * x.[6] * x.[17] // D + Ac | ligation: D + Ac <-> DAc
        |]
        |> Array.sum


    // 274 - DAd
    let d274 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[274] // DAd | ligation: D + Ad <-> DAd
            1.0 * x.[6] * x.[18] // D + Ad | ligation: D + Ad <-> DAd
        |]
        |> Array.sum


    // 275 - DBA
    let d275 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[275] // DBA | ligation: D + BA <-> DBA
            1.0 * x.[6] * x.[19] // D + BA | ligation: D + BA <-> DBA
        |]
        |> Array.sum


    // 276 - DBB
    let d276 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[276] // DBB | ligation: D + BB <-> DBB
            1.0 * x.[6] * x.[20] // D + BB | ligation: D + BB <-> DBB
        |]
        |> Array.sum


    // 277 - DBC
    let d277 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[277] // DBC | ligation: D + BC <-> DBC
            1.0 * x.[6] * x.[21] // D + BC | ligation: D + BC <-> DBC
        |]
        |> Array.sum


    // 278 - DBD
    let d278 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[278] // DBD | ligation: D + BD <-> DBD
            1.0 * x.[6] * x.[22] // D + BD | ligation: D + BD <-> DBD
        |]
        |> Array.sum


    // 279 - DBa
    let d279 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[279] // DBa | ligation: D + Ba <-> DBa
            1.0 * x.[6] * x.[23] // D + Ba | ligation: D + Ba <-> DBa
        |]
        |> Array.sum


    // 280 - DBb
    let d280 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[280] // DBb | ligation: D + Bb <-> DBb
            1.0 * x.[6] * x.[24] // D + Bb | ligation: D + Bb <-> DBb
        |]
        |> Array.sum


    // 281 - DBc
    let d281 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[281] // DBc | ligation: D + Bc <-> DBc
            1.0 * x.[6] * x.[25] // D + Bc | ligation: D + Bc <-> DBc
        |]
        |> Array.sum


    // 282 - DBd
    let d282 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[282] // DBd | ligation: D + Bd <-> DBd
            1.0 * x.[6] * x.[26] // D + Bd | ligation: D + Bd <-> DBd
        |]
        |> Array.sum


    // 283 - DCA
    let d283 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[283] // DCA | ligation: D + CA <-> DCA
            1.0 * x.[6] * x.[27] // D + CA | ligation: D + CA <-> DCA
        |]
        |> Array.sum


    // 284 - DCB
    let d284 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[284] // DCB | ligation: D + CB <-> DCB
            1.0 * x.[6] * x.[28] // D + CB | ligation: D + CB <-> DCB
        |]
        |> Array.sum


    // 285 - DCC
    let d285 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[285] // DCC | ligation: D + CC <-> DCC
            1.0 * x.[6] * x.[29] // D + CC | ligation: D + CC <-> DCC
        |]
        |> Array.sum


    // 286 - DCD
    let d286 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[286] // DCD | ligation: D + CD <-> DCD
            1.0 * x.[6] * x.[30] // D + CD | ligation: D + CD <-> DCD
        |]
        |> Array.sum


    // 287 - DCa
    let d287 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[287] // DCa | ligation: D + Ca <-> DCa
            1.0 * x.[6] * x.[31] // D + Ca | ligation: D + Ca <-> DCa
        |]
        |> Array.sum


    // 288 - DCb
    let d288 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[288] // DCb | ligation: D + Cb <-> DCb
            1.0 * x.[6] * x.[32] // D + Cb | ligation: D + Cb <-> DCb
        |]
        |> Array.sum


    // 289 - DCc
    let d289 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[289] // DCc | ligation: D + Cc <-> DCc
            1.0 * x.[6] * x.[33] // D + Cc | ligation: D + Cc <-> DCc
        |]
        |> Array.sum


    // 290 - DCd
    let d290 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[290] // DCd | ligation: D + Cd <-> DCd
            1.0 * x.[6] * x.[34] // D + Cd | ligation: D + Cd <-> DCd
        |]
        |> Array.sum


    // 291 - DDA
    let d291 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[291] // DDA | ligation: D + DA <-> DDA
            1.0 * x.[6] * x.[35] // D + DA | ligation: D + DA <-> DDA
        |]
        |> Array.sum


    // 292 - DDB
    let d292 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[292] // DDB | ligation: D + DB <-> DDB
            1.0 * x.[6] * x.[36] // D + DB | ligation: D + DB <-> DDB
        |]
        |> Array.sum


    // 293 - DDC
    let d293 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[293] // DDC | ligation: D + DC <-> DDC
            1.0 * x.[6] * x.[37] // D + DC | ligation: D + DC <-> DDC
        |]
        |> Array.sum


    // 294 - DDD
    let d294 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[294] // DDD | ligation: D + DD <-> DDD
            1.0 * x.[6] * x.[38] // D + DD | ligation: D + DD <-> DDD
        |]
        |> Array.sum


    // 295 - DDa
    let d295 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[295] // DDa | ligation: D + Da <-> DDa
            1.0 * x.[6] * x.[39] // D + Da | ligation: D + Da <-> DDa
        |]
        |> Array.sum


    // 296 - DDb
    let d296 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[296] // DDb | ligation: D + Db <-> DDb
            1.0 * x.[6] * x.[40] // D + Db | ligation: D + Db <-> DDb
        |]
        |> Array.sum


    // 297 - DDc
    let d297 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[297] // DDc | ligation: D + Dc <-> DDc
            1.0 * x.[6] * x.[41] // D + Dc | ligation: D + Dc <-> DDc
        |]
        |> Array.sum


    // 298 - DDd
    let d298 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[298] // DDd | ligation: D + Dd <-> DDd
            1.0 * x.[6] * x.[42] // D + Dd | ligation: D + Dd <-> DDd
        |]
        |> Array.sum


    // 299 - DaA
    let d299 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[299] // DaA | ligation: D + aA <-> DaA
            1.0 * x.[6] * x.[43] // D + aA | ligation: D + aA <-> DaA
        |]
        |> Array.sum


    // 300 - DaB
    let d300 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[300] // DaB | ligation: D + aB <-> DaB
            1.0 * x.[6] * x.[44] // D + aB | ligation: D + aB <-> DaB
        |]
        |> Array.sum


    // 301 - DaC
    let d301 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[301] // DaC | ligation: D + aC <-> DaC
            1.0 * x.[6] * x.[45] // D + aC | ligation: D + aC <-> DaC
        |]
        |> Array.sum


    // 302 - DaD
    let d302 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[302] // DaD | ligation: D + aD <-> DaD
            1.0 * x.[6] * x.[46] // D + aD | ligation: D + aD <-> DaD
        |]
        |> Array.sum


    // 303 - Daa
    let d303 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[303] // Daa | ligation: D + aa <-> Daa
            1.0 * x.[6] * x.[47] // D + aa | ligation: D + aa <-> Daa
        |]
        |> Array.sum


    // 304 - Dab
    let d304 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[304] // Dab | ligation: D + ab <-> Dab
            1.0 * x.[6] * x.[48] // D + ab | ligation: D + ab <-> Dab
        |]
        |> Array.sum


    // 305 - Dac
    let d305 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[305] // Dac | ligation: D + ac <-> Dac
            1.0 * x.[6] * x.[49] // D + ac | ligation: D + ac <-> Dac
        |]
        |> Array.sum


    // 306 - Dad
    let d306 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[306] // Dad | ligation: D + ad <-> Dad
            1.0 * x.[6] * x.[50] // D + ad | ligation: D + ad <-> Dad
        |]
        |> Array.sum


    // 307 - DbA
    let d307 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[307] // DbA | ligation: D + bA <-> DbA
            1.0 * x.[6] * x.[51] // D + bA | ligation: D + bA <-> DbA
        |]
        |> Array.sum


    // 308 - DbB
    let d308 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[308] // DbB | ligation: D + bB <-> DbB
            1.0 * x.[6] * x.[52] // D + bB | ligation: D + bB <-> DbB
        |]
        |> Array.sum


    // 309 - DbC
    let d309 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[309] // DbC | ligation: D + bC <-> DbC
            1.0 * x.[6] * x.[53] // D + bC | ligation: D + bC <-> DbC
        |]
        |> Array.sum


    // 310 - DbD
    let d310 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[310] // DbD | ligation: D + bD <-> DbD
            1.0 * x.[6] * x.[54] // D + bD | ligation: D + bD <-> DbD
        |]
        |> Array.sum


    // 311 - Dba
    let d311 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[311] // Dba | ligation: D + ba <-> Dba
            1.0 * x.[6] * x.[55] // D + ba | ligation: D + ba <-> Dba
        |]
        |> Array.sum


    // 312 - Dbb
    let d312 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[312] // Dbb | ligation: D + bb <-> Dbb
            1.0 * x.[6] * x.[56] // D + bb | ligation: D + bb <-> Dbb
        |]
        |> Array.sum


    // 313 - Dbc
    let d313 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[313] // Dbc | ligation: D + bc <-> Dbc
            1.0 * x.[6] * x.[57] // D + bc | ligation: D + bc <-> Dbc
        |]
        |> Array.sum


    // 314 - Dbd
    let d314 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[314] // Dbd | ligation: D + bd <-> Dbd
            1.0 * x.[6] * x.[58] // D + bd | ligation: D + bd <-> Dbd
        |]
        |> Array.sum


    // 315 - DcA
    let d315 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[315] // DcA | ligation: D + cA <-> DcA
            1.0 * x.[6] * x.[59] // D + cA | ligation: D + cA <-> DcA
        |]
        |> Array.sum


    // 316 - DcB
    let d316 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[316] // DcB | ligation: D + cB <-> DcB
            1.0 * x.[6] * x.[60] // D + cB | ligation: D + cB <-> DcB
        |]
        |> Array.sum


    // 317 - DcC
    let d317 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[317] // DcC | ligation: D + cC <-> DcC
            1.0 * x.[6] * x.[61] // D + cC | ligation: D + cC <-> DcC
        |]
        |> Array.sum


    // 318 - DcD
    let d318 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[318] // DcD | ligation: D + cD <-> DcD
            1.0 * x.[6] * x.[62] // D + cD | ligation: D + cD <-> DcD
        |]
        |> Array.sum


    // 319 - Dca
    let d319 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[319] // Dca | ligation: D + ca <-> Dca
            1.0 * x.[6] * x.[63] // D + ca | ligation: D + ca <-> Dca
        |]
        |> Array.sum


    // 320 - Dcb
    let d320 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[320] // Dcb | ligation: D + cb <-> Dcb
            1.0 * x.[6] * x.[64] // D + cb | ligation: D + cb <-> Dcb
        |]
        |> Array.sum


    // 321 - Dcc
    let d321 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[321] // Dcc | ligation: D + cc <-> Dcc
            1.0 * x.[6] * x.[65] // D + cc | ligation: D + cc <-> Dcc
        |]
        |> Array.sum


    // 322 - Dcd
    let d322 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[322] // Dcd | ligation: D + cd <-> Dcd
            1.0 * x.[6] * x.[66] // D + cd | ligation: D + cd <-> Dcd
        |]
        |> Array.sum


    // 323 - DdA
    let d323 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[323] // DdA | ligation: D + dA <-> DdA
            1.0 * x.[6] * x.[67] // D + dA | ligation: D + dA <-> DdA
        |]
        |> Array.sum


    // 324 - DdB
    let d324 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[324] // DdB | ligation: D + dB <-> DdB
            1.0 * x.[6] * x.[68] // D + dB | ligation: D + dB <-> DdB
        |]
        |> Array.sum


    // 325 - DdC
    let d325 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[325] // DdC | ligation: D + dC <-> DdC
            1.0 * x.[6] * x.[69] // D + dC | ligation: D + dC <-> DdC
        |]
        |> Array.sum


    // 326 - DdD
    let d326 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[326] // DdD | ligation: D + dD <-> DdD
            1.0 * x.[6] * x.[70] // D + dD | ligation: D + dD <-> DdD
        |]
        |> Array.sum


    // 327 - Dda
    let d327 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[327] // Dda | ligation: D + da <-> Dda
            1.0 * x.[6] * x.[71] // D + da | ligation: D + da <-> Dda
        |]
        |> Array.sum


    // 328 - Ddb
    let d328 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[328] // Ddb | ligation: D + db <-> Ddb
            1.0 * x.[6] * x.[72] // D + db | ligation: D + db <-> Ddb
        |]
        |> Array.sum


    // 329 - Ddc
    let d329 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[329] // Ddc | ligation: D + dc <-> Ddc
            1.0 * x.[6] * x.[73] // D + dc | ligation: D + dc <-> Ddc
        |]
        |> Array.sum


    // 330 - Ddd
    let d330 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[330] // Ddd | ligation: D + dd <-> Ddd
            1.0 * x.[6] * x.[74] // D + dd | ligation: D + dd <-> Ddd
        |]
        |> Array.sum


    // 331 - aAA
    let d331 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[331] // aAA | ligation: a + AA <-> aAA
            1.0 * x.[7] * x.[11] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 332 - aAB
    let d332 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[332] // aAB | ligation: a + AB <-> aAB
            1.0 * x.[7] * x.[12] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 333 - aAC
    let d333 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[333] // aAC | ligation: a + AC <-> aAC
            1.0 * x.[7] * x.[13] // a + AC | ligation: a + AC <-> aAC
        |]
        |> Array.sum


    // 334 - aAD
    let d334 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[334] // aAD | ligation: a + AD <-> aAD
            1.0 * x.[7] * x.[14] // a + AD | ligation: a + AD <-> aAD
        |]
        |> Array.sum


    // 335 - aAa
    let d335 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[335] // aAa | ligation: a + Aa <-> aAa
            1.0 * x.[7] * x.[15] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 336 - aAb
    let d336 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[336] // aAb | ligation: a + Ab <-> aAb
            1.0 * x.[7] * x.[16] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 337 - aAc
    let d337 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[337] // aAc | ligation: a + Ac <-> aAc
            1.0 * x.[7] * x.[17] // a + Ac | ligation: a + Ac <-> aAc
        |]
        |> Array.sum


    // 338 - aAd
    let d338 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[338] // aAd | ligation: a + Ad <-> aAd
            1.0 * x.[7] * x.[18] // a + Ad | ligation: a + Ad <-> aAd
        |]
        |> Array.sum


    // 339 - aBA
    let d339 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[339] // aBA | ligation: a + BA <-> aBA
            1.0 * x.[7] * x.[19] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 340 - aBB
    let d340 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[340] // aBB | ligation: a + BB <-> aBB
            1.0 * x.[7] * x.[20] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 341 - aBC
    let d341 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[341] // aBC | ligation: a + BC <-> aBC
            1.0 * x.[7] * x.[21] // a + BC | ligation: a + BC <-> aBC
        |]
        |> Array.sum


    // 342 - aBD
    let d342 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[342] // aBD | ligation: a + BD <-> aBD
            1.0 * x.[7] * x.[22] // a + BD | ligation: a + BD <-> aBD
        |]
        |> Array.sum


    // 343 - aBa
    let d343 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[343] // aBa | ligation: a + Ba <-> aBa
            1.0 * x.[7] * x.[23] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 344 - aBb
    let d344 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[344] // aBb | ligation: a + Bb <-> aBb
            1.0 * x.[7] * x.[24] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 345 - aBc
    let d345 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[345] // aBc | ligation: a + Bc <-> aBc
            1.0 * x.[7] * x.[25] // a + Bc | ligation: a + Bc <-> aBc
        |]
        |> Array.sum


    // 346 - aBd
    let d346 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[346] // aBd | ligation: a + Bd <-> aBd
            1.0 * x.[7] * x.[26] // a + Bd | ligation: a + Bd <-> aBd
        |]
        |> Array.sum


    // 347 - aCA
    let d347 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[347] // aCA | ligation: a + CA <-> aCA
            1.0 * x.[7] * x.[27] // a + CA | ligation: a + CA <-> aCA
        |]
        |> Array.sum


    // 348 - aCB
    let d348 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[348] // aCB | ligation: a + CB <-> aCB
            1.0 * x.[7] * x.[28] // a + CB | ligation: a + CB <-> aCB
        |]
        |> Array.sum


    // 349 - aCC
    let d349 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[349] // aCC | ligation: a + CC <-> aCC
            1.0 * x.[7] * x.[29] // a + CC | ligation: a + CC <-> aCC
        |]
        |> Array.sum


    // 350 - aCD
    let d350 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[350] // aCD | ligation: a + CD <-> aCD
            1.0 * x.[7] * x.[30] // a + CD | ligation: a + CD <-> aCD
        |]
        |> Array.sum


    // 351 - aCa
    let d351 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[351] // aCa | ligation: a + Ca <-> aCa
            1.0 * x.[7] * x.[31] // a + Ca | ligation: a + Ca <-> aCa
        |]
        |> Array.sum


    // 352 - aCb
    let d352 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[352] // aCb | ligation: a + Cb <-> aCb
            1.0 * x.[7] * x.[32] // a + Cb | ligation: a + Cb <-> aCb
        |]
        |> Array.sum


    // 353 - aCc
    let d353 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[353] // aCc | ligation: a + Cc <-> aCc
            1.0 * x.[7] * x.[33] // a + Cc | ligation: a + Cc <-> aCc
        |]
        |> Array.sum


    // 354 - aCd
    let d354 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[354] // aCd | ligation: a + Cd <-> aCd
            1.0 * x.[7] * x.[34] // a + Cd | ligation: a + Cd <-> aCd
        |]
        |> Array.sum


    // 355 - aDA
    let d355 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[355] // aDA | ligation: a + DA <-> aDA
            1.0 * x.[7] * x.[35] // a + DA | ligation: a + DA <-> aDA
        |]
        |> Array.sum


    // 356 - aDB
    let d356 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[356] // aDB | ligation: a + DB <-> aDB
            1.0 * x.[7] * x.[36] // a + DB | ligation: a + DB <-> aDB
        |]
        |> Array.sum


    // 357 - aDC
    let d357 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[357] // aDC | ligation: a + DC <-> aDC
            1.0 * x.[7] * x.[37] // a + DC | ligation: a + DC <-> aDC
        |]
        |> Array.sum


    // 358 - aDD
    let d358 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[358] // aDD | ligation: a + DD <-> aDD
            1.0 * x.[7] * x.[38] // a + DD | ligation: a + DD <-> aDD
        |]
        |> Array.sum


    // 359 - aDa
    let d359 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[359] // aDa | ligation: a + Da <-> aDa
            1.0 * x.[7] * x.[39] // a + Da | ligation: a + Da <-> aDa
        |]
        |> Array.sum


    // 360 - aDb
    let d360 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[360] // aDb | ligation: a + Db <-> aDb
            1.0 * x.[7] * x.[40] // a + Db | ligation: a + Db <-> aDb
        |]
        |> Array.sum


    // 361 - aDc
    let d361 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[361] // aDc | ligation: a + Dc <-> aDc
            1.0 * x.[7] * x.[41] // a + Dc | ligation: a + Dc <-> aDc
        |]
        |> Array.sum


    // 362 - aDd
    let d362 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[362] // aDd | ligation: a + Dd <-> aDd
            1.0 * x.[7] * x.[42] // a + Dd | ligation: a + Dd <-> aDd
        |]
        |> Array.sum


    // 363 - aaA
    let d363 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[363] // aaA | ligation: a + aA <-> aaA
            1.0 * x.[7] * x.[43] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 364 - aaB
    let d364 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[364] // aaB | ligation: a + aB <-> aaB
            1.0 * x.[7] * x.[44] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 365 - aaC
    let d365 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[365] // aaC | ligation: a + aC <-> aaC
            1.0 * x.[7] * x.[45] // a + aC | ligation: a + aC <-> aaC
        |]
        |> Array.sum


    // 366 - aaD
    let d366 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[366] // aaD | ligation: a + aD <-> aaD
            1.0 * x.[7] * x.[46] // a + aD | ligation: a + aD <-> aaD
        |]
        |> Array.sum


    // 367 - aaa
    let d367 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[367] // aaa | ligation: a + aa <-> aaa
            1.0 * x.[7] * x.[47] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 368 - aab
    let d368 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[368] // aab | ligation: a + ab <-> aab
            1.0 * x.[7] * x.[48] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 369 - aac
    let d369 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[369] // aac | ligation: a + ac <-> aac
            1.0 * x.[7] * x.[49] // a + ac | ligation: a + ac <-> aac
        |]
        |> Array.sum


    // 370 - aad
    let d370 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[370] // aad | ligation: a + ad <-> aad
            1.0 * x.[7] * x.[50] // a + ad | ligation: a + ad <-> aad
        |]
        |> Array.sum


    // 371 - abA
    let d371 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[371] // abA | ligation: a + bA <-> abA
            1.0 * x.[7] * x.[51] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 372 - abB
    let d372 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[372] // abB | ligation: a + bB <-> abB
            1.0 * x.[7] * x.[52] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 373 - abC
    let d373 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[373] // abC | ligation: a + bC <-> abC
            1.0 * x.[7] * x.[53] // a + bC | ligation: a + bC <-> abC
        |]
        |> Array.sum


    // 374 - abD
    let d374 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[374] // abD | ligation: a + bD <-> abD
            1.0 * x.[7] * x.[54] // a + bD | ligation: a + bD <-> abD
        |]
        |> Array.sum


    // 375 - aba
    let d375 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[375] // aba | ligation: a + ba <-> aba
            1.0 * x.[7] * x.[55] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 376 - abb
    let d376 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[376] // abb | ligation: a + bb <-> abb
            1.0 * x.[7] * x.[56] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 377 - abc
    let d377 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[377] // abc | ligation: a + bc <-> abc
            1.0 * x.[7] * x.[57] // a + bc | ligation: a + bc <-> abc
        |]
        |> Array.sum


    // 378 - abd
    let d378 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[378] // abd | ligation: a + bd <-> abd
            1.0 * x.[7] * x.[58] // a + bd | ligation: a + bd <-> abd
        |]
        |> Array.sum


    // 379 - acA
    let d379 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[379] // acA | ligation: a + cA <-> acA
            1.0 * x.[7] * x.[59] // a + cA | ligation: a + cA <-> acA
        |]
        |> Array.sum


    // 380 - acB
    let d380 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[380] // acB | ligation: a + cB <-> acB
            1.0 * x.[7] * x.[60] // a + cB | ligation: a + cB <-> acB
        |]
        |> Array.sum


    // 381 - acC
    let d381 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[381] // acC | ligation: a + cC <-> acC
            1.0 * x.[7] * x.[61] // a + cC | ligation: a + cC <-> acC
        |]
        |> Array.sum


    // 382 - acD
    let d382 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[382] // acD | ligation: a + cD <-> acD
            1.0 * x.[7] * x.[62] // a + cD | ligation: a + cD <-> acD
        |]
        |> Array.sum


    // 383 - aca
    let d383 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[383] // aca | ligation: a + ca <-> aca
            1.0 * x.[7] * x.[63] // a + ca | ligation: a + ca <-> aca
        |]
        |> Array.sum


    // 384 - acb
    let d384 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[384] // acb | ligation: a + cb <-> acb
            1.0 * x.[7] * x.[64] // a + cb | ligation: a + cb <-> acb
        |]
        |> Array.sum


    // 385 - acc
    let d385 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[385] // acc | ligation: a + cc <-> acc
            1.0 * x.[7] * x.[65] // a + cc | ligation: a + cc <-> acc
        |]
        |> Array.sum


    // 386 - acd
    let d386 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[386] // acd | ligation: a + cd <-> acd
            1.0 * x.[7] * x.[66] // a + cd | ligation: a + cd <-> acd
        |]
        |> Array.sum


    // 387 - adA
    let d387 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[387] // adA | ligation: a + dA <-> adA
            1.0 * x.[7] * x.[67] // a + dA | ligation: a + dA <-> adA
        |]
        |> Array.sum


    // 388 - adB
    let d388 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[388] // adB | ligation: a + dB <-> adB
            1.0 * x.[7] * x.[68] // a + dB | ligation: a + dB <-> adB
        |]
        |> Array.sum


    // 389 - adC
    let d389 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[389] // adC | ligation: a + dC <-> adC
            1.0 * x.[7] * x.[69] // a + dC | ligation: a + dC <-> adC
        |]
        |> Array.sum


    // 390 - adD
    let d390 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[390] // adD | ligation: a + dD <-> adD
            1.0 * x.[7] * x.[70] // a + dD | ligation: a + dD <-> adD
        |]
        |> Array.sum


    // 391 - ada
    let d391 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[391] // ada | ligation: a + da <-> ada
            1.0 * x.[7] * x.[71] // a + da | ligation: a + da <-> ada
        |]
        |> Array.sum


    // 392 - adb
    let d392 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[392] // adb | ligation: a + db <-> adb
            1.0 * x.[7] * x.[72] // a + db | ligation: a + db <-> adb
        |]
        |> Array.sum


    // 393 - adc
    let d393 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[393] // adc | ligation: a + dc <-> adc
            1.0 * x.[7] * x.[73] // a + dc | ligation: a + dc <-> adc
        |]
        |> Array.sum


    // 394 - add
    let d394 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[394] // add | ligation: a + dd <-> add
            1.0 * x.[7] * x.[74] // a + dd | ligation: a + dd <-> add
        |]
        |> Array.sum


    // 395 - bAA
    let d395 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[395] // bAA | ligation: b + AA <-> bAA
            1.0 * x.[8] * x.[11] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 396 - bAB
    let d396 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[396] // bAB | ligation: b + AB <-> bAB
            1.0 * x.[8] * x.[12] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 397 - bAC
    let d397 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[397] // bAC | ligation: b + AC <-> bAC
            1.0 * x.[8] * x.[13] // b + AC | ligation: b + AC <-> bAC
        |]
        |> Array.sum


    // 398 - bAD
    let d398 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[398] // bAD | ligation: b + AD <-> bAD
            1.0 * x.[8] * x.[14] // b + AD | ligation: b + AD <-> bAD
        |]
        |> Array.sum


    // 399 - bAa
    let d399 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[399] // bAa | ligation: b + Aa <-> bAa
            1.0 * x.[8] * x.[15] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 400 - bAb
    let d400 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[400] // bAb | ligation: b + Ab <-> bAb
            1.0 * x.[8] * x.[16] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 401 - bAc
    let d401 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[401] // bAc | ligation: b + Ac <-> bAc
            1.0 * x.[8] * x.[17] // b + Ac | ligation: b + Ac <-> bAc
        |]
        |> Array.sum


    // 402 - bAd
    let d402 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[402] // bAd | ligation: b + Ad <-> bAd
            1.0 * x.[8] * x.[18] // b + Ad | ligation: b + Ad <-> bAd
        |]
        |> Array.sum


    // 403 - bBA
    let d403 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[403] // bBA | ligation: b + BA <-> bBA
            1.0 * x.[8] * x.[19] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 404 - bBB
    let d404 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[404] // bBB | ligation: b + BB <-> bBB
            1.0 * x.[8] * x.[20] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 405 - bBC
    let d405 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[405] // bBC | ligation: b + BC <-> bBC
            1.0 * x.[8] * x.[21] // b + BC | ligation: b + BC <-> bBC
        |]
        |> Array.sum


    // 406 - bBD
    let d406 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[406] // bBD | ligation: b + BD <-> bBD
            1.0 * x.[8] * x.[22] // b + BD | ligation: b + BD <-> bBD
        |]
        |> Array.sum


    // 407 - bBa
    let d407 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[407] // bBa | ligation: b + Ba <-> bBa
            1.0 * x.[8] * x.[23] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 408 - bBb
    let d408 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[408] // bBb | ligation: b + Bb <-> bBb
            1.0 * x.[8] * x.[24] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 409 - bBc
    let d409 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[409] // bBc | ligation: b + Bc <-> bBc
            1.0 * x.[8] * x.[25] // b + Bc | ligation: b + Bc <-> bBc
        |]
        |> Array.sum


    // 410 - bBd
    let d410 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[410] // bBd | ligation: b + Bd <-> bBd
            1.0 * x.[8] * x.[26] // b + Bd | ligation: b + Bd <-> bBd
        |]
        |> Array.sum


    // 411 - bCA
    let d411 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[411] // bCA | ligation: b + CA <-> bCA
            1.0 * x.[8] * x.[27] // b + CA | ligation: b + CA <-> bCA
        |]
        |> Array.sum


    // 412 - bCB
    let d412 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[412] // bCB | ligation: b + CB <-> bCB
            1.0 * x.[8] * x.[28] // b + CB | ligation: b + CB <-> bCB
        |]
        |> Array.sum


    // 413 - bCC
    let d413 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[413] // bCC | ligation: b + CC <-> bCC
            1.0 * x.[8] * x.[29] // b + CC | ligation: b + CC <-> bCC
        |]
        |> Array.sum


    // 414 - bCD
    let d414 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[414] // bCD | ligation: b + CD <-> bCD
            1.0 * x.[8] * x.[30] // b + CD | ligation: b + CD <-> bCD
        |]
        |> Array.sum


    // 415 - bCa
    let d415 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[415] // bCa | ligation: b + Ca <-> bCa
            1.0 * x.[8] * x.[31] // b + Ca | ligation: b + Ca <-> bCa
        |]
        |> Array.sum


    // 416 - bCb
    let d416 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[416] // bCb | ligation: b + Cb <-> bCb
            1.0 * x.[8] * x.[32] // b + Cb | ligation: b + Cb <-> bCb
        |]
        |> Array.sum


    // 417 - bCc
    let d417 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[417] // bCc | ligation: b + Cc <-> bCc
            1.0 * x.[8] * x.[33] // b + Cc | ligation: b + Cc <-> bCc
        |]
        |> Array.sum


    // 418 - bCd
    let d418 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[418] // bCd | ligation: b + Cd <-> bCd
            1.0 * x.[8] * x.[34] // b + Cd | ligation: b + Cd <-> bCd
        |]
        |> Array.sum


    // 419 - bDA
    let d419 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[419] // bDA | ligation: b + DA <-> bDA
            1.0 * x.[8] * x.[35] // b + DA | ligation: b + DA <-> bDA
        |]
        |> Array.sum


    // 420 - bDB
    let d420 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[420] // bDB | ligation: b + DB <-> bDB
            1.0 * x.[8] * x.[36] // b + DB | ligation: b + DB <-> bDB
        |]
        |> Array.sum


    // 421 - bDC
    let d421 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[421] // bDC | ligation: b + DC <-> bDC
            1.0 * x.[8] * x.[37] // b + DC | ligation: b + DC <-> bDC
        |]
        |> Array.sum


    // 422 - bDD
    let d422 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[422] // bDD | ligation: b + DD <-> bDD
            1.0 * x.[8] * x.[38] // b + DD | ligation: b + DD <-> bDD
        |]
        |> Array.sum


    // 423 - bDa
    let d423 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[423] // bDa | ligation: b + Da <-> bDa
            1.0 * x.[8] * x.[39] // b + Da | ligation: b + Da <-> bDa
        |]
        |> Array.sum


    // 424 - bDb
    let d424 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[424] // bDb | ligation: b + Db <-> bDb
            1.0 * x.[8] * x.[40] // b + Db | ligation: b + Db <-> bDb
        |]
        |> Array.sum


    // 425 - bDc
    let d425 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[425] // bDc | ligation: b + Dc <-> bDc
            1.0 * x.[8] * x.[41] // b + Dc | ligation: b + Dc <-> bDc
        |]
        |> Array.sum


    // 426 - bDd
    let d426 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[426] // bDd | ligation: b + Dd <-> bDd
            1.0 * x.[8] * x.[42] // b + Dd | ligation: b + Dd <-> bDd
        |]
        |> Array.sum


    // 427 - baA
    let d427 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[427] // baA | ligation: b + aA <-> baA
            1.0 * x.[8] * x.[43] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 428 - baB
    let d428 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[428] // baB | ligation: b + aB <-> baB
            1.0 * x.[8] * x.[44] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 429 - baC
    let d429 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[429] // baC | ligation: b + aC <-> baC
            1.0 * x.[8] * x.[45] // b + aC | ligation: b + aC <-> baC
        |]
        |> Array.sum


    // 430 - baD
    let d430 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[430] // baD | ligation: b + aD <-> baD
            1.0 * x.[8] * x.[46] // b + aD | ligation: b + aD <-> baD
        |]
        |> Array.sum


    // 431 - baa
    let d431 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[431] // baa | ligation: b + aa <-> baa
            1.0 * x.[8] * x.[47] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 432 - bab
    let d432 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[432] // bab | ligation: b + ab <-> bab
            1.0 * x.[8] * x.[48] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 433 - bac
    let d433 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[433] // bac | ligation: b + ac <-> bac
            1.0 * x.[8] * x.[49] // b + ac | ligation: b + ac <-> bac
        |]
        |> Array.sum


    // 434 - bad
    let d434 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[434] // bad | ligation: b + ad <-> bad
            1.0 * x.[8] * x.[50] // b + ad | ligation: b + ad <-> bad
        |]
        |> Array.sum


    // 435 - bbA
    let d435 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[435] // bbA | ligation: b + bA <-> bbA
            1.0 * x.[8] * x.[51] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 436 - bbB
    let d436 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[436] // bbB | ligation: b + bB <-> bbB
            1.0 * x.[8] * x.[52] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 437 - bbC
    let d437 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[437] // bbC | ligation: b + bC <-> bbC
            1.0 * x.[8] * x.[53] // b + bC | ligation: b + bC <-> bbC
        |]
        |> Array.sum


    // 438 - bbD
    let d438 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[438] // bbD | ligation: b + bD <-> bbD
            1.0 * x.[8] * x.[54] // b + bD | ligation: b + bD <-> bbD
        |]
        |> Array.sum


    // 439 - bba
    let d439 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[439] // bba | ligation: b + ba <-> bba
            1.0 * x.[8] * x.[55] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 440 - bbb
    let d440 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[440] // bbb | ligation: b + bb <-> bbb
            1.0 * x.[8] * x.[56] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum


    // 441 - bbc
    let d441 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[441] // bbc | ligation: b + bc <-> bbc
            1.0 * x.[8] * x.[57] // b + bc | ligation: b + bc <-> bbc
        |]
        |> Array.sum


    // 442 - bbd
    let d442 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[442] // bbd | ligation: b + bd <-> bbd
            1.0 * x.[8] * x.[58] // b + bd | ligation: b + bd <-> bbd
        |]
        |> Array.sum


    // 443 - bcA
    let d443 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[443] // bcA | ligation: b + cA <-> bcA
            1.0 * x.[8] * x.[59] // b + cA | ligation: b + cA <-> bcA
        |]
        |> Array.sum


    // 444 - bcB
    let d444 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[444] // bcB | ligation: b + cB <-> bcB
            1.0 * x.[8] * x.[60] // b + cB | ligation: b + cB <-> bcB
        |]
        |> Array.sum


    // 445 - bcC
    let d445 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[445] // bcC | ligation: b + cC <-> bcC
            1.0 * x.[8] * x.[61] // b + cC | ligation: b + cC <-> bcC
        |]
        |> Array.sum


    // 446 - bcD
    let d446 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[446] // bcD | ligation: b + cD <-> bcD
            1.0 * x.[8] * x.[62] // b + cD | ligation: b + cD <-> bcD
        |]
        |> Array.sum


    // 447 - bca
    let d447 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[447] // bca | ligation: b + ca <-> bca
            1.0 * x.[8] * x.[63] // b + ca | ligation: b + ca <-> bca
        |]
        |> Array.sum


    // 448 - bcb
    let d448 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[448] // bcb | ligation: b + cb <-> bcb
            1.0 * x.[8] * x.[64] // b + cb | ligation: b + cb <-> bcb
        |]
        |> Array.sum


    // 449 - bcc
    let d449 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[449] // bcc | ligation: b + cc <-> bcc
            1.0 * x.[8] * x.[65] // b + cc | ligation: b + cc <-> bcc
        |]
        |> Array.sum


    // 450 - bcd
    let d450 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[450] // bcd | ligation: b + cd <-> bcd
            1.0 * x.[8] * x.[66] // b + cd | ligation: b + cd <-> bcd
        |]
        |> Array.sum


    // 451 - bdA
    let d451 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[451] // bdA | ligation: b + dA <-> bdA
            1.0 * x.[8] * x.[67] // b + dA | ligation: b + dA <-> bdA
        |]
        |> Array.sum


    // 452 - bdB
    let d452 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[452] // bdB | ligation: b + dB <-> bdB
            1.0 * x.[8] * x.[68] // b + dB | ligation: b + dB <-> bdB
        |]
        |> Array.sum


    // 453 - bdC
    let d453 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[453] // bdC | ligation: b + dC <-> bdC
            1.0 * x.[8] * x.[69] // b + dC | ligation: b + dC <-> bdC
        |]
        |> Array.sum


    // 454 - bdD
    let d454 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[454] // bdD | ligation: b + dD <-> bdD
            1.0 * x.[8] * x.[70] // b + dD | ligation: b + dD <-> bdD
        |]
        |> Array.sum


    // 455 - bda
    let d455 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[455] // bda | ligation: b + da <-> bda
            1.0 * x.[8] * x.[71] // b + da | ligation: b + da <-> bda
        |]
        |> Array.sum


    // 456 - bdb
    let d456 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[456] // bdb | ligation: b + db <-> bdb
            1.0 * x.[8] * x.[72] // b + db | ligation: b + db <-> bdb
        |]
        |> Array.sum


    // 457 - bdc
    let d457 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[457] // bdc | ligation: b + dc <-> bdc
            1.0 * x.[8] * x.[73] // b + dc | ligation: b + dc <-> bdc
        |]
        |> Array.sum


    // 458 - bdd
    let d458 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[458] // bdd | ligation: b + dd <-> bdd
            1.0 * x.[8] * x.[74] // b + dd | ligation: b + dd <-> bdd
        |]
        |> Array.sum


    // 459 - cAA
    let d459 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[459] // cAA | ligation: c + AA <-> cAA
            1.0 * x.[9] * x.[11] // c + AA | ligation: c + AA <-> cAA
        |]
        |> Array.sum


    // 460 - cAB
    let d460 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[460] // cAB | ligation: c + AB <-> cAB
            1.0 * x.[9] * x.[12] // c + AB | ligation: c + AB <-> cAB
        |]
        |> Array.sum


    // 461 - cAC
    let d461 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[461] // cAC | ligation: c + AC <-> cAC
            1.0 * x.[9] * x.[13] // c + AC | ligation: c + AC <-> cAC
        |]
        |> Array.sum


    // 462 - cAD
    let d462 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[462] // cAD | ligation: c + AD <-> cAD
            1.0 * x.[9] * x.[14] // c + AD | ligation: c + AD <-> cAD
        |]
        |> Array.sum


    // 463 - cAa
    let d463 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[463] // cAa | ligation: c + Aa <-> cAa
            1.0 * x.[9] * x.[15] // c + Aa | ligation: c + Aa <-> cAa
        |]
        |> Array.sum


    // 464 - cAb
    let d464 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[464] // cAb | ligation: c + Ab <-> cAb
            1.0 * x.[9] * x.[16] // c + Ab | ligation: c + Ab <-> cAb
        |]
        |> Array.sum


    // 465 - cAc
    let d465 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[465] // cAc | ligation: c + Ac <-> cAc
            1.0 * x.[9] * x.[17] // c + Ac | ligation: c + Ac <-> cAc
        |]
        |> Array.sum


    // 466 - cAd
    let d466 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[466] // cAd | ligation: c + Ad <-> cAd
            1.0 * x.[9] * x.[18] // c + Ad | ligation: c + Ad <-> cAd
        |]
        |> Array.sum


    // 467 - cBA
    let d467 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[467] // cBA | ligation: c + BA <-> cBA
            1.0 * x.[9] * x.[19] // c + BA | ligation: c + BA <-> cBA
        |]
        |> Array.sum


    // 468 - cBB
    let d468 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[468] // cBB | ligation: c + BB <-> cBB
            1.0 * x.[9] * x.[20] // c + BB | ligation: c + BB <-> cBB
        |]
        |> Array.sum


    // 469 - cBC
    let d469 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[469] // cBC | ligation: c + BC <-> cBC
            1.0 * x.[9] * x.[21] // c + BC | ligation: c + BC <-> cBC
        |]
        |> Array.sum


    // 470 - cBD
    let d470 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[470] // cBD | ligation: c + BD <-> cBD
            1.0 * x.[9] * x.[22] // c + BD | ligation: c + BD <-> cBD
        |]
        |> Array.sum


    // 471 - cBa
    let d471 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[471] // cBa | ligation: c + Ba <-> cBa
            1.0 * x.[9] * x.[23] // c + Ba | ligation: c + Ba <-> cBa
        |]
        |> Array.sum


    // 472 - cBb
    let d472 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[472] // cBb | ligation: c + Bb <-> cBb
            1.0 * x.[9] * x.[24] // c + Bb | ligation: c + Bb <-> cBb
        |]
        |> Array.sum


    // 473 - cBc
    let d473 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[473] // cBc | ligation: c + Bc <-> cBc
            1.0 * x.[9] * x.[25] // c + Bc | ligation: c + Bc <-> cBc
        |]
        |> Array.sum


    // 474 - cBd
    let d474 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[474] // cBd | ligation: c + Bd <-> cBd
            1.0 * x.[9] * x.[26] // c + Bd | ligation: c + Bd <-> cBd
        |]
        |> Array.sum


    // 475 - cCA
    let d475 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[475] // cCA | ligation: c + CA <-> cCA
            1.0 * x.[9] * x.[27] // c + CA | ligation: c + CA <-> cCA
        |]
        |> Array.sum


    // 476 - cCB
    let d476 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[476] // cCB | ligation: c + CB <-> cCB
            1.0 * x.[9] * x.[28] // c + CB | ligation: c + CB <-> cCB
        |]
        |> Array.sum


    // 477 - cCC
    let d477 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[477] // cCC | ligation: c + CC <-> cCC
            1.0 * x.[9] * x.[29] // c + CC | ligation: c + CC <-> cCC
        |]
        |> Array.sum


    // 478 - cCD
    let d478 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[478] // cCD | ligation: c + CD <-> cCD
            1.0 * x.[9] * x.[30] // c + CD | ligation: c + CD <-> cCD
        |]
        |> Array.sum


    // 479 - cCa
    let d479 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[479] // cCa | ligation: c + Ca <-> cCa
            1.0 * x.[9] * x.[31] // c + Ca | ligation: c + Ca <-> cCa
        |]
        |> Array.sum


    // 480 - cCb
    let d480 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[480] // cCb | ligation: c + Cb <-> cCb
            1.0 * x.[9] * x.[32] // c + Cb | ligation: c + Cb <-> cCb
        |]
        |> Array.sum


    // 481 - cCc
    let d481 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[481] // cCc | ligation: c + Cc <-> cCc
            1.0 * x.[9] * x.[33] // c + Cc | ligation: c + Cc <-> cCc
        |]
        |> Array.sum


    // 482 - cCd
    let d482 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[482] // cCd | ligation: c + Cd <-> cCd
            1.0 * x.[9] * x.[34] // c + Cd | ligation: c + Cd <-> cCd
        |]
        |> Array.sum


    // 483 - cDA
    let d483 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[483] // cDA | ligation: c + DA <-> cDA
            1.0 * x.[9] * x.[35] // c + DA | ligation: c + DA <-> cDA
        |]
        |> Array.sum


    // 484 - cDB
    let d484 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[484] // cDB | ligation: c + DB <-> cDB
            1.0 * x.[9] * x.[36] // c + DB | ligation: c + DB <-> cDB
        |]
        |> Array.sum


    // 485 - cDC
    let d485 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[485] // cDC | ligation: c + DC <-> cDC
            1.0 * x.[9] * x.[37] // c + DC | ligation: c + DC <-> cDC
        |]
        |> Array.sum


    // 486 - cDD
    let d486 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[486] // cDD | ligation: c + DD <-> cDD
            1.0 * x.[9] * x.[38] // c + DD | ligation: c + DD <-> cDD
        |]
        |> Array.sum


    // 487 - cDa
    let d487 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[487] // cDa | ligation: c + Da <-> cDa
            1.0 * x.[9] * x.[39] // c + Da | ligation: c + Da <-> cDa
        |]
        |> Array.sum


    // 488 - cDb
    let d488 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[488] // cDb | ligation: c + Db <-> cDb
            1.0 * x.[9] * x.[40] // c + Db | ligation: c + Db <-> cDb
        |]
        |> Array.sum


    // 489 - cDc
    let d489 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[489] // cDc | ligation: c + Dc <-> cDc
            1.0 * x.[9] * x.[41] // c + Dc | ligation: c + Dc <-> cDc
        |]
        |> Array.sum


    // 490 - cDd
    let d490 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[490] // cDd | ligation: c + Dd <-> cDd
            1.0 * x.[9] * x.[42] // c + Dd | ligation: c + Dd <-> cDd
        |]
        |> Array.sum


    // 491 - caA
    let d491 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[491] // caA | ligation: c + aA <-> caA
            1.0 * x.[9] * x.[43] // c + aA | ligation: c + aA <-> caA
        |]
        |> Array.sum


    // 492 - caB
    let d492 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[492] // caB | ligation: c + aB <-> caB
            1.0 * x.[9] * x.[44] // c + aB | ligation: c + aB <-> caB
        |]
        |> Array.sum


    // 493 - caC
    let d493 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[493] // caC | ligation: c + aC <-> caC
            1.0 * x.[9] * x.[45] // c + aC | ligation: c + aC <-> caC
        |]
        |> Array.sum


    // 494 - caD
    let d494 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[494] // caD | ligation: c + aD <-> caD
            1.0 * x.[9] * x.[46] // c + aD | ligation: c + aD <-> caD
        |]
        |> Array.sum


    // 495 - caa
    let d495 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[495] // caa | ligation: c + aa <-> caa
            1.0 * x.[9] * x.[47] // c + aa | ligation: c + aa <-> caa
        |]
        |> Array.sum


    // 496 - cab
    let d496 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[496] // cab | ligation: c + ab <-> cab
            1.0 * x.[9] * x.[48] // c + ab | ligation: c + ab <-> cab
        |]
        |> Array.sum


    // 497 - cac
    let d497 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[497] // cac | ligation: c + ac <-> cac
            1.0 * x.[9] * x.[49] // c + ac | ligation: c + ac <-> cac
        |]
        |> Array.sum


    // 498 - cad
    let d498 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[498] // cad | ligation: c + ad <-> cad
            1.0 * x.[9] * x.[50] // c + ad | ligation: c + ad <-> cad
        |]
        |> Array.sum


    // 499 - cbA
    let d499 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[499] // cbA | ligation: c + bA <-> cbA
            1.0 * x.[9] * x.[51] // c + bA | ligation: c + bA <-> cbA
        |]
        |> Array.sum


    // 500 - cbB
    let d500 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[500] // cbB | ligation: c + bB <-> cbB
            1.0 * x.[9] * x.[52] // c + bB | ligation: c + bB <-> cbB
        |]
        |> Array.sum


    // 501 - cbC
    let d501 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[501] // cbC | ligation: c + bC <-> cbC
            1.0 * x.[9] * x.[53] // c + bC | ligation: c + bC <-> cbC
        |]
        |> Array.sum


    // 502 - cbD
    let d502 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[502] // cbD | ligation: c + bD <-> cbD
            1.0 * x.[9] * x.[54] // c + bD | ligation: c + bD <-> cbD
        |]
        |> Array.sum


    // 503 - cba
    let d503 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[503] // cba | ligation: c + ba <-> cba
            1.0 * x.[9] * x.[55] // c + ba | ligation: c + ba <-> cba
        |]
        |> Array.sum


    // 504 - cbb
    let d504 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[504] // cbb | ligation: c + bb <-> cbb
            1.0 * x.[9] * x.[56] // c + bb | ligation: c + bb <-> cbb
        |]
        |> Array.sum


    // 505 - cbc
    let d505 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[505] // cbc | ligation: c + bc <-> cbc
            1.0 * x.[9] * x.[57] // c + bc | ligation: c + bc <-> cbc
        |]
        |> Array.sum


    // 506 - cbd
    let d506 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[506] // cbd | ligation: c + bd <-> cbd
            1.0 * x.[9] * x.[58] // c + bd | ligation: c + bd <-> cbd
        |]
        |> Array.sum


    // 507 - ccA
    let d507 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[507] // ccA | ligation: c + cA <-> ccA
            1.0 * x.[9] * x.[59] // c + cA | ligation: c + cA <-> ccA
        |]
        |> Array.sum


    // 508 - ccB
    let d508 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[508] // ccB | ligation: c + cB <-> ccB
            1.0 * x.[9] * x.[60] // c + cB | ligation: c + cB <-> ccB
        |]
        |> Array.sum


    // 509 - ccC
    let d509 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[509] // ccC | ligation: c + cC <-> ccC
            1.0 * x.[9] * x.[61] // c + cC | ligation: c + cC <-> ccC
        |]
        |> Array.sum


    // 510 - ccD
    let d510 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[510] // ccD | ligation: c + cD <-> ccD
            1.0 * x.[9] * x.[62] // c + cD | ligation: c + cD <-> ccD
        |]
        |> Array.sum


    // 511 - cca
    let d511 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[511] // cca | ligation: c + ca <-> cca
            1.0 * x.[9] * x.[63] // c + ca | ligation: c + ca <-> cca
        |]
        |> Array.sum


    // 512 - ccb
    let d512 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[512] // ccb | ligation: c + cb <-> ccb
            1.0 * x.[9] * x.[64] // c + cb | ligation: c + cb <-> ccb
        |]
        |> Array.sum


    // 513 - ccc
    let d513 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[513] // ccc | ligation: c + cc <-> ccc
            1.0 * x.[9] * x.[65] // c + cc | ligation: c + cc <-> ccc
        |]
        |> Array.sum


    // 514 - ccd
    let d514 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[514] // ccd | ligation: c + cd <-> ccd
            1.0 * x.[9] * x.[66] // c + cd | ligation: c + cd <-> ccd
        |]
        |> Array.sum


    // 515 - cdA
    let d515 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[515] // cdA | ligation: c + dA <-> cdA
            1.0 * x.[9] * x.[67] // c + dA | ligation: c + dA <-> cdA
        |]
        |> Array.sum


    // 516 - cdB
    let d516 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[516] // cdB | ligation: c + dB <-> cdB
            1.0 * x.[9] * x.[68] // c + dB | ligation: c + dB <-> cdB
        |]
        |> Array.sum


    // 517 - cdC
    let d517 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[517] // cdC | ligation: c + dC <-> cdC
            1.0 * x.[9] * x.[69] // c + dC | ligation: c + dC <-> cdC
        |]
        |> Array.sum


    // 518 - cdD
    let d518 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[518] // cdD | ligation: c + dD <-> cdD
            1.0 * x.[9] * x.[70] // c + dD | ligation: c + dD <-> cdD
        |]
        |> Array.sum


    // 519 - cda
    let d519 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[519] // cda | ligation: c + da <-> cda
            1.0 * x.[9] * x.[71] // c + da | ligation: c + da <-> cda
        |]
        |> Array.sum


    // 520 - cdb
    let d520 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[520] // cdb | ligation: c + db <-> cdb
            1.0 * x.[9] * x.[72] // c + db | ligation: c + db <-> cdb
        |]
        |> Array.sum


    // 521 - cdc
    let d521 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[521] // cdc | ligation: c + dc <-> cdc
            1.0 * x.[9] * x.[73] // c + dc | ligation: c + dc <-> cdc
        |]
        |> Array.sum


    // 522 - cdd
    let d522 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[522] // cdd | ligation: c + dd <-> cdd
            1.0 * x.[9] * x.[74] // c + dd | ligation: c + dd <-> cdd
        |]
        |> Array.sum


    // 523 - dAA
    let d523 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[523] // dAA | ligation: d + AA <-> dAA
            1.0 * x.[10] * x.[11] // d + AA | ligation: d + AA <-> dAA
        |]
        |> Array.sum


    // 524 - dAB
    let d524 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[524] // dAB | ligation: d + AB <-> dAB
            1.0 * x.[10] * x.[12] // d + AB | ligation: d + AB <-> dAB
        |]
        |> Array.sum


    // 525 - dAC
    let d525 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[525] // dAC | ligation: d + AC <-> dAC
            1.0 * x.[10] * x.[13] // d + AC | ligation: d + AC <-> dAC
        |]
        |> Array.sum


    // 526 - dAD
    let d526 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[526] // dAD | ligation: d + AD <-> dAD
            1.0 * x.[10] * x.[14] // d + AD | ligation: d + AD <-> dAD
        |]
        |> Array.sum


    // 527 - dAa
    let d527 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[527] // dAa | ligation: d + Aa <-> dAa
            1.0 * x.[10] * x.[15] // d + Aa | ligation: d + Aa <-> dAa
        |]
        |> Array.sum


    // 528 - dAb
    let d528 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[528] // dAb | ligation: d + Ab <-> dAb
            1.0 * x.[10] * x.[16] // d + Ab | ligation: d + Ab <-> dAb
        |]
        |> Array.sum


    // 529 - dAc
    let d529 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[529] // dAc | ligation: d + Ac <-> dAc
            1.0 * x.[10] * x.[17] // d + Ac | ligation: d + Ac <-> dAc
        |]
        |> Array.sum


    // 530 - dAd
    let d530 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[530] // dAd | ligation: d + Ad <-> dAd
            1.0 * x.[10] * x.[18] // d + Ad | ligation: d + Ad <-> dAd
        |]
        |> Array.sum


    // 531 - dBA
    let d531 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[531] // dBA | ligation: d + BA <-> dBA
            1.0 * x.[10] * x.[19] // d + BA | ligation: d + BA <-> dBA
        |]
        |> Array.sum


    // 532 - dBB
    let d532 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[532] // dBB | ligation: d + BB <-> dBB
            1.0 * x.[10] * x.[20] // d + BB | ligation: d + BB <-> dBB
        |]
        |> Array.sum


    // 533 - dBC
    let d533 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[533] // dBC | ligation: d + BC <-> dBC
            1.0 * x.[10] * x.[21] // d + BC | ligation: d + BC <-> dBC
        |]
        |> Array.sum


    // 534 - dBD
    let d534 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[534] // dBD | ligation: d + BD <-> dBD
            1.0 * x.[10] * x.[22] // d + BD | ligation: d + BD <-> dBD
        |]
        |> Array.sum


    // 535 - dBa
    let d535 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[535] // dBa | ligation: d + Ba <-> dBa
            1.0 * x.[10] * x.[23] // d + Ba | ligation: d + Ba <-> dBa
        |]
        |> Array.sum


    // 536 - dBb
    let d536 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[536] // dBb | ligation: d + Bb <-> dBb
            1.0 * x.[10] * x.[24] // d + Bb | ligation: d + Bb <-> dBb
        |]
        |> Array.sum


    // 537 - dBc
    let d537 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[537] // dBc | ligation: d + Bc <-> dBc
            1.0 * x.[10] * x.[25] // d + Bc | ligation: d + Bc <-> dBc
        |]
        |> Array.sum


    // 538 - dBd
    let d538 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[538] // dBd | ligation: d + Bd <-> dBd
            1.0 * x.[10] * x.[26] // d + Bd | ligation: d + Bd <-> dBd
        |]
        |> Array.sum


    // 539 - dCA
    let d539 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[539] // dCA | ligation: d + CA <-> dCA
            1.0 * x.[10] * x.[27] // d + CA | ligation: d + CA <-> dCA
        |]
        |> Array.sum


    // 540 - dCB
    let d540 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[540] // dCB | ligation: d + CB <-> dCB
            1.0 * x.[10] * x.[28] // d + CB | ligation: d + CB <-> dCB
        |]
        |> Array.sum


    // 541 - dCC
    let d541 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[541] // dCC | ligation: d + CC <-> dCC
            1.0 * x.[10] * x.[29] // d + CC | ligation: d + CC <-> dCC
        |]
        |> Array.sum


    // 542 - dCD
    let d542 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[542] // dCD | ligation: d + CD <-> dCD
            1.0 * x.[10] * x.[30] // d + CD | ligation: d + CD <-> dCD
        |]
        |> Array.sum


    // 543 - dCa
    let d543 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[543] // dCa | ligation: d + Ca <-> dCa
            1.0 * x.[10] * x.[31] // d + Ca | ligation: d + Ca <-> dCa
        |]
        |> Array.sum


    // 544 - dCb
    let d544 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[544] // dCb | ligation: d + Cb <-> dCb
            1.0 * x.[10] * x.[32] // d + Cb | ligation: d + Cb <-> dCb
        |]
        |> Array.sum


    // 545 - dCc
    let d545 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[545] // dCc | ligation: d + Cc <-> dCc
            1.0 * x.[10] * x.[33] // d + Cc | ligation: d + Cc <-> dCc
        |]
        |> Array.sum


    // 546 - dCd
    let d546 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[546] // dCd | ligation: d + Cd <-> dCd
            1.0 * x.[10] * x.[34] // d + Cd | ligation: d + Cd <-> dCd
        |]
        |> Array.sum


    // 547 - dDA
    let d547 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[547] // dDA | ligation: d + DA <-> dDA
            1.0 * x.[10] * x.[35] // d + DA | ligation: d + DA <-> dDA
        |]
        |> Array.sum


    // 548 - dDB
    let d548 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[548] // dDB | ligation: d + DB <-> dDB
            1.0 * x.[10] * x.[36] // d + DB | ligation: d + DB <-> dDB
        |]
        |> Array.sum


    // 549 - dDC
    let d549 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[549] // dDC | ligation: d + DC <-> dDC
            1.0 * x.[10] * x.[37] // d + DC | ligation: d + DC <-> dDC
        |]
        |> Array.sum


    // 550 - dDD
    let d550 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[550] // dDD | ligation: d + DD <-> dDD
            1.0 * x.[10] * x.[38] // d + DD | ligation: d + DD <-> dDD
        |]
        |> Array.sum


    // 551 - dDa
    let d551 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[551] // dDa | ligation: d + Da <-> dDa
            1.0 * x.[10] * x.[39] // d + Da | ligation: d + Da <-> dDa
        |]
        |> Array.sum


    // 552 - dDb
    let d552 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[552] // dDb | ligation: d + Db <-> dDb
            1.0 * x.[10] * x.[40] // d + Db | ligation: d + Db <-> dDb
        |]
        |> Array.sum


    // 553 - dDc
    let d553 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[553] // dDc | ligation: d + Dc <-> dDc
            1.0 * x.[10] * x.[41] // d + Dc | ligation: d + Dc <-> dDc
        |]
        |> Array.sum


    // 554 - dDd
    let d554 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[554] // dDd | ligation: d + Dd <-> dDd
            1.0 * x.[10] * x.[42] // d + Dd | ligation: d + Dd <-> dDd
        |]
        |> Array.sum


    // 555 - daA
    let d555 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[555] // daA | ligation: d + aA <-> daA
            1.0 * x.[10] * x.[43] // d + aA | ligation: d + aA <-> daA
        |]
        |> Array.sum


    // 556 - daB
    let d556 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[556] // daB | ligation: d + aB <-> daB
            1.0 * x.[10] * x.[44] // d + aB | ligation: d + aB <-> daB
        |]
        |> Array.sum


    // 557 - daC
    let d557 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[557] // daC | ligation: d + aC <-> daC
            1.0 * x.[10] * x.[45] // d + aC | ligation: d + aC <-> daC
        |]
        |> Array.sum


    // 558 - daD
    let d558 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[558] // daD | ligation: d + aD <-> daD
            1.0 * x.[10] * x.[46] // d + aD | ligation: d + aD <-> daD
        |]
        |> Array.sum


    // 559 - daa
    let d559 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[559] // daa | ligation: d + aa <-> daa
            1.0 * x.[10] * x.[47] // d + aa | ligation: d + aa <-> daa
        |]
        |> Array.sum


    // 560 - dab
    let d560 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[560] // dab | ligation: d + ab <-> dab
            1.0 * x.[10] * x.[48] // d + ab | ligation: d + ab <-> dab
        |]
        |> Array.sum


    // 561 - dac
    let d561 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[561] // dac | ligation: d + ac <-> dac
            1.0 * x.[10] * x.[49] // d + ac | ligation: d + ac <-> dac
        |]
        |> Array.sum


    // 562 - dad
    let d562 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[562] // dad | ligation: d + ad <-> dad
            1.0 * x.[10] * x.[50] // d + ad | ligation: d + ad <-> dad
        |]
        |> Array.sum


    // 563 - dbA
    let d563 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[563] // dbA | ligation: d + bA <-> dbA
            1.0 * x.[10] * x.[51] // d + bA | ligation: d + bA <-> dbA
        |]
        |> Array.sum


    // 564 - dbB
    let d564 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[564] // dbB | ligation: d + bB <-> dbB
            1.0 * x.[10] * x.[52] // d + bB | ligation: d + bB <-> dbB
        |]
        |> Array.sum


    // 565 - dbC
    let d565 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[565] // dbC | ligation: d + bC <-> dbC
            1.0 * x.[10] * x.[53] // d + bC | ligation: d + bC <-> dbC
        |]
        |> Array.sum


    // 566 - dbD
    let d566 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[566] // dbD | ligation: d + bD <-> dbD
            1.0 * x.[10] * x.[54] // d + bD | ligation: d + bD <-> dbD
        |]
        |> Array.sum


    // 567 - dba
    let d567 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[567] // dba | ligation: d + ba <-> dba
            1.0 * x.[10] * x.[55] // d + ba | ligation: d + ba <-> dba
        |]
        |> Array.sum


    // 568 - dbb
    let d568 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[568] // dbb | ligation: d + bb <-> dbb
            1.0 * x.[10] * x.[56] // d + bb | ligation: d + bb <-> dbb
        |]
        |> Array.sum


    // 569 - dbc
    let d569 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[569] // dbc | ligation: d + bc <-> dbc
            1.0 * x.[10] * x.[57] // d + bc | ligation: d + bc <-> dbc
        |]
        |> Array.sum


    // 570 - dbd
    let d570 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[570] // dbd | ligation: d + bd <-> dbd
            1.0 * x.[10] * x.[58] // d + bd | ligation: d + bd <-> dbd
        |]
        |> Array.sum


    // 571 - dcA
    let d571 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[571] // dcA | ligation: d + cA <-> dcA
            1.0 * x.[10] * x.[59] // d + cA | ligation: d + cA <-> dcA
        |]
        |> Array.sum


    // 572 - dcB
    let d572 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[572] // dcB | ligation: d + cB <-> dcB
            1.0 * x.[10] * x.[60] // d + cB | ligation: d + cB <-> dcB
        |]
        |> Array.sum


    // 573 - dcC
    let d573 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[573] // dcC | ligation: d + cC <-> dcC
            1.0 * x.[10] * x.[61] // d + cC | ligation: d + cC <-> dcC
        |]
        |> Array.sum


    // 574 - dcD
    let d574 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[574] // dcD | ligation: d + cD <-> dcD
            1.0 * x.[10] * x.[62] // d + cD | ligation: d + cD <-> dcD
        |]
        |> Array.sum


    // 575 - dca
    let d575 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[575] // dca | ligation: d + ca <-> dca
            1.0 * x.[10] * x.[63] // d + ca | ligation: d + ca <-> dca
        |]
        |> Array.sum


    // 576 - dcb
    let d576 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[576] // dcb | ligation: d + cb <-> dcb
            1.0 * x.[10] * x.[64] // d + cb | ligation: d + cb <-> dcb
        |]
        |> Array.sum


    // 577 - dcc
    let d577 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[577] // dcc | ligation: d + cc <-> dcc
            1.0 * x.[10] * x.[65] // d + cc | ligation: d + cc <-> dcc
        |]
        |> Array.sum


    // 578 - dcd
    let d578 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[578] // dcd | ligation: d + cd <-> dcd
            1.0 * x.[10] * x.[66] // d + cd | ligation: d + cd <-> dcd
        |]
        |> Array.sum


    // 579 - ddA
    let d579 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[579] // ddA | ligation: d + dA <-> ddA
            1.0 * x.[10] * x.[67] // d + dA | ligation: d + dA <-> ddA
        |]
        |> Array.sum


    // 580 - ddB
    let d580 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[580] // ddB | ligation: d + dB <-> ddB
            1.0 * x.[10] * x.[68] // d + dB | ligation: d + dB <-> ddB
        |]
        |> Array.sum


    // 581 - ddC
    let d581 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[581] // ddC | ligation: d + dC <-> ddC
            1.0 * x.[10] * x.[69] // d + dC | ligation: d + dC <-> ddC
        |]
        |> Array.sum


    // 582 - ddD
    let d582 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[582] // ddD | ligation: d + dD <-> ddD
            1.0 * x.[10] * x.[70] // d + dD | ligation: d + dD <-> ddD
        |]
        |> Array.sum


    // 583 - dda
    let d583 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[583] // dda | ligation: d + da <-> dda
            1.0 * x.[10] * x.[71] // d + da | ligation: d + da <-> dda
        |]
        |> Array.sum


    // 584 - ddb
    let d584 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[584] // ddb | ligation: d + db <-> ddb
            1.0 * x.[10] * x.[72] // d + db | ligation: d + db <-> ddb
        |]
        |> Array.sum


    // 585 - ddc
    let d585 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[585] // ddc | ligation: d + dc <-> ddc
            1.0 * x.[10] * x.[73] // d + dc | ligation: d + dc <-> ddc
        |]
        |> Array.sum


    // 586 - ddd
    let d586 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -1.0 * x.[586] // ddd | ligation: d + dd <-> ddd
            1.0 * x.[10] * x.[74] // d + dd | ligation: d + dd <-> ddd
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
                1.0 * x.[6] // D
                1.0 * x.[7] // a
                1.0 * x.[8] // b
                1.0 * x.[9] // c
                1.0 * x.[10] // d
                2.0 * x.[11] // AA
                2.0 * x.[12] // AB
                2.0 * x.[13] // AC
                2.0 * x.[14] // AD
                2.0 * x.[15] // Aa
                2.0 * x.[16] // Ab
                2.0 * x.[17] // Ac
                2.0 * x.[18] // Ad
                2.0 * x.[19] // BA
                2.0 * x.[20] // BB
                2.0 * x.[21] // BC
                2.0 * x.[22] // BD
                2.0 * x.[23] // Ba
                2.0 * x.[24] // Bb
                2.0 * x.[25] // Bc
                2.0 * x.[26] // Bd
                2.0 * x.[27] // CA
                2.0 * x.[28] // CB
                2.0 * x.[29] // CC
                2.0 * x.[30] // CD
                2.0 * x.[31] // Ca
                2.0 * x.[32] // Cb
                2.0 * x.[33] // Cc
                2.0 * x.[34] // Cd
                2.0 * x.[35] // DA
                2.0 * x.[36] // DB
                2.0 * x.[37] // DC
                2.0 * x.[38] // DD
                2.0 * x.[39] // Da
                2.0 * x.[40] // Db
                2.0 * x.[41] // Dc
                2.0 * x.[42] // Dd
                2.0 * x.[43] // aA
                2.0 * x.[44] // aB
                2.0 * x.[45] // aC
                2.0 * x.[46] // aD
                2.0 * x.[47] // aa
                2.0 * x.[48] // ab
                2.0 * x.[49] // ac
                2.0 * x.[50] // ad
                2.0 * x.[51] // bA
                2.0 * x.[52] // bB
                2.0 * x.[53] // bC
                2.0 * x.[54] // bD
                2.0 * x.[55] // ba
                2.0 * x.[56] // bb
                2.0 * x.[57] // bc
                2.0 * x.[58] // bd
                2.0 * x.[59] // cA
                2.0 * x.[60] // cB
                2.0 * x.[61] // cC
                2.0 * x.[62] // cD
                2.0 * x.[63] // ca
                2.0 * x.[64] // cb
                2.0 * x.[65] // cc
                2.0 * x.[66] // cd
                2.0 * x.[67] // dA
                2.0 * x.[68] // dB
                2.0 * x.[69] // dC
                2.0 * x.[70] // dD
                2.0 * x.[71] // da
                2.0 * x.[72] // db
                2.0 * x.[73] // dc
                2.0 * x.[74] // dd
                3.0 * x.[75] // AAA
                3.0 * x.[76] // AAB
                3.0 * x.[77] // AAC
                3.0 * x.[78] // AAD
                3.0 * x.[79] // AAa
                3.0 * x.[80] // AAb
                3.0 * x.[81] // AAc
                3.0 * x.[82] // AAd
                3.0 * x.[83] // ABA
                3.0 * x.[84] // ABB
                3.0 * x.[85] // ABC
                3.0 * x.[86] // ABD
                3.0 * x.[87] // ABa
                3.0 * x.[88] // ABb
                3.0 * x.[89] // ABc
                3.0 * x.[90] // ABd
                3.0 * x.[91] // ACA
                3.0 * x.[92] // ACB
                3.0 * x.[93] // ACC
                3.0 * x.[94] // ACD
                3.0 * x.[95] // ACa
                3.0 * x.[96] // ACb
                3.0 * x.[97] // ACc
                3.0 * x.[98] // ACd
                3.0 * x.[99] // ADA
                3.0 * x.[100] // ADB
                3.0 * x.[101] // ADC
                3.0 * x.[102] // ADD
                3.0 * x.[103] // ADa
                3.0 * x.[104] // ADb
                3.0 * x.[105] // ADc
                3.0 * x.[106] // ADd
                3.0 * x.[107] // AaA
                3.0 * x.[108] // AaB
                3.0 * x.[109] // AaC
                3.0 * x.[110] // AaD
                3.0 * x.[111] // Aaa
                3.0 * x.[112] // Aab
                3.0 * x.[113] // Aac
                3.0 * x.[114] // Aad
                3.0 * x.[115] // AbA
                3.0 * x.[116] // AbB
                3.0 * x.[117] // AbC
                3.0 * x.[118] // AbD
                3.0 * x.[119] // Aba
                3.0 * x.[120] // Abb
                3.0 * x.[121] // Abc
                3.0 * x.[122] // Abd
                3.0 * x.[123] // AcA
                3.0 * x.[124] // AcB
                3.0 * x.[125] // AcC
                3.0 * x.[126] // AcD
                3.0 * x.[127] // Aca
                3.0 * x.[128] // Acb
                3.0 * x.[129] // Acc
                3.0 * x.[130] // Acd
                3.0 * x.[131] // AdA
                3.0 * x.[132] // AdB
                3.0 * x.[133] // AdC
                3.0 * x.[134] // AdD
                3.0 * x.[135] // Ada
                3.0 * x.[136] // Adb
                3.0 * x.[137] // Adc
                3.0 * x.[138] // Add
                3.0 * x.[139] // BAA
                3.0 * x.[140] // BAB
                3.0 * x.[141] // BAC
                3.0 * x.[142] // BAD
                3.0 * x.[143] // BAa
                3.0 * x.[144] // BAb
                3.0 * x.[145] // BAc
                3.0 * x.[146] // BAd
                3.0 * x.[147] // BBA
                3.0 * x.[148] // BBB
                3.0 * x.[149] // BBC
                3.0 * x.[150] // BBD
                3.0 * x.[151] // BBa
                3.0 * x.[152] // BBb
                3.0 * x.[153] // BBc
                3.0 * x.[154] // BBd
                3.0 * x.[155] // BCA
                3.0 * x.[156] // BCB
                3.0 * x.[157] // BCC
                3.0 * x.[158] // BCD
                3.0 * x.[159] // BCa
                3.0 * x.[160] // BCb
                3.0 * x.[161] // BCc
                3.0 * x.[162] // BCd
                3.0 * x.[163] // BDA
                3.0 * x.[164] // BDB
                3.0 * x.[165] // BDC
                3.0 * x.[166] // BDD
                3.0 * x.[167] // BDa
                3.0 * x.[168] // BDb
                3.0 * x.[169] // BDc
                3.0 * x.[170] // BDd
                3.0 * x.[171] // BaA
                3.0 * x.[172] // BaB
                3.0 * x.[173] // BaC
                3.0 * x.[174] // BaD
                3.0 * x.[175] // Baa
                3.0 * x.[176] // Bab
                3.0 * x.[177] // Bac
                3.0 * x.[178] // Bad
                3.0 * x.[179] // BbA
                3.0 * x.[180] // BbB
                3.0 * x.[181] // BbC
                3.0 * x.[182] // BbD
                3.0 * x.[183] // Bba
                3.0 * x.[184] // Bbb
                3.0 * x.[185] // Bbc
                3.0 * x.[186] // Bbd
                3.0 * x.[187] // BcA
                3.0 * x.[188] // BcB
                3.0 * x.[189] // BcC
                3.0 * x.[190] // BcD
                3.0 * x.[191] // Bca
                3.0 * x.[192] // Bcb
                3.0 * x.[193] // Bcc
                3.0 * x.[194] // Bcd
                3.0 * x.[195] // BdA
                3.0 * x.[196] // BdB
                3.0 * x.[197] // BdC
                3.0 * x.[198] // BdD
                3.0 * x.[199] // Bda
                3.0 * x.[200] // Bdb
                3.0 * x.[201] // Bdc
                3.0 * x.[202] // Bdd
                3.0 * x.[203] // CAA
                3.0 * x.[204] // CAB
                3.0 * x.[205] // CAC
                3.0 * x.[206] // CAD
                3.0 * x.[207] // CAa
                3.0 * x.[208] // CAb
                3.0 * x.[209] // CAc
                3.0 * x.[210] // CAd
                3.0 * x.[211] // CBA
                3.0 * x.[212] // CBB
                3.0 * x.[213] // CBC
                3.0 * x.[214] // CBD
                3.0 * x.[215] // CBa
                3.0 * x.[216] // CBb
                3.0 * x.[217] // CBc
                3.0 * x.[218] // CBd
                3.0 * x.[219] // CCA
                3.0 * x.[220] // CCB
                3.0 * x.[221] // CCC
                3.0 * x.[222] // CCD
                3.0 * x.[223] // CCa
                3.0 * x.[224] // CCb
                3.0 * x.[225] // CCc
                3.0 * x.[226] // CCd
                3.0 * x.[227] // CDA
                3.0 * x.[228] // CDB
                3.0 * x.[229] // CDC
                3.0 * x.[230] // CDD
                3.0 * x.[231] // CDa
                3.0 * x.[232] // CDb
                3.0 * x.[233] // CDc
                3.0 * x.[234] // CDd
                3.0 * x.[235] // CaA
                3.0 * x.[236] // CaB
                3.0 * x.[237] // CaC
                3.0 * x.[238] // CaD
                3.0 * x.[239] // Caa
                3.0 * x.[240] // Cab
                3.0 * x.[241] // Cac
                3.0 * x.[242] // Cad
                3.0 * x.[243] // CbA
                3.0 * x.[244] // CbB
                3.0 * x.[245] // CbC
                3.0 * x.[246] // CbD
                3.0 * x.[247] // Cba
                3.0 * x.[248] // Cbb
                3.0 * x.[249] // Cbc
                3.0 * x.[250] // Cbd
                3.0 * x.[251] // CcA
                3.0 * x.[252] // CcB
                3.0 * x.[253] // CcC
                3.0 * x.[254] // CcD
                3.0 * x.[255] // Cca
                3.0 * x.[256] // Ccb
                3.0 * x.[257] // Ccc
                3.0 * x.[258] // Ccd
                3.0 * x.[259] // CdA
                3.0 * x.[260] // CdB
                3.0 * x.[261] // CdC
                3.0 * x.[262] // CdD
                3.0 * x.[263] // Cda
                3.0 * x.[264] // Cdb
                3.0 * x.[265] // Cdc
                3.0 * x.[266] // Cdd
                3.0 * x.[267] // DAA
                3.0 * x.[268] // DAB
                3.0 * x.[269] // DAC
                3.0 * x.[270] // DAD
                3.0 * x.[271] // DAa
                3.0 * x.[272] // DAb
                3.0 * x.[273] // DAc
                3.0 * x.[274] // DAd
                3.0 * x.[275] // DBA
                3.0 * x.[276] // DBB
                3.0 * x.[277] // DBC
                3.0 * x.[278] // DBD
                3.0 * x.[279] // DBa
                3.0 * x.[280] // DBb
                3.0 * x.[281] // DBc
                3.0 * x.[282] // DBd
                3.0 * x.[283] // DCA
                3.0 * x.[284] // DCB
                3.0 * x.[285] // DCC
                3.0 * x.[286] // DCD
                3.0 * x.[287] // DCa
                3.0 * x.[288] // DCb
                3.0 * x.[289] // DCc
                3.0 * x.[290] // DCd
                3.0 * x.[291] // DDA
                3.0 * x.[292] // DDB
                3.0 * x.[293] // DDC
                3.0 * x.[294] // DDD
                3.0 * x.[295] // DDa
                3.0 * x.[296] // DDb
                3.0 * x.[297] // DDc
                3.0 * x.[298] // DDd
                3.0 * x.[299] // DaA
                3.0 * x.[300] // DaB
                3.0 * x.[301] // DaC
                3.0 * x.[302] // DaD
                3.0 * x.[303] // Daa
                3.0 * x.[304] // Dab
                3.0 * x.[305] // Dac
                3.0 * x.[306] // Dad
                3.0 * x.[307] // DbA
                3.0 * x.[308] // DbB
                3.0 * x.[309] // DbC
                3.0 * x.[310] // DbD
                3.0 * x.[311] // Dba
                3.0 * x.[312] // Dbb
                3.0 * x.[313] // Dbc
                3.0 * x.[314] // Dbd
                3.0 * x.[315] // DcA
                3.0 * x.[316] // DcB
                3.0 * x.[317] // DcC
                3.0 * x.[318] // DcD
                3.0 * x.[319] // Dca
                3.0 * x.[320] // Dcb
                3.0 * x.[321] // Dcc
                3.0 * x.[322] // Dcd
                3.0 * x.[323] // DdA
                3.0 * x.[324] // DdB
                3.0 * x.[325] // DdC
                3.0 * x.[326] // DdD
                3.0 * x.[327] // Dda
                3.0 * x.[328] // Ddb
                3.0 * x.[329] // Ddc
                3.0 * x.[330] // Ddd
                3.0 * x.[331] // aAA
                3.0 * x.[332] // aAB
                3.0 * x.[333] // aAC
                3.0 * x.[334] // aAD
                3.0 * x.[335] // aAa
                3.0 * x.[336] // aAb
                3.0 * x.[337] // aAc
                3.0 * x.[338] // aAd
                3.0 * x.[339] // aBA
                3.0 * x.[340] // aBB
                3.0 * x.[341] // aBC
                3.0 * x.[342] // aBD
                3.0 * x.[343] // aBa
                3.0 * x.[344] // aBb
                3.0 * x.[345] // aBc
                3.0 * x.[346] // aBd
                3.0 * x.[347] // aCA
                3.0 * x.[348] // aCB
                3.0 * x.[349] // aCC
                3.0 * x.[350] // aCD
                3.0 * x.[351] // aCa
                3.0 * x.[352] // aCb
                3.0 * x.[353] // aCc
                3.0 * x.[354] // aCd
                3.0 * x.[355] // aDA
                3.0 * x.[356] // aDB
                3.0 * x.[357] // aDC
                3.0 * x.[358] // aDD
                3.0 * x.[359] // aDa
                3.0 * x.[360] // aDb
                3.0 * x.[361] // aDc
                3.0 * x.[362] // aDd
                3.0 * x.[363] // aaA
                3.0 * x.[364] // aaB
                3.0 * x.[365] // aaC
                3.0 * x.[366] // aaD
                3.0 * x.[367] // aaa
                3.0 * x.[368] // aab
                3.0 * x.[369] // aac
                3.0 * x.[370] // aad
                3.0 * x.[371] // abA
                3.0 * x.[372] // abB
                3.0 * x.[373] // abC
                3.0 * x.[374] // abD
                3.0 * x.[375] // aba
                3.0 * x.[376] // abb
                3.0 * x.[377] // abc
                3.0 * x.[378] // abd
                3.0 * x.[379] // acA
                3.0 * x.[380] // acB
                3.0 * x.[381] // acC
                3.0 * x.[382] // acD
                3.0 * x.[383] // aca
                3.0 * x.[384] // acb
                3.0 * x.[385] // acc
                3.0 * x.[386] // acd
                3.0 * x.[387] // adA
                3.0 * x.[388] // adB
                3.0 * x.[389] // adC
                3.0 * x.[390] // adD
                3.0 * x.[391] // ada
                3.0 * x.[392] // adb
                3.0 * x.[393] // adc
                3.0 * x.[394] // add
                3.0 * x.[395] // bAA
                3.0 * x.[396] // bAB
                3.0 * x.[397] // bAC
                3.0 * x.[398] // bAD
                3.0 * x.[399] // bAa
                3.0 * x.[400] // bAb
                3.0 * x.[401] // bAc
                3.0 * x.[402] // bAd
                3.0 * x.[403] // bBA
                3.0 * x.[404] // bBB
                3.0 * x.[405] // bBC
                3.0 * x.[406] // bBD
                3.0 * x.[407] // bBa
                3.0 * x.[408] // bBb
                3.0 * x.[409] // bBc
                3.0 * x.[410] // bBd
                3.0 * x.[411] // bCA
                3.0 * x.[412] // bCB
                3.0 * x.[413] // bCC
                3.0 * x.[414] // bCD
                3.0 * x.[415] // bCa
                3.0 * x.[416] // bCb
                3.0 * x.[417] // bCc
                3.0 * x.[418] // bCd
                3.0 * x.[419] // bDA
                3.0 * x.[420] // bDB
                3.0 * x.[421] // bDC
                3.0 * x.[422] // bDD
                3.0 * x.[423] // bDa
                3.0 * x.[424] // bDb
                3.0 * x.[425] // bDc
                3.0 * x.[426] // bDd
                3.0 * x.[427] // baA
                3.0 * x.[428] // baB
                3.0 * x.[429] // baC
                3.0 * x.[430] // baD
                3.0 * x.[431] // baa
                3.0 * x.[432] // bab
                3.0 * x.[433] // bac
                3.0 * x.[434] // bad
                3.0 * x.[435] // bbA
                3.0 * x.[436] // bbB
                3.0 * x.[437] // bbC
                3.0 * x.[438] // bbD
                3.0 * x.[439] // bba
                3.0 * x.[440] // bbb
                3.0 * x.[441] // bbc
                3.0 * x.[442] // bbd
                3.0 * x.[443] // bcA
                3.0 * x.[444] // bcB
                3.0 * x.[445] // bcC
                3.0 * x.[446] // bcD
                3.0 * x.[447] // bca
                3.0 * x.[448] // bcb
                3.0 * x.[449] // bcc
                3.0 * x.[450] // bcd
                3.0 * x.[451] // bdA
                3.0 * x.[452] // bdB
                3.0 * x.[453] // bdC
                3.0 * x.[454] // bdD
                3.0 * x.[455] // bda
                3.0 * x.[456] // bdb
                3.0 * x.[457] // bdc
                3.0 * x.[458] // bdd
                3.0 * x.[459] // cAA
                3.0 * x.[460] // cAB
                3.0 * x.[461] // cAC
                3.0 * x.[462] // cAD
                3.0 * x.[463] // cAa
                3.0 * x.[464] // cAb
                3.0 * x.[465] // cAc
                3.0 * x.[466] // cAd
                3.0 * x.[467] // cBA
                3.0 * x.[468] // cBB
                3.0 * x.[469] // cBC
                3.0 * x.[470] // cBD
                3.0 * x.[471] // cBa
                3.0 * x.[472] // cBb
                3.0 * x.[473] // cBc
                3.0 * x.[474] // cBd
                3.0 * x.[475] // cCA
                3.0 * x.[476] // cCB
                3.0 * x.[477] // cCC
                3.0 * x.[478] // cCD
                3.0 * x.[479] // cCa
                3.0 * x.[480] // cCb
                3.0 * x.[481] // cCc
                3.0 * x.[482] // cCd
                3.0 * x.[483] // cDA
                3.0 * x.[484] // cDB
                3.0 * x.[485] // cDC
                3.0 * x.[486] // cDD
                3.0 * x.[487] // cDa
                3.0 * x.[488] // cDb
                3.0 * x.[489] // cDc
                3.0 * x.[490] // cDd
                3.0 * x.[491] // caA
                3.0 * x.[492] // caB
                3.0 * x.[493] // caC
                3.0 * x.[494] // caD
                3.0 * x.[495] // caa
                3.0 * x.[496] // cab
                3.0 * x.[497] // cac
                3.0 * x.[498] // cad
                3.0 * x.[499] // cbA
                3.0 * x.[500] // cbB
                3.0 * x.[501] // cbC
                3.0 * x.[502] // cbD
                3.0 * x.[503] // cba
                3.0 * x.[504] // cbb
                3.0 * x.[505] // cbc
                3.0 * x.[506] // cbd
                3.0 * x.[507] // ccA
                3.0 * x.[508] // ccB
                3.0 * x.[509] // ccC
                3.0 * x.[510] // ccD
                3.0 * x.[511] // cca
                3.0 * x.[512] // ccb
                3.0 * x.[513] // ccc
                3.0 * x.[514] // ccd
                3.0 * x.[515] // cdA
                3.0 * x.[516] // cdB
                3.0 * x.[517] // cdC
                3.0 * x.[518] // cdD
                3.0 * x.[519] // cda
                3.0 * x.[520] // cdb
                3.0 * x.[521] // cdc
                3.0 * x.[522] // cdd
                3.0 * x.[523] // dAA
                3.0 * x.[524] // dAB
                3.0 * x.[525] // dAC
                3.0 * x.[526] // dAD
                3.0 * x.[527] // dAa
                3.0 * x.[528] // dAb
                3.0 * x.[529] // dAc
                3.0 * x.[530] // dAd
                3.0 * x.[531] // dBA
                3.0 * x.[532] // dBB
                3.0 * x.[533] // dBC
                3.0 * x.[534] // dBD
                3.0 * x.[535] // dBa
                3.0 * x.[536] // dBb
                3.0 * x.[537] // dBc
                3.0 * x.[538] // dBd
                3.0 * x.[539] // dCA
                3.0 * x.[540] // dCB
                3.0 * x.[541] // dCC
                3.0 * x.[542] // dCD
                3.0 * x.[543] // dCa
                3.0 * x.[544] // dCb
                3.0 * x.[545] // dCc
                3.0 * x.[546] // dCd
                3.0 * x.[547] // dDA
                3.0 * x.[548] // dDB
                3.0 * x.[549] // dDC
                3.0 * x.[550] // dDD
                3.0 * x.[551] // dDa
                3.0 * x.[552] // dDb
                3.0 * x.[553] // dDc
                3.0 * x.[554] // dDd
                3.0 * x.[555] // daA
                3.0 * x.[556] // daB
                3.0 * x.[557] // daC
                3.0 * x.[558] // daD
                3.0 * x.[559] // daa
                3.0 * x.[560] // dab
                3.0 * x.[561] // dac
                3.0 * x.[562] // dad
                3.0 * x.[563] // dbA
                3.0 * x.[564] // dbB
                3.0 * x.[565] // dbC
                3.0 * x.[566] // dbD
                3.0 * x.[567] // dba
                3.0 * x.[568] // dbb
                3.0 * x.[569] // dbc
                3.0 * x.[570] // dbd
                3.0 * x.[571] // dcA
                3.0 * x.[572] // dcB
                3.0 * x.[573] // dcC
                3.0 * x.[574] // dcD
                3.0 * x.[575] // dca
                3.0 * x.[576] // dcb
                3.0 * x.[577] // dcc
                3.0 * x.[578] // dcd
                3.0 * x.[579] // ddA
                3.0 * x.[580] // ddB
                3.0 * x.[581] // ddC
                3.0 * x.[582] // ddD
                3.0 * x.[583] // dda
                3.0 * x.[584] // ddb
                3.0 * x.[585] // ddc
                3.0 * x.[586] // ddd
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[3] * x.[3] // A
                1.0 * x.[4] * x.[4] // B
                1.0 * x.[5] * x.[5] // C
                1.0 * x.[6] * x.[6] // D
                1.0 * x.[7] * x.[7] // a
                1.0 * x.[8] * x.[8] // b
                1.0 * x.[9] * x.[9] // c
                1.0 * x.[10] * x.[10] // d
                2.0 * x.[11] * x.[11] // AA
                2.0 * x.[12] * x.[12] // AB
                2.0 * x.[13] * x.[13] // AC
                2.0 * x.[14] * x.[14] // AD
                2.0 * x.[15] * x.[15] // Aa
                2.0 * x.[16] * x.[16] // Ab
                2.0 * x.[17] * x.[17] // Ac
                2.0 * x.[18] * x.[18] // Ad
                2.0 * x.[19] * x.[19] // BA
                2.0 * x.[20] * x.[20] // BB
                2.0 * x.[21] * x.[21] // BC
                2.0 * x.[22] * x.[22] // BD
                2.0 * x.[23] * x.[23] // Ba
                2.0 * x.[24] * x.[24] // Bb
                2.0 * x.[25] * x.[25] // Bc
                2.0 * x.[26] * x.[26] // Bd
                2.0 * x.[27] * x.[27] // CA
                2.0 * x.[28] * x.[28] // CB
                2.0 * x.[29] * x.[29] // CC
                2.0 * x.[30] * x.[30] // CD
                2.0 * x.[31] * x.[31] // Ca
                2.0 * x.[32] * x.[32] // Cb
                2.0 * x.[33] * x.[33] // Cc
                2.0 * x.[34] * x.[34] // Cd
                2.0 * x.[35] * x.[35] // DA
                2.0 * x.[36] * x.[36] // DB
                2.0 * x.[37] * x.[37] // DC
                2.0 * x.[38] * x.[38] // DD
                2.0 * x.[39] * x.[39] // Da
                2.0 * x.[40] * x.[40] // Db
                2.0 * x.[41] * x.[41] // Dc
                2.0 * x.[42] * x.[42] // Dd
                2.0 * x.[43] * x.[43] // aA
                2.0 * x.[44] * x.[44] // aB
                2.0 * x.[45] * x.[45] // aC
                2.0 * x.[46] * x.[46] // aD
                2.0 * x.[47] * x.[47] // aa
                2.0 * x.[48] * x.[48] // ab
                2.0 * x.[49] * x.[49] // ac
                2.0 * x.[50] * x.[50] // ad
                2.0 * x.[51] * x.[51] // bA
                2.0 * x.[52] * x.[52] // bB
                2.0 * x.[53] * x.[53] // bC
                2.0 * x.[54] * x.[54] // bD
                2.0 * x.[55] * x.[55] // ba
                2.0 * x.[56] * x.[56] // bb
                2.0 * x.[57] * x.[57] // bc
                2.0 * x.[58] * x.[58] // bd
                2.0 * x.[59] * x.[59] // cA
                2.0 * x.[60] * x.[60] // cB
                2.0 * x.[61] * x.[61] // cC
                2.0 * x.[62] * x.[62] // cD
                2.0 * x.[63] * x.[63] // ca
                2.0 * x.[64] * x.[64] // cb
                2.0 * x.[65] * x.[65] // cc
                2.0 * x.[66] * x.[66] // cd
                2.0 * x.[67] * x.[67] // dA
                2.0 * x.[68] * x.[68] // dB
                2.0 * x.[69] * x.[69] // dC
                2.0 * x.[70] * x.[70] // dD
                2.0 * x.[71] * x.[71] // da
                2.0 * x.[72] * x.[72] // db
                2.0 * x.[73] * x.[73] // dc
                2.0 * x.[74] * x.[74] // dd
                3.0 * x.[75] * x.[75] // AAA
                3.0 * x.[76] * x.[76] // AAB
                3.0 * x.[77] * x.[77] // AAC
                3.0 * x.[78] * x.[78] // AAD
                3.0 * x.[79] * x.[79] // AAa
                3.0 * x.[80] * x.[80] // AAb
                3.0 * x.[81] * x.[81] // AAc
                3.0 * x.[82] * x.[82] // AAd
                3.0 * x.[83] * x.[83] // ABA
                3.0 * x.[84] * x.[84] // ABB
                3.0 * x.[85] * x.[85] // ABC
                3.0 * x.[86] * x.[86] // ABD
                3.0 * x.[87] * x.[87] // ABa
                3.0 * x.[88] * x.[88] // ABb
                3.0 * x.[89] * x.[89] // ABc
                3.0 * x.[90] * x.[90] // ABd
                3.0 * x.[91] * x.[91] // ACA
                3.0 * x.[92] * x.[92] // ACB
                3.0 * x.[93] * x.[93] // ACC
                3.0 * x.[94] * x.[94] // ACD
                3.0 * x.[95] * x.[95] // ACa
                3.0 * x.[96] * x.[96] // ACb
                3.0 * x.[97] * x.[97] // ACc
                3.0 * x.[98] * x.[98] // ACd
                3.0 * x.[99] * x.[99] // ADA
                3.0 * x.[100] * x.[100] // ADB
                3.0 * x.[101] * x.[101] // ADC
                3.0 * x.[102] * x.[102] // ADD
                3.0 * x.[103] * x.[103] // ADa
                3.0 * x.[104] * x.[104] // ADb
                3.0 * x.[105] * x.[105] // ADc
                3.0 * x.[106] * x.[106] // ADd
                3.0 * x.[107] * x.[107] // AaA
                3.0 * x.[108] * x.[108] // AaB
                3.0 * x.[109] * x.[109] // AaC
                3.0 * x.[110] * x.[110] // AaD
                3.0 * x.[111] * x.[111] // Aaa
                3.0 * x.[112] * x.[112] // Aab
                3.0 * x.[113] * x.[113] // Aac
                3.0 * x.[114] * x.[114] // Aad
                3.0 * x.[115] * x.[115] // AbA
                3.0 * x.[116] * x.[116] // AbB
                3.0 * x.[117] * x.[117] // AbC
                3.0 * x.[118] * x.[118] // AbD
                3.0 * x.[119] * x.[119] // Aba
                3.0 * x.[120] * x.[120] // Abb
                3.0 * x.[121] * x.[121] // Abc
                3.0 * x.[122] * x.[122] // Abd
                3.0 * x.[123] * x.[123] // AcA
                3.0 * x.[124] * x.[124] // AcB
                3.0 * x.[125] * x.[125] // AcC
                3.0 * x.[126] * x.[126] // AcD
                3.0 * x.[127] * x.[127] // Aca
                3.0 * x.[128] * x.[128] // Acb
                3.0 * x.[129] * x.[129] // Acc
                3.0 * x.[130] * x.[130] // Acd
                3.0 * x.[131] * x.[131] // AdA
                3.0 * x.[132] * x.[132] // AdB
                3.0 * x.[133] * x.[133] // AdC
                3.0 * x.[134] * x.[134] // AdD
                3.0 * x.[135] * x.[135] // Ada
                3.0 * x.[136] * x.[136] // Adb
                3.0 * x.[137] * x.[137] // Adc
                3.0 * x.[138] * x.[138] // Add
                3.0 * x.[139] * x.[139] // BAA
                3.0 * x.[140] * x.[140] // BAB
                3.0 * x.[141] * x.[141] // BAC
                3.0 * x.[142] * x.[142] // BAD
                3.0 * x.[143] * x.[143] // BAa
                3.0 * x.[144] * x.[144] // BAb
                3.0 * x.[145] * x.[145] // BAc
                3.0 * x.[146] * x.[146] // BAd
                3.0 * x.[147] * x.[147] // BBA
                3.0 * x.[148] * x.[148] // BBB
                3.0 * x.[149] * x.[149] // BBC
                3.0 * x.[150] * x.[150] // BBD
                3.0 * x.[151] * x.[151] // BBa
                3.0 * x.[152] * x.[152] // BBb
                3.0 * x.[153] * x.[153] // BBc
                3.0 * x.[154] * x.[154] // BBd
                3.0 * x.[155] * x.[155] // BCA
                3.0 * x.[156] * x.[156] // BCB
                3.0 * x.[157] * x.[157] // BCC
                3.0 * x.[158] * x.[158] // BCD
                3.0 * x.[159] * x.[159] // BCa
                3.0 * x.[160] * x.[160] // BCb
                3.0 * x.[161] * x.[161] // BCc
                3.0 * x.[162] * x.[162] // BCd
                3.0 * x.[163] * x.[163] // BDA
                3.0 * x.[164] * x.[164] // BDB
                3.0 * x.[165] * x.[165] // BDC
                3.0 * x.[166] * x.[166] // BDD
                3.0 * x.[167] * x.[167] // BDa
                3.0 * x.[168] * x.[168] // BDb
                3.0 * x.[169] * x.[169] // BDc
                3.0 * x.[170] * x.[170] // BDd
                3.0 * x.[171] * x.[171] // BaA
                3.0 * x.[172] * x.[172] // BaB
                3.0 * x.[173] * x.[173] // BaC
                3.0 * x.[174] * x.[174] // BaD
                3.0 * x.[175] * x.[175] // Baa
                3.0 * x.[176] * x.[176] // Bab
                3.0 * x.[177] * x.[177] // Bac
                3.0 * x.[178] * x.[178] // Bad
                3.0 * x.[179] * x.[179] // BbA
                3.0 * x.[180] * x.[180] // BbB
                3.0 * x.[181] * x.[181] // BbC
                3.0 * x.[182] * x.[182] // BbD
                3.0 * x.[183] * x.[183] // Bba
                3.0 * x.[184] * x.[184] // Bbb
                3.0 * x.[185] * x.[185] // Bbc
                3.0 * x.[186] * x.[186] // Bbd
                3.0 * x.[187] * x.[187] // BcA
                3.0 * x.[188] * x.[188] // BcB
                3.0 * x.[189] * x.[189] // BcC
                3.0 * x.[190] * x.[190] // BcD
                3.0 * x.[191] * x.[191] // Bca
                3.0 * x.[192] * x.[192] // Bcb
                3.0 * x.[193] * x.[193] // Bcc
                3.0 * x.[194] * x.[194] // Bcd
                3.0 * x.[195] * x.[195] // BdA
                3.0 * x.[196] * x.[196] // BdB
                3.0 * x.[197] * x.[197] // BdC
                3.0 * x.[198] * x.[198] // BdD
                3.0 * x.[199] * x.[199] // Bda
                3.0 * x.[200] * x.[200] // Bdb
                3.0 * x.[201] * x.[201] // Bdc
                3.0 * x.[202] * x.[202] // Bdd
                3.0 * x.[203] * x.[203] // CAA
                3.0 * x.[204] * x.[204] // CAB
                3.0 * x.[205] * x.[205] // CAC
                3.0 * x.[206] * x.[206] // CAD
                3.0 * x.[207] * x.[207] // CAa
                3.0 * x.[208] * x.[208] // CAb
                3.0 * x.[209] * x.[209] // CAc
                3.0 * x.[210] * x.[210] // CAd
                3.0 * x.[211] * x.[211] // CBA
                3.0 * x.[212] * x.[212] // CBB
                3.0 * x.[213] * x.[213] // CBC
                3.0 * x.[214] * x.[214] // CBD
                3.0 * x.[215] * x.[215] // CBa
                3.0 * x.[216] * x.[216] // CBb
                3.0 * x.[217] * x.[217] // CBc
                3.0 * x.[218] * x.[218] // CBd
                3.0 * x.[219] * x.[219] // CCA
                3.0 * x.[220] * x.[220] // CCB
                3.0 * x.[221] * x.[221] // CCC
                3.0 * x.[222] * x.[222] // CCD
                3.0 * x.[223] * x.[223] // CCa
                3.0 * x.[224] * x.[224] // CCb
                3.0 * x.[225] * x.[225] // CCc
                3.0 * x.[226] * x.[226] // CCd
                3.0 * x.[227] * x.[227] // CDA
                3.0 * x.[228] * x.[228] // CDB
                3.0 * x.[229] * x.[229] // CDC
                3.0 * x.[230] * x.[230] // CDD
                3.0 * x.[231] * x.[231] // CDa
                3.0 * x.[232] * x.[232] // CDb
                3.0 * x.[233] * x.[233] // CDc
                3.0 * x.[234] * x.[234] // CDd
                3.0 * x.[235] * x.[235] // CaA
                3.0 * x.[236] * x.[236] // CaB
                3.0 * x.[237] * x.[237] // CaC
                3.0 * x.[238] * x.[238] // CaD
                3.0 * x.[239] * x.[239] // Caa
                3.0 * x.[240] * x.[240] // Cab
                3.0 * x.[241] * x.[241] // Cac
                3.0 * x.[242] * x.[242] // Cad
                3.0 * x.[243] * x.[243] // CbA
                3.0 * x.[244] * x.[244] // CbB
                3.0 * x.[245] * x.[245] // CbC
                3.0 * x.[246] * x.[246] // CbD
                3.0 * x.[247] * x.[247] // Cba
                3.0 * x.[248] * x.[248] // Cbb
                3.0 * x.[249] * x.[249] // Cbc
                3.0 * x.[250] * x.[250] // Cbd
                3.0 * x.[251] * x.[251] // CcA
                3.0 * x.[252] * x.[252] // CcB
                3.0 * x.[253] * x.[253] // CcC
                3.0 * x.[254] * x.[254] // CcD
                3.0 * x.[255] * x.[255] // Cca
                3.0 * x.[256] * x.[256] // Ccb
                3.0 * x.[257] * x.[257] // Ccc
                3.0 * x.[258] * x.[258] // Ccd
                3.0 * x.[259] * x.[259] // CdA
                3.0 * x.[260] * x.[260] // CdB
                3.0 * x.[261] * x.[261] // CdC
                3.0 * x.[262] * x.[262] // CdD
                3.0 * x.[263] * x.[263] // Cda
                3.0 * x.[264] * x.[264] // Cdb
                3.0 * x.[265] * x.[265] // Cdc
                3.0 * x.[266] * x.[266] // Cdd
                3.0 * x.[267] * x.[267] // DAA
                3.0 * x.[268] * x.[268] // DAB
                3.0 * x.[269] * x.[269] // DAC
                3.0 * x.[270] * x.[270] // DAD
                3.0 * x.[271] * x.[271] // DAa
                3.0 * x.[272] * x.[272] // DAb
                3.0 * x.[273] * x.[273] // DAc
                3.0 * x.[274] * x.[274] // DAd
                3.0 * x.[275] * x.[275] // DBA
                3.0 * x.[276] * x.[276] // DBB
                3.0 * x.[277] * x.[277] // DBC
                3.0 * x.[278] * x.[278] // DBD
                3.0 * x.[279] * x.[279] // DBa
                3.0 * x.[280] * x.[280] // DBb
                3.0 * x.[281] * x.[281] // DBc
                3.0 * x.[282] * x.[282] // DBd
                3.0 * x.[283] * x.[283] // DCA
                3.0 * x.[284] * x.[284] // DCB
                3.0 * x.[285] * x.[285] // DCC
                3.0 * x.[286] * x.[286] // DCD
                3.0 * x.[287] * x.[287] // DCa
                3.0 * x.[288] * x.[288] // DCb
                3.0 * x.[289] * x.[289] // DCc
                3.0 * x.[290] * x.[290] // DCd
                3.0 * x.[291] * x.[291] // DDA
                3.0 * x.[292] * x.[292] // DDB
                3.0 * x.[293] * x.[293] // DDC
                3.0 * x.[294] * x.[294] // DDD
                3.0 * x.[295] * x.[295] // DDa
                3.0 * x.[296] * x.[296] // DDb
                3.0 * x.[297] * x.[297] // DDc
                3.0 * x.[298] * x.[298] // DDd
                3.0 * x.[299] * x.[299] // DaA
                3.0 * x.[300] * x.[300] // DaB
                3.0 * x.[301] * x.[301] // DaC
                3.0 * x.[302] * x.[302] // DaD
                3.0 * x.[303] * x.[303] // Daa
                3.0 * x.[304] * x.[304] // Dab
                3.0 * x.[305] * x.[305] // Dac
                3.0 * x.[306] * x.[306] // Dad
                3.0 * x.[307] * x.[307] // DbA
                3.0 * x.[308] * x.[308] // DbB
                3.0 * x.[309] * x.[309] // DbC
                3.0 * x.[310] * x.[310] // DbD
                3.0 * x.[311] * x.[311] // Dba
                3.0 * x.[312] * x.[312] // Dbb
                3.0 * x.[313] * x.[313] // Dbc
                3.0 * x.[314] * x.[314] // Dbd
                3.0 * x.[315] * x.[315] // DcA
                3.0 * x.[316] * x.[316] // DcB
                3.0 * x.[317] * x.[317] // DcC
                3.0 * x.[318] * x.[318] // DcD
                3.0 * x.[319] * x.[319] // Dca
                3.0 * x.[320] * x.[320] // Dcb
                3.0 * x.[321] * x.[321] // Dcc
                3.0 * x.[322] * x.[322] // Dcd
                3.0 * x.[323] * x.[323] // DdA
                3.0 * x.[324] * x.[324] // DdB
                3.0 * x.[325] * x.[325] // DdC
                3.0 * x.[326] * x.[326] // DdD
                3.0 * x.[327] * x.[327] // Dda
                3.0 * x.[328] * x.[328] // Ddb
                3.0 * x.[329] * x.[329] // Ddc
                3.0 * x.[330] * x.[330] // Ddd
                3.0 * x.[331] * x.[331] // aAA
                3.0 * x.[332] * x.[332] // aAB
                3.0 * x.[333] * x.[333] // aAC
                3.0 * x.[334] * x.[334] // aAD
                3.0 * x.[335] * x.[335] // aAa
                3.0 * x.[336] * x.[336] // aAb
                3.0 * x.[337] * x.[337] // aAc
                3.0 * x.[338] * x.[338] // aAd
                3.0 * x.[339] * x.[339] // aBA
                3.0 * x.[340] * x.[340] // aBB
                3.0 * x.[341] * x.[341] // aBC
                3.0 * x.[342] * x.[342] // aBD
                3.0 * x.[343] * x.[343] // aBa
                3.0 * x.[344] * x.[344] // aBb
                3.0 * x.[345] * x.[345] // aBc
                3.0 * x.[346] * x.[346] // aBd
                3.0 * x.[347] * x.[347] // aCA
                3.0 * x.[348] * x.[348] // aCB
                3.0 * x.[349] * x.[349] // aCC
                3.0 * x.[350] * x.[350] // aCD
                3.0 * x.[351] * x.[351] // aCa
                3.0 * x.[352] * x.[352] // aCb
                3.0 * x.[353] * x.[353] // aCc
                3.0 * x.[354] * x.[354] // aCd
                3.0 * x.[355] * x.[355] // aDA
                3.0 * x.[356] * x.[356] // aDB
                3.0 * x.[357] * x.[357] // aDC
                3.0 * x.[358] * x.[358] // aDD
                3.0 * x.[359] * x.[359] // aDa
                3.0 * x.[360] * x.[360] // aDb
                3.0 * x.[361] * x.[361] // aDc
                3.0 * x.[362] * x.[362] // aDd
                3.0 * x.[363] * x.[363] // aaA
                3.0 * x.[364] * x.[364] // aaB
                3.0 * x.[365] * x.[365] // aaC
                3.0 * x.[366] * x.[366] // aaD
                3.0 * x.[367] * x.[367] // aaa
                3.0 * x.[368] * x.[368] // aab
                3.0 * x.[369] * x.[369] // aac
                3.0 * x.[370] * x.[370] // aad
                3.0 * x.[371] * x.[371] // abA
                3.0 * x.[372] * x.[372] // abB
                3.0 * x.[373] * x.[373] // abC
                3.0 * x.[374] * x.[374] // abD
                3.0 * x.[375] * x.[375] // aba
                3.0 * x.[376] * x.[376] // abb
                3.0 * x.[377] * x.[377] // abc
                3.0 * x.[378] * x.[378] // abd
                3.0 * x.[379] * x.[379] // acA
                3.0 * x.[380] * x.[380] // acB
                3.0 * x.[381] * x.[381] // acC
                3.0 * x.[382] * x.[382] // acD
                3.0 * x.[383] * x.[383] // aca
                3.0 * x.[384] * x.[384] // acb
                3.0 * x.[385] * x.[385] // acc
                3.0 * x.[386] * x.[386] // acd
                3.0 * x.[387] * x.[387] // adA
                3.0 * x.[388] * x.[388] // adB
                3.0 * x.[389] * x.[389] // adC
                3.0 * x.[390] * x.[390] // adD
                3.0 * x.[391] * x.[391] // ada
                3.0 * x.[392] * x.[392] // adb
                3.0 * x.[393] * x.[393] // adc
                3.0 * x.[394] * x.[394] // add
                3.0 * x.[395] * x.[395] // bAA
                3.0 * x.[396] * x.[396] // bAB
                3.0 * x.[397] * x.[397] // bAC
                3.0 * x.[398] * x.[398] // bAD
                3.0 * x.[399] * x.[399] // bAa
                3.0 * x.[400] * x.[400] // bAb
                3.0 * x.[401] * x.[401] // bAc
                3.0 * x.[402] * x.[402] // bAd
                3.0 * x.[403] * x.[403] // bBA
                3.0 * x.[404] * x.[404] // bBB
                3.0 * x.[405] * x.[405] // bBC
                3.0 * x.[406] * x.[406] // bBD
                3.0 * x.[407] * x.[407] // bBa
                3.0 * x.[408] * x.[408] // bBb
                3.0 * x.[409] * x.[409] // bBc
                3.0 * x.[410] * x.[410] // bBd
                3.0 * x.[411] * x.[411] // bCA
                3.0 * x.[412] * x.[412] // bCB
                3.0 * x.[413] * x.[413] // bCC
                3.0 * x.[414] * x.[414] // bCD
                3.0 * x.[415] * x.[415] // bCa
                3.0 * x.[416] * x.[416] // bCb
                3.0 * x.[417] * x.[417] // bCc
                3.0 * x.[418] * x.[418] // bCd
                3.0 * x.[419] * x.[419] // bDA
                3.0 * x.[420] * x.[420] // bDB
                3.0 * x.[421] * x.[421] // bDC
                3.0 * x.[422] * x.[422] // bDD
                3.0 * x.[423] * x.[423] // bDa
                3.0 * x.[424] * x.[424] // bDb
                3.0 * x.[425] * x.[425] // bDc
                3.0 * x.[426] * x.[426] // bDd
                3.0 * x.[427] * x.[427] // baA
                3.0 * x.[428] * x.[428] // baB
                3.0 * x.[429] * x.[429] // baC
                3.0 * x.[430] * x.[430] // baD
                3.0 * x.[431] * x.[431] // baa
                3.0 * x.[432] * x.[432] // bab
                3.0 * x.[433] * x.[433] // bac
                3.0 * x.[434] * x.[434] // bad
                3.0 * x.[435] * x.[435] // bbA
                3.0 * x.[436] * x.[436] // bbB
                3.0 * x.[437] * x.[437] // bbC
                3.0 * x.[438] * x.[438] // bbD
                3.0 * x.[439] * x.[439] // bba
                3.0 * x.[440] * x.[440] // bbb
                3.0 * x.[441] * x.[441] // bbc
                3.0 * x.[442] * x.[442] // bbd
                3.0 * x.[443] * x.[443] // bcA
                3.0 * x.[444] * x.[444] // bcB
                3.0 * x.[445] * x.[445] // bcC
                3.0 * x.[446] * x.[446] // bcD
                3.0 * x.[447] * x.[447] // bca
                3.0 * x.[448] * x.[448] // bcb
                3.0 * x.[449] * x.[449] // bcc
                3.0 * x.[450] * x.[450] // bcd
                3.0 * x.[451] * x.[451] // bdA
                3.0 * x.[452] * x.[452] // bdB
                3.0 * x.[453] * x.[453] // bdC
                3.0 * x.[454] * x.[454] // bdD
                3.0 * x.[455] * x.[455] // bda
                3.0 * x.[456] * x.[456] // bdb
                3.0 * x.[457] * x.[457] // bdc
                3.0 * x.[458] * x.[458] // bdd
                3.0 * x.[459] * x.[459] // cAA
                3.0 * x.[460] * x.[460] // cAB
                3.0 * x.[461] * x.[461] // cAC
                3.0 * x.[462] * x.[462] // cAD
                3.0 * x.[463] * x.[463] // cAa
                3.0 * x.[464] * x.[464] // cAb
                3.0 * x.[465] * x.[465] // cAc
                3.0 * x.[466] * x.[466] // cAd
                3.0 * x.[467] * x.[467] // cBA
                3.0 * x.[468] * x.[468] // cBB
                3.0 * x.[469] * x.[469] // cBC
                3.0 * x.[470] * x.[470] // cBD
                3.0 * x.[471] * x.[471] // cBa
                3.0 * x.[472] * x.[472] // cBb
                3.0 * x.[473] * x.[473] // cBc
                3.0 * x.[474] * x.[474] // cBd
                3.0 * x.[475] * x.[475] // cCA
                3.0 * x.[476] * x.[476] // cCB
                3.0 * x.[477] * x.[477] // cCC
                3.0 * x.[478] * x.[478] // cCD
                3.0 * x.[479] * x.[479] // cCa
                3.0 * x.[480] * x.[480] // cCb
                3.0 * x.[481] * x.[481] // cCc
                3.0 * x.[482] * x.[482] // cCd
                3.0 * x.[483] * x.[483] // cDA
                3.0 * x.[484] * x.[484] // cDB
                3.0 * x.[485] * x.[485] // cDC
                3.0 * x.[486] * x.[486] // cDD
                3.0 * x.[487] * x.[487] // cDa
                3.0 * x.[488] * x.[488] // cDb
                3.0 * x.[489] * x.[489] // cDc
                3.0 * x.[490] * x.[490] // cDd
                3.0 * x.[491] * x.[491] // caA
                3.0 * x.[492] * x.[492] // caB
                3.0 * x.[493] * x.[493] // caC
                3.0 * x.[494] * x.[494] // caD
                3.0 * x.[495] * x.[495] // caa
                3.0 * x.[496] * x.[496] // cab
                3.0 * x.[497] * x.[497] // cac
                3.0 * x.[498] * x.[498] // cad
                3.0 * x.[499] * x.[499] // cbA
                3.0 * x.[500] * x.[500] // cbB
                3.0 * x.[501] * x.[501] // cbC
                3.0 * x.[502] * x.[502] // cbD
                3.0 * x.[503] * x.[503] // cba
                3.0 * x.[504] * x.[504] // cbb
                3.0 * x.[505] * x.[505] // cbc
                3.0 * x.[506] * x.[506] // cbd
                3.0 * x.[507] * x.[507] // ccA
                3.0 * x.[508] * x.[508] // ccB
                3.0 * x.[509] * x.[509] // ccC
                3.0 * x.[510] * x.[510] // ccD
                3.0 * x.[511] * x.[511] // cca
                3.0 * x.[512] * x.[512] // ccb
                3.0 * x.[513] * x.[513] // ccc
                3.0 * x.[514] * x.[514] // ccd
                3.0 * x.[515] * x.[515] // cdA
                3.0 * x.[516] * x.[516] // cdB
                3.0 * x.[517] * x.[517] // cdC
                3.0 * x.[518] * x.[518] // cdD
                3.0 * x.[519] * x.[519] // cda
                3.0 * x.[520] * x.[520] // cdb
                3.0 * x.[521] * x.[521] // cdc
                3.0 * x.[522] * x.[522] // cdd
                3.0 * x.[523] * x.[523] // dAA
                3.0 * x.[524] * x.[524] // dAB
                3.0 * x.[525] * x.[525] // dAC
                3.0 * x.[526] * x.[526] // dAD
                3.0 * x.[527] * x.[527] // dAa
                3.0 * x.[528] * x.[528] // dAb
                3.0 * x.[529] * x.[529] // dAc
                3.0 * x.[530] * x.[530] // dAd
                3.0 * x.[531] * x.[531] // dBA
                3.0 * x.[532] * x.[532] // dBB
                3.0 * x.[533] * x.[533] // dBC
                3.0 * x.[534] * x.[534] // dBD
                3.0 * x.[535] * x.[535] // dBa
                3.0 * x.[536] * x.[536] // dBb
                3.0 * x.[537] * x.[537] // dBc
                3.0 * x.[538] * x.[538] // dBd
                3.0 * x.[539] * x.[539] // dCA
                3.0 * x.[540] * x.[540] // dCB
                3.0 * x.[541] * x.[541] // dCC
                3.0 * x.[542] * x.[542] // dCD
                3.0 * x.[543] * x.[543] // dCa
                3.0 * x.[544] * x.[544] // dCb
                3.0 * x.[545] * x.[545] // dCc
                3.0 * x.[546] * x.[546] // dCd
                3.0 * x.[547] * x.[547] // dDA
                3.0 * x.[548] * x.[548] // dDB
                3.0 * x.[549] * x.[549] // dDC
                3.0 * x.[550] * x.[550] // dDD
                3.0 * x.[551] * x.[551] // dDa
                3.0 * x.[552] * x.[552] // dDb
                3.0 * x.[553] * x.[553] // dDc
                3.0 * x.[554] * x.[554] // dDd
                3.0 * x.[555] * x.[555] // daA
                3.0 * x.[556] * x.[556] // daB
                3.0 * x.[557] * x.[557] // daC
                3.0 * x.[558] * x.[558] // daD
                3.0 * x.[559] * x.[559] // daa
                3.0 * x.[560] * x.[560] // dab
                3.0 * x.[561] * x.[561] // dac
                3.0 * x.[562] * x.[562] // dad
                3.0 * x.[563] * x.[563] // dbA
                3.0 * x.[564] * x.[564] // dbB
                3.0 * x.[565] * x.[565] // dbC
                3.0 * x.[566] * x.[566] // dbD
                3.0 * x.[567] * x.[567] // dba
                3.0 * x.[568] * x.[568] // dbb
                3.0 * x.[569] * x.[569] // dbc
                3.0 * x.[570] * x.[570] // dbd
                3.0 * x.[571] * x.[571] // dcA
                3.0 * x.[572] * x.[572] // dcB
                3.0 * x.[573] * x.[573] // dcC
                3.0 * x.[574] * x.[574] // dcD
                3.0 * x.[575] * x.[575] // dca
                3.0 * x.[576] * x.[576] // dcb
                3.0 * x.[577] * x.[577] // dcc
                3.0 * x.[578] * x.[578] // dcd
                3.0 * x.[579] * x.[579] // ddA
                3.0 * x.[580] * x.[580] // ddB
                3.0 * x.[581] * x.[581] // ddC
                3.0 * x.[582] * x.[582] // ddD
                3.0 * x.[583] * x.[583] // dda
                3.0 * x.[584] * x.[584] // ddb
                3.0 * x.[585] * x.[585] // ddc
                3.0 * x.[586] * x.[586] // ddd
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
            d261 x xSum xSumN xSumSquaredN
            d262 x xSum xSumN xSumSquaredN
            d263 x xSum xSumN xSumSquaredN
            d264 x xSum xSumN xSumSquaredN
            d265 x xSum xSumN xSumSquaredN
            d266 x xSum xSumN xSumSquaredN
            d267 x xSum xSumN xSumSquaredN
            d268 x xSum xSumN xSumSquaredN
            d269 x xSum xSumN xSumSquaredN
            d270 x xSum xSumN xSumSquaredN
            d271 x xSum xSumN xSumSquaredN
            d272 x xSum xSumN xSumSquaredN
            d273 x xSum xSumN xSumSquaredN
            d274 x xSum xSumN xSumSquaredN
            d275 x xSum xSumN xSumSquaredN
            d276 x xSum xSumN xSumSquaredN
            d277 x xSum xSumN xSumSquaredN
            d278 x xSum xSumN xSumSquaredN
            d279 x xSum xSumN xSumSquaredN
            d280 x xSum xSumN xSumSquaredN
            d281 x xSum xSumN xSumSquaredN
            d282 x xSum xSumN xSumSquaredN
            d283 x xSum xSumN xSumSquaredN
            d284 x xSum xSumN xSumSquaredN
            d285 x xSum xSumN xSumSquaredN
            d286 x xSum xSumN xSumSquaredN
            d287 x xSum xSumN xSumSquaredN
            d288 x xSum xSumN xSumSquaredN
            d289 x xSum xSumN xSumSquaredN
            d290 x xSum xSumN xSumSquaredN
            d291 x xSum xSumN xSumSquaredN
            d292 x xSum xSumN xSumSquaredN
            d293 x xSum xSumN xSumSquaredN
            d294 x xSum xSumN xSumSquaredN
            d295 x xSum xSumN xSumSquaredN
            d296 x xSum xSumN xSumSquaredN
            d297 x xSum xSumN xSumSquaredN
            d298 x xSum xSumN xSumSquaredN
            d299 x xSum xSumN xSumSquaredN
            d300 x xSum xSumN xSumSquaredN
            d301 x xSum xSumN xSumSquaredN
            d302 x xSum xSumN xSumSquaredN
            d303 x xSum xSumN xSumSquaredN
            d304 x xSum xSumN xSumSquaredN
            d305 x xSum xSumN xSumSquaredN
            d306 x xSum xSumN xSumSquaredN
            d307 x xSum xSumN xSumSquaredN
            d308 x xSum xSumN xSumSquaredN
            d309 x xSum xSumN xSumSquaredN
            d310 x xSum xSumN xSumSquaredN
            d311 x xSum xSumN xSumSquaredN
            d312 x xSum xSumN xSumSquaredN
            d313 x xSum xSumN xSumSquaredN
            d314 x xSum xSumN xSumSquaredN
            d315 x xSum xSumN xSumSquaredN
            d316 x xSum xSumN xSumSquaredN
            d317 x xSum xSumN xSumSquaredN
            d318 x xSum xSumN xSumSquaredN
            d319 x xSum xSumN xSumSquaredN
            d320 x xSum xSumN xSumSquaredN
            d321 x xSum xSumN xSumSquaredN
            d322 x xSum xSumN xSumSquaredN
            d323 x xSum xSumN xSumSquaredN
            d324 x xSum xSumN xSumSquaredN
            d325 x xSum xSumN xSumSquaredN
            d326 x xSum xSumN xSumSquaredN
            d327 x xSum xSumN xSumSquaredN
            d328 x xSum xSumN xSumSquaredN
            d329 x xSum xSumN xSumSquaredN
            d330 x xSum xSumN xSumSquaredN
            d331 x xSum xSumN xSumSquaredN
            d332 x xSum xSumN xSumSquaredN
            d333 x xSum xSumN xSumSquaredN
            d334 x xSum xSumN xSumSquaredN
            d335 x xSum xSumN xSumSquaredN
            d336 x xSum xSumN xSumSquaredN
            d337 x xSum xSumN xSumSquaredN
            d338 x xSum xSumN xSumSquaredN
            d339 x xSum xSumN xSumSquaredN
            d340 x xSum xSumN xSumSquaredN
            d341 x xSum xSumN xSumSquaredN
            d342 x xSum xSumN xSumSquaredN
            d343 x xSum xSumN xSumSquaredN
            d344 x xSum xSumN xSumSquaredN
            d345 x xSum xSumN xSumSquaredN
            d346 x xSum xSumN xSumSquaredN
            d347 x xSum xSumN xSumSquaredN
            d348 x xSum xSumN xSumSquaredN
            d349 x xSum xSumN xSumSquaredN
            d350 x xSum xSumN xSumSquaredN
            d351 x xSum xSumN xSumSquaredN
            d352 x xSum xSumN xSumSquaredN
            d353 x xSum xSumN xSumSquaredN
            d354 x xSum xSumN xSumSquaredN
            d355 x xSum xSumN xSumSquaredN
            d356 x xSum xSumN xSumSquaredN
            d357 x xSum xSumN xSumSquaredN
            d358 x xSum xSumN xSumSquaredN
            d359 x xSum xSumN xSumSquaredN
            d360 x xSum xSumN xSumSquaredN
            d361 x xSum xSumN xSumSquaredN
            d362 x xSum xSumN xSumSquaredN
            d363 x xSum xSumN xSumSquaredN
            d364 x xSum xSumN xSumSquaredN
            d365 x xSum xSumN xSumSquaredN
            d366 x xSum xSumN xSumSquaredN
            d367 x xSum xSumN xSumSquaredN
            d368 x xSum xSumN xSumSquaredN
            d369 x xSum xSumN xSumSquaredN
            d370 x xSum xSumN xSumSquaredN
            d371 x xSum xSumN xSumSquaredN
            d372 x xSum xSumN xSumSquaredN
            d373 x xSum xSumN xSumSquaredN
            d374 x xSum xSumN xSumSquaredN
            d375 x xSum xSumN xSumSquaredN
            d376 x xSum xSumN xSumSquaredN
            d377 x xSum xSumN xSumSquaredN
            d378 x xSum xSumN xSumSquaredN
            d379 x xSum xSumN xSumSquaredN
            d380 x xSum xSumN xSumSquaredN
            d381 x xSum xSumN xSumSquaredN
            d382 x xSum xSumN xSumSquaredN
            d383 x xSum xSumN xSumSquaredN
            d384 x xSum xSumN xSumSquaredN
            d385 x xSum xSumN xSumSquaredN
            d386 x xSum xSumN xSumSquaredN
            d387 x xSum xSumN xSumSquaredN
            d388 x xSum xSumN xSumSquaredN
            d389 x xSum xSumN xSumSquaredN
            d390 x xSum xSumN xSumSquaredN
            d391 x xSum xSumN xSumSquaredN
            d392 x xSum xSumN xSumSquaredN
            d393 x xSum xSumN xSumSquaredN
            d394 x xSum xSumN xSumSquaredN
            d395 x xSum xSumN xSumSquaredN
            d396 x xSum xSumN xSumSquaredN
            d397 x xSum xSumN xSumSquaredN
            d398 x xSum xSumN xSumSquaredN
            d399 x xSum xSumN xSumSquaredN
            d400 x xSum xSumN xSumSquaredN
            d401 x xSum xSumN xSumSquaredN
            d402 x xSum xSumN xSumSquaredN
            d403 x xSum xSumN xSumSquaredN
            d404 x xSum xSumN xSumSquaredN
            d405 x xSum xSumN xSumSquaredN
            d406 x xSum xSumN xSumSquaredN
            d407 x xSum xSumN xSumSquaredN
            d408 x xSum xSumN xSumSquaredN
            d409 x xSum xSumN xSumSquaredN
            d410 x xSum xSumN xSumSquaredN
            d411 x xSum xSumN xSumSquaredN
            d412 x xSum xSumN xSumSquaredN
            d413 x xSum xSumN xSumSquaredN
            d414 x xSum xSumN xSumSquaredN
            d415 x xSum xSumN xSumSquaredN
            d416 x xSum xSumN xSumSquaredN
            d417 x xSum xSumN xSumSquaredN
            d418 x xSum xSumN xSumSquaredN
            d419 x xSum xSumN xSumSquaredN
            d420 x xSum xSumN xSumSquaredN
            d421 x xSum xSumN xSumSquaredN
            d422 x xSum xSumN xSumSquaredN
            d423 x xSum xSumN xSumSquaredN
            d424 x xSum xSumN xSumSquaredN
            d425 x xSum xSumN xSumSquaredN
            d426 x xSum xSumN xSumSquaredN
            d427 x xSum xSumN xSumSquaredN
            d428 x xSum xSumN xSumSquaredN
            d429 x xSum xSumN xSumSquaredN
            d430 x xSum xSumN xSumSquaredN
            d431 x xSum xSumN xSumSquaredN
            d432 x xSum xSumN xSumSquaredN
            d433 x xSum xSumN xSumSquaredN
            d434 x xSum xSumN xSumSquaredN
            d435 x xSum xSumN xSumSquaredN
            d436 x xSum xSumN xSumSquaredN
            d437 x xSum xSumN xSumSquaredN
            d438 x xSum xSumN xSumSquaredN
            d439 x xSum xSumN xSumSquaredN
            d440 x xSum xSumN xSumSquaredN
            d441 x xSum xSumN xSumSquaredN
            d442 x xSum xSumN xSumSquaredN
            d443 x xSum xSumN xSumSquaredN
            d444 x xSum xSumN xSumSquaredN
            d445 x xSum xSumN xSumSquaredN
            d446 x xSum xSumN xSumSquaredN
            d447 x xSum xSumN xSumSquaredN
            d448 x xSum xSumN xSumSquaredN
            d449 x xSum xSumN xSumSquaredN
            d450 x xSum xSumN xSumSquaredN
            d451 x xSum xSumN xSumSquaredN
            d452 x xSum xSumN xSumSquaredN
            d453 x xSum xSumN xSumSquaredN
            d454 x xSum xSumN xSumSquaredN
            d455 x xSum xSumN xSumSquaredN
            d456 x xSum xSumN xSumSquaredN
            d457 x xSum xSumN xSumSquaredN
            d458 x xSum xSumN xSumSquaredN
            d459 x xSum xSumN xSumSquaredN
            d460 x xSum xSumN xSumSquaredN
            d461 x xSum xSumN xSumSquaredN
            d462 x xSum xSumN xSumSquaredN
            d463 x xSum xSumN xSumSquaredN
            d464 x xSum xSumN xSumSquaredN
            d465 x xSum xSumN xSumSquaredN
            d466 x xSum xSumN xSumSquaredN
            d467 x xSum xSumN xSumSquaredN
            d468 x xSum xSumN xSumSquaredN
            d469 x xSum xSumN xSumSquaredN
            d470 x xSum xSumN xSumSquaredN
            d471 x xSum xSumN xSumSquaredN
            d472 x xSum xSumN xSumSquaredN
            d473 x xSum xSumN xSumSquaredN
            d474 x xSum xSumN xSumSquaredN
            d475 x xSum xSumN xSumSquaredN
            d476 x xSum xSumN xSumSquaredN
            d477 x xSum xSumN xSumSquaredN
            d478 x xSum xSumN xSumSquaredN
            d479 x xSum xSumN xSumSquaredN
            d480 x xSum xSumN xSumSquaredN
            d481 x xSum xSumN xSumSquaredN
            d482 x xSum xSumN xSumSquaredN
            d483 x xSum xSumN xSumSquaredN
            d484 x xSum xSumN xSumSquaredN
            d485 x xSum xSumN xSumSquaredN
            d486 x xSum xSumN xSumSquaredN
            d487 x xSum xSumN xSumSquaredN
            d488 x xSum xSumN xSumSquaredN
            d489 x xSum xSumN xSumSquaredN
            d490 x xSum xSumN xSumSquaredN
            d491 x xSum xSumN xSumSquaredN
            d492 x xSum xSumN xSumSquaredN
            d493 x xSum xSumN xSumSquaredN
            d494 x xSum xSumN xSumSquaredN
            d495 x xSum xSumN xSumSquaredN
            d496 x xSum xSumN xSumSquaredN
            d497 x xSum xSumN xSumSquaredN
            d498 x xSum xSumN xSumSquaredN
            d499 x xSum xSumN xSumSquaredN
            d500 x xSum xSumN xSumSquaredN
            d501 x xSum xSumN xSumSquaredN
            d502 x xSum xSumN xSumSquaredN
            d503 x xSum xSumN xSumSquaredN
            d504 x xSum xSumN xSumSquaredN
            d505 x xSum xSumN xSumSquaredN
            d506 x xSum xSumN xSumSquaredN
            d507 x xSum xSumN xSumSquaredN
            d508 x xSum xSumN xSumSquaredN
            d509 x xSum xSumN xSumSquaredN
            d510 x xSum xSumN xSumSquaredN
            d511 x xSum xSumN xSumSquaredN
            d512 x xSum xSumN xSumSquaredN
            d513 x xSum xSumN xSumSquaredN
            d514 x xSum xSumN xSumSquaredN
            d515 x xSum xSumN xSumSquaredN
            d516 x xSum xSumN xSumSquaredN
            d517 x xSum xSumN xSumSquaredN
            d518 x xSum xSumN xSumSquaredN
            d519 x xSum xSumN xSumSquaredN
            d520 x xSum xSumN xSumSquaredN
            d521 x xSum xSumN xSumSquaredN
            d522 x xSum xSumN xSumSquaredN
            d523 x xSum xSumN xSumSquaredN
            d524 x xSum xSumN xSumSquaredN
            d525 x xSum xSumN xSumSquaredN
            d526 x xSum xSumN xSumSquaredN
            d527 x xSum xSumN xSumSquaredN
            d528 x xSum xSumN xSumSquaredN
            d529 x xSum xSumN xSumSquaredN
            d530 x xSum xSumN xSumSquaredN
            d531 x xSum xSumN xSumSquaredN
            d532 x xSum xSumN xSumSquaredN
            d533 x xSum xSumN xSumSquaredN
            d534 x xSum xSumN xSumSquaredN
            d535 x xSum xSumN xSumSquaredN
            d536 x xSum xSumN xSumSquaredN
            d537 x xSum xSumN xSumSquaredN
            d538 x xSum xSumN xSumSquaredN
            d539 x xSum xSumN xSumSquaredN
            d540 x xSum xSumN xSumSquaredN
            d541 x xSum xSumN xSumSquaredN
            d542 x xSum xSumN xSumSquaredN
            d543 x xSum xSumN xSumSquaredN
            d544 x xSum xSumN xSumSquaredN
            d545 x xSum xSumN xSumSquaredN
            d546 x xSum xSumN xSumSquaredN
            d547 x xSum xSumN xSumSquaredN
            d548 x xSum xSumN xSumSquaredN
            d549 x xSum xSumN xSumSquaredN
            d550 x xSum xSumN xSumSquaredN
            d551 x xSum xSumN xSumSquaredN
            d552 x xSum xSumN xSumSquaredN
            d553 x xSum xSumN xSumSquaredN
            d554 x xSum xSumN xSumSquaredN
            d555 x xSum xSumN xSumSquaredN
            d556 x xSum xSumN xSumSquaredN
            d557 x xSum xSumN xSumSquaredN
            d558 x xSum xSumN xSumSquaredN
            d559 x xSum xSumN xSumSquaredN
            d560 x xSum xSumN xSumSquaredN
            d561 x xSum xSumN xSumSquaredN
            d562 x xSum xSumN xSumSquaredN
            d563 x xSum xSumN xSumSquaredN
            d564 x xSum xSumN xSumSquaredN
            d565 x xSum xSumN xSumSquaredN
            d566 x xSum xSumN xSumSquaredN
            d567 x xSum xSumN xSumSquaredN
            d568 x xSum xSumN xSumSquaredN
            d569 x xSum xSumN xSumSquaredN
            d570 x xSum xSumN xSumSquaredN
            d571 x xSum xSumN xSumSquaredN
            d572 x xSum xSumN xSumSquaredN
            d573 x xSum xSumN xSumSquaredN
            d574 x xSum xSumN xSumSquaredN
            d575 x xSum xSumN xSumSquaredN
            d576 x xSum xSumN xSumSquaredN
            d577 x xSum xSumN xSumSquaredN
            d578 x xSum xSumN xSumSquaredN
            d579 x xSum xSumN xSumSquaredN
            d580 x xSum xSumN xSumSquaredN
            d581 x xSum xSumN xSumSquaredN
            d582 x xSum xSumN xSumSquaredN
            d583 x xSum xSumN xSumSquaredN
            d584 x xSum xSumN xSumSquaredN
            d585 x xSum xSumN xSumSquaredN
            d586 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = "1.5.0.0"
                            versionNumber = "1.5.0.2"
                            seedValue = seedValue
                            modelName = "000128"
                            numberOfSubstances = 587
                            numberOfAminoAcids = FourAminoAcids
                            maxPeptideLength = ThreeMax
                            updateAllModels = false
                            allResultsFile = @"C:\GitHub\ClmFSharp\Clm\Clm\..\Results\AllResults.fs"
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
                                        synthesisDistribution = DeltaDistribution(1926637845, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
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
                                        destructionDistribution = DeltaDistribution(1042347505, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
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
                                        catSynthRndEeParams = 
                                        {
                                            rateMultiplierDistr = TriangularDistribution(357952374, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                            eeForwardDistribution = BiDeltaDistribution(929477102, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                            eeBackwardDistribution = BiDeltaDistribution(1720448601, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                        }
                                    }
                                    |> CatSynthRndParam
                                    |> CatalyticSynthesisRateParam
                                usage = DependsOnParam
                            }

                            {
                                modelParam = 
                                    {
                                        simBaseDistribution = UniformDistribution(962999997, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
                                        getRateMultiplierDistr = DeltaRateMultDistrGetter
                                        getForwardEeDistr = DeltaEeDistributionGetter
                                        getBackwardEeDistr = DeltaEeDistributionGetter
                                    }
                                    |> CatSynthSimParam
                                    |> CatalyticSynthesisRateParam
                                usage = PrimaryParam
                            }

                            {
                                modelParam = 
                                    {
                                        catDestrRndEeParams = 
                                        {
                                            rateMultiplierDistr = TriangularDistribution(1835140595, { threshold = Some 0.002; scale = Some 10000.0; shift = None }) |> Triangular |> RateMultDistr
                                            eeForwardDistribution = BiDeltaDistribution(1218356398, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                            eeBackwardDistribution = BiDeltaDistribution(1774494135, { threshold = None; scale = Some 0.95; shift = None }) |> BiDelta |> EeDistribution |> Some
                                        }
                                    }
                                    |> CatDestrRndParam
                                    |> CatalyticDestructionRateParam
                                usage = DependsOnParam
                            }

                            {
                                modelParam = 
                                    {
                                        simBaseDistribution = UniformDistribution(1211304457, { threshold = Some 0.3; scale = None; shift = Some 1.0 }) |> Uniform
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
                                        ligationDistribution = DeltaDistribution(1152520211, { threshold = None; scale = None; shift = Some 1.0 }) |> Delta
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
                                        sedimentationDirectDistribution = TriangularDistribution(325673751, { threshold = Some 1E-05; scale = None; shift = None }) |> Triangular
                                        forwardScale = Some 10000.0
                                    }
                                    |> SedDirRndParam
                                    |> SedimentationDirectRateParam
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
                    (SynthesisName, 8)
                    (DestructionName, 8)
                    (CatalyticSynthesisName, 4608)
                    (CatalyticDestructionName, 4608)
                    (LigationName, 282)
                    (CatalyticLigationName, 162432)
                    (SedimentationDirectName, 118698)
                    (SedimentationAllName, 8)
                    (RacemizationName, 8)
                    (CatalyticRacemizationName, 4608)
                ]

            allReactions = 
                [
                    (WasteRecyclingName, 1)
                    (SynthesisName, 8)
                    (DestructionName, 8)
                    (LigationName, 564)
                ]
        }


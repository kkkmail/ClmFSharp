namespace Clm

module Substances =

    type NumberOfAminoAcids =
        | OneAminoAcid
        | TwoAminoAcids
        | ThreeAminoAcids
        | FourAminoAcids
        | FiveAminoAcids
        | SixAminoAcids
        | SevenAminoAcids
        | EightAminoAcids
        | NineAminoAcids
        | TenAminoAcids
        | ElevenAminoAcids
        | TwelveAminoAcids
        | ThirteenAminoAcids
        | FourteenAminoAcids
        | FifteenAminoAcids
        | SixteenAminoAcids

        | SeventeenAminoAcids
        | EighteenAminoAcids
        | NineteenAminoAcids
        | TwentyAminoAcids
        | TwentyOneAminoAcids
        | TwentyTwoAminoAcids
        | TwentyThreeAminoAcids
        | TwentyFourAminoAcids
        | TwentyFiveAminoAcids
        | TwentySixAminoAcids
        | TwentySevemnAminoAcids
        | TwentyEightAminoAcids
        | TwentyNineAminoAcids
        | ThirtyAminoAcids
        | ThirtyOneAminoAcids
        | ThirtyTwoAminoAcids

        member this.length =
            match this with
            | OneAminoAcid -> 1
            | TwoAminoAcids -> 2
            | ThreeAminoAcids -> 3
            | FourAminoAcids -> 4
            | FiveAminoAcids -> 5
            | SixAminoAcids -> 6
            | SevenAminoAcids -> 7
            | EightAminoAcids -> 8
            | NineAminoAcids -> 9
            | TenAminoAcids -> 10
            | ElevenAminoAcids -> 11
            | TwelveAminoAcids -> 12
            | ThirteenAminoAcids -> 13
            | FourteenAminoAcids -> 14
            | FifteenAminoAcids -> 15
            | SixteenAminoAcids -> 16

            | SeventeenAminoAcids -> 17
            | EighteenAminoAcids -> 18
            | NineteenAminoAcids -> 19
            | TwentyAminoAcids -> 20
            | TwentyOneAminoAcids -> 21
            | TwentyTwoAminoAcids -> 22
            | TwentyThreeAminoAcids -> 23
            | TwentyFourAminoAcids -> 24
            | TwentyFiveAminoAcids -> 25
            | TwentySixAminoAcids -> 26
            | TwentySevemnAminoAcids -> 27
            | TwentyEightAminoAcids -> 28
            | TwentyNineAminoAcids -> 29
            | ThirtyAminoAcids -> 30
            | ThirtyOneAminoAcids -> 31
            | ThirtyTwoAminoAcids -> 32

        static member all =
            [
                OneAminoAcid
                TwoAminoAcids
                ThreeAminoAcids
                FourAminoAcids
                FiveAminoAcids
                SixAminoAcids
                SevenAminoAcids
                EightAminoAcids
                NineAminoAcids
                TenAminoAcids
                ElevenAminoAcids
                TwelveAminoAcids
                ThirteenAminoAcids
                FourteenAminoAcids
                FifteenAminoAcids
                SixteenAminoAcids

                SeventeenAminoAcids
                EighteenAminoAcids
                NineteenAminoAcids
                TwentyAminoAcids
                TwentyOneAminoAcids
                TwentyTwoAminoAcids
                TwentyThreeAminoAcids
                TwentyFourAminoAcids
                TwentyFiveAminoAcids
                TwentySixAminoAcids
                TwentySevemnAminoAcids
                TwentyEightAminoAcids
                TwentyNineAminoAcids
                ThirtyAminoAcids
                ThirtyOneAminoAcids
                ThirtyTwoAminoAcids
            ]

        static member tryCreate n =
            NumberOfAminoAcids.all |> List.tryPick (fun a -> if a.length = n then Some a else None)


    type MaxPeptideLength =
        | TwoMax
        | ThreeMax
        | FourMax
        | FiveMax

        member this.length =
            match this with
            | TwoMax -> 2
            | ThreeMax -> 3
            | FourMax -> 4
            | FiveMax -> 5

        static member all =
            [
                TwoMax
                ThreeMax
                FourMax
                FiveMax
            ]

        static member tryCreate n =
            MaxPeptideLength.all |> List.tryPick (fun a -> if a.length = n then Some a else None)


    type AchiralSubst =
        | Abundant
        | Food
        | Waste

        static member all =
            [
                Abundant
                Food
                Waste
            ]

        member __.length = 0

        member this.name =
            match this with
            | Abundant -> "X"
            | Food -> "Y"
            | Waste -> "Z"

        member __.atoms = 1
        member this.enantiomer = this


    type SumSubst =
        | SumSubst

        member __.length = 0
        member __.name = "W"
        static member w = SumSubst


    type AminoAcid =
        | A01
        | A02
        | A03
        | A04
        | A05
        | A06
        | A07
        | A08
        | A09
        | A10
        | A11
        | A12
        | A13
        | A14
        | A15
        | A16

        | A17
        | A18
        | A19
        | A20
        | A21
        | A22
        | A23
        | A24
        | A25
        | A26
        | A27
        | A28
        | A29
        | A30
        | A31
        | A32

        static member all =
            [
                A01
                A02
                A03
                A04
                A05
                A06
                A07
                A08
                A09
                A10
                A11
                A12
                A13
                A14
                A15
                A16

                A17
                A18
                A19
                A20
                A21
                A22
                A23
                A24
                A25
                A26
                A27
                A28
                A29
                A30
                A31
                A32
            ]

        member aminoAcid.name =
            match aminoAcid with
            | A01 -> "A"
            | A02 -> "B"
            | A03 -> "C"
            | A04 -> "D"
            | A05 -> "E"
            | A06 -> "F"
            | A07 -> "G"
            | A08 -> "H"
            | A09 -> "I"
            | A10 -> "J"
            | A11 -> "K"
            | A12 -> "L"
            | A13 -> "M"
            | A14 -> "N"
            | A15 -> "O"
            | A16 -> "P"

            | A17 -> "A1"
            | A18 -> "B1"
            | A19 -> "C1"
            | A20 -> "D1"
            | A21 -> "E1"
            | A22 -> "F1"
            | A23 -> "G1"
            | A24 -> "H1"
            | A25 -> "I1"
            | A26 -> "J1"
            | A27 -> "K1"
            | A28 -> "L1"
            | A29 -> "M1"
            | A30 -> "N1"
            | A31 -> "O1"
            | A32 -> "P1"

        member aminoAcid.number =
            match aminoAcid with
            | A01 -> 0
            | A02 -> 1
            | A03 -> 2
            | A04 -> 3
            | A05 -> 4
            | A06 -> 5
            | A07 -> 6
            | A08 -> 7
            | A09 -> 8
            | A10 -> 9
            | A11 -> 10
            | A12 -> 11
            | A13 -> 12
            | A14 -> 13
            | A15 -> 14
            | A16 -> 15

            | A17 -> 16
            | A18 -> 17
            | A19 -> 18
            | A20 -> 19
            | A21 -> 20
            | A22 -> 21
            | A23 -> 22
            | A24 -> 23
            | A25 -> 24
            | A26 -> 25
            | A27 -> 26
            | A28 -> 27
            | A29 -> 28
            | A30 -> 29
            | A31 -> 30
            | A32 -> 31

        static member toString (a : AminoAcid) = a.name

        static member toString (i : int) = 
            match AminoAcid.all |> List.tryFind(fun a -> a.number = i) with
            | Some a -> AminoAcid.toString a
            | None -> sprintf "Invalid amino acid index %A" i

        static member names = AminoAcid.all |> List.map (fun e -> e, e.name) |> Map.ofList

        static member getAminoAcids (n :NumberOfAminoAcids) =
            AminoAcid.all
            |> List.take n.length


    type ChiralAminoAcid =
        | L of AminoAcid
        | R of AminoAcid

        member __.length = 1
        member __.atoms = 1

        member aminoAcid.isL =
            match aminoAcid with
            | L _ -> true
            | R _ -> false

        member aminoAcid.isR = aminoAcid.isL |> not

        member aminoAcid.enantiomer =
            match aminoAcid with
            | L a -> R a
            | R a -> L a

        static member getAminoAcids n = 
            (AminoAcid.getAminoAcids n |> List.map (fun a -> L a))
            @
            (AminoAcid.getAminoAcids n |> List.map (fun a -> R a))

        member aminoAcid.name =
            match aminoAcid with
            | L a -> a.name
            | R a -> a.name.ToLower()

        member aminoAcid.noOfLR =
            match aminoAcid with
            | L _ -> (1, 0)
            | R _ -> (0, 1)

        member aminoAcid.createSameChirality a =
            match aminoAcid with
            | L _ -> L a
            | R _ -> R a


    type Peptide =
        | Peptide of list<ChiralAminoAcid>

        member peptide.length =
            let (Peptide p) = peptide
            p.Length

        member peptide.atoms = peptide.length

        member peptide.enantiomer =
            let (Peptide p) = peptide
            p |> List.map (fun a -> a.enantiomer) |> Peptide

        member peptide.aminoAcids =
            let (Peptide p) = peptide
            p

        member peptide.name =
            peptide.aminoAcids
            |> List.map (fun a -> a.name)
            |> String.concat ""

        member peptide.noOfLR =
            let counts =
                peptide.aminoAcids
                |> List.countBy (fun a -> a.isL)
                |> Map.ofList

            let count v =
                match counts.TryFind v with 
                | Some c -> c
                | None -> 0

            (count true, count false)

        member peptide.noOfAminoAcids =
            let (Peptide p) = peptide
            let count a = p |> List.sumBy (fun b -> if a = b then 1 else 0)
            AminoAcid.all |> List.map (fun a -> L a |> count, R a |> count)

        static member private create m n =
            let rec makePeptide acc l =
                //printfn "makePeptide::l = %A" l
                //printfn "makePeptide::acc = %A" acc
                match l with
                | [] -> acc
                | h :: t -> 
                    match acc with 
                    | [] -> makePeptide (h |> List.map (fun e -> [e])) t
                    | _ -> 
                        //let pairs = (List.allPairs h acc)
                        //printfn "makePeptide::pairs = %A" pairs
                        //let x = pairs |> List.map (fun e -> e)
                        makePeptide ((List.allPairs h acc) |> List.map (fun (a, e) -> a :: e)) t

            let aa = ChiralAminoAcid.getAminoAcids n
            //printfn "aa = %A" aa
            [ for _ in 1..m -> aa ]
            |> makePeptide []
            |> List.map (fun e -> Peptide e)

        /// Peptides start from length 2.
        static member getPeptides (m : MaxPeptideLength) n =
            [ for i in 2..m.length -> Peptide.create i n]
            |> List.concat


    type Substance =
        | Simple of AchiralSubst
        | Chiral of ChiralAminoAcid
        | PeptideChain of Peptide
        | Sum of SumSubst

        member substance.enantiomer =
            match substance with 
            | Simple f -> f |> Simple
            | Chiral c -> c.enantiomer |> Chiral
            | PeptideChain p -> p.enantiomer |> PeptideChain
            | Sum s -> s |> Sum

        member substance.name =
            match substance with
            | Simple f -> f.name
            | Chiral c -> c.name
            | PeptideChain p -> p.name
            | Sum s -> s.name

        member substance.noOfAminoAcid a =
            match substance with
            | Simple _ -> None
            | Chiral c ->
                match c = a with
                | true -> Some 1
                | false -> None
            | PeptideChain (Peptide p) ->
                match (p |> List.sumBy (fun b -> if a = b then 1 else 0)) with
                | 0 -> None
                | n -> Some n
            | Sum _ -> None

        member substance.isSimple =
            match substance with
            | Simple _ -> true
            | Chiral _ -> false
            | PeptideChain _ -> false
            | Sum _ -> false

        member substance.atoms =
            match substance with
            | Simple f -> f.atoms
            | Chiral c -> c.atoms
            | PeptideChain p -> p.atoms
            | Sum _ -> 0

        member substance.length =
            match substance with
            | Simple _ -> 0
            | Chiral c -> c.atoms
            | PeptideChain p -> p.atoms
            | Sum _ -> 0

        member substance.aminoAcids =
            match substance with
            | Simple _ -> []
            | Chiral c -> [ c ]
            | PeptideChain p -> p.aminoAcids
            | Sum _ -> []

        static member food = AchiralSubst.Food |> Simple
        static member waste = AchiralSubst.Waste |> Simple
        static member abundant = AchiralSubst.Abundant |> Simple

        static member allSimple =
            AchiralSubst.all
            |> List.map (fun e -> e |> Simple)

        static member chiralL a = a |> L |> Chiral

        static member fromList (a : list<ChiralAminoAcid>) =
            match a.Length with 
            | 1 -> Chiral a.Head
            | _ -> Peptide a |> PeptideChain


    /// Maps substances to array / vector indices.
    type SubstanceMap = Map<Substance, int>


    /// TODO 20181029 Check.
    let orderPairs (a : list<ChiralAminoAcid>, b : list<ChiralAminoAcid>) =
        if a.Length < b.Length
        then (a, b)
        else 
            if a.Length > b.Length
            then (b, a)
            else
                if a <= b then (a, b)
                else (b, a)


    let inline getEnantiomer i = ((^T) : (member enantiomer : 'T) (i))


    let getTotalsValue (allInd : Map<Substance, int>) (allSubst : list<Substance>) (aminoAcids : list<AminoAcid>) (x : array<double>) =
        let g a =
            allSubst
            |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (x.[allInd.[s]] * (double i)) | None -> None)
            |> List.choose id
            |> List.sum

        aminoAcids |> List.map (fun a -> L a |> g, R a |> g)


    let getTotalSubstValue (allInd : Map<Substance, int>) (allSubst : list<Substance>)  (x : array<double>) =
        allSubst |> List.map (fun s -> x.[allInd.[s]] * (double s.atoms)) |> List.sum

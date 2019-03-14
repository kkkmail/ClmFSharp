namespace Clm

open ClmSys.GeneralData
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.Reactions
open Clm.ModelParams
open Clm.ReactionTypes

module CalculationData =

    type SedDirReagentInfo =
        {
            minSedDirChainLength : MaxPeptideLength
            maxSedDirChainLength : MaxPeptideLength
        }

        /// Default is that we want resolving agents affect all chains starting from some amino acid.
        static member defaultValue =
            {
                minSedDirChainLength = OneMax
                maxSedDirChainLength = ThreeMax
            }


    type SedDirAgentInfo =
        {
            minSedDirAgentLength : MaxPeptideLength
            maxSedDirAgentLength : MaxPeptideLength
        }

        /// Default is that we want resolving agents of length 3 only.
        static member defaultValue =
            {
                minSedDirAgentLength = ThreeMax
                maxSedDirAgentLength = ThreeMax
            }


    type SedDirInfo =
        {
            sedDirReagentInfo : SedDirReagentInfo
            sedDirAgentInfo : SedDirAgentInfo
        }

        static member defaultValue =
            {
                sedDirReagentInfo = SedDirReagentInfo.defaultValue
                sedDirAgentInfo = SedDirAgentInfo.defaultValue
            }


    type SubstInfoParam =
        {
            maxPeptideLength : MaxPeptideLength
            numberOfAminoAcids : NumberOfAminoAcids
            sedDirInfo : SedDirInfo
        }


    type SubstInfo =
        {
            infoParam : SubstInfoParam
            aminoAcids : list<AminoAcid>
            chiralAminoAcids : list<ChiralAminoAcid>
            peptides : list<Peptide>
            synthCatalysts : list<SynthCatalyst>
            destrCatalysts : list<DestrCatalyst>
            ligCatalysts : list<LigCatalyst>
            ligationPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
            racemCatalysts : list<RacemizationCatalyst>

            sedDirReagents : list<list<ChiralAminoAcid>>
            sedDirAgents : list<SedDirAgent>

            allChains : list<list<ChiralAminoAcid>>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allNamesMap : Map<Substance, string>
        }

        static member create (p : SubstInfoParam) =
            let peptides = Peptide.getPeptides p.maxPeptideLength p.numberOfAminoAcids
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids p.numberOfAminoAcids
            let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))
            let allLigChains = allChains |> List.filter(fun a -> a.Length < p.maxPeptideLength.length)

            let allSubst =
                    Substance.allSimple
                    @
                    (chiralAminoAcids |> List.map (fun a -> Chiral a))
                    @
                    (peptides |> List.map (fun p -> PeptideChain p))

            {
                infoParam = p
                aminoAcids = AminoAcid.getAminoAcids p.numberOfAminoAcids
                chiralAminoAcids = chiralAminoAcids
                peptides = peptides
                synthCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> SynthCatalyst p)
                destrCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> DestrCatalyst p)
                ligCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> LigCatalyst p)

                ligationPairs =
                    List.allPairs allLigChains allLigChains
                    |> List.filter (fun (a, b) -> a.Length + b.Length <= p.maxPeptideLength.length)
                    |> List.map (fun (a, b) -> orderPairs (a, b))
                    |> List.filter (fun (a, _) -> a.Head.isL)
                    |> List.distinct

                racemCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> RacemizationCatalyst p)

                sedDirReagents = allChains |> List.filter(fun a -> a.Length >= p.sedDirInfo.sedDirReagentInfo.minSedDirChainLength.length && a.Length <= p.sedDirInfo.sedDirReagentInfo.maxSedDirChainLength.length)
                sedDirAgents = allChains |> List.filter(fun a -> a.Length >= p.sedDirInfo.sedDirAgentInfo.minSedDirAgentLength.length && a.Length <= p.sedDirInfo.sedDirAgentInfo.maxSedDirAgentLength.length) |> List.map (fun e -> SedDirAgent e)

                allChains = allChains
                allSubst = allSubst
                allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList

                allNamesMap =
                    allSubst
                    |> List.map (fun s -> s, s.name)
                    |> Map.ofList
            }

        member si.synthesisReactions = si.chiralAminoAcids |> List.map SynthesisReaction
        member si.destructionReactions = si.chiralAminoAcids |> List.map DestructionReaction
        member si.ligationReactions = si.ligationPairs |> List.map LigationReaction
        member si.racemizationReactions = si.chiralAminoAcids |> List.map RacemizationReaction

        member si.catSynthInfo =
            {
                a = si.synthesisReactions |> Array.ofList
                b = si.synthCatalysts |> Array.ofList
                reactionName = ReactionName.CatalyticSynthesisName
            }

        member si.catDestrInfo =
            {
                a = si.destructionReactions |> Array.ofList
                b = si.destrCatalysts |> Array.ofList
                reactionName = ReactionName.CatalyticDestructionName
            }

        member si.catLigInfo =
            {
                a = si.ligationReactions |> Array.ofList
                b = si.ligCatalysts |> Array.ofList
                reactionName = ReactionName.CatalyticLigationName
            }

        member si.catRacemInfo =
            {
                a = si.racemizationReactions |> Array.ofList
                b = si.racemCatalysts |> Array.ofList
                reactionName = ReactionName.CatalyticRacemizationName
            }

        member si.sedDirInfo =
            {
                a = si.sedDirReagents |> Array.ofList
                b = si.sedDirAgents |> Array.ofList
                reactionName = ReactionName.SedimentationDirectName
            }


    type LevelZero = double
    type LevelOne = double * int
    type LevelTwo = double * int * int
    type LevelThree = double * int * int * int


    type SubstUpdateInfo =
        | NoSubst
        | OneSubst of Substance
        | TwoSubst of Substance * Substance
        | ThreeSubst of Substance * Substance * Substance

        static member create i =
            match i with
            | [] -> NoSubst
            | h1 :: t1 ->
                match t1 with
                | [] -> h1 |> OneSubst
                | h2 :: t2 ->
                    match t2 with
                    | [] -> (h1, h2) |> TwoSubst
                    | h3 :: t3 ->
                        match t3 with
                        | [] -> (h1, h2, h3) |> ThreeSubst
                        | _ -> failwith (sprintf "SubstUpdateInfo: invalid input: %A" i)


    type ModelIndices =
        {
            level0 : array<LevelZero>
            level1 : array<LevelOne>
            level2 : array<LevelTwo>
            level3 : array<LevelThree>
        }

        static member defaultValue =
            {
                level0 = [||]
                level1 = [||]
                level2 = [||]
                level3 = [||]
            }

        static member create (m : Map<Substance, int>) (i : list<double * SubstUpdateInfo>) =
            let l0 =
                i
                |> List.map (fun (v, e) -> match e with | NoSubst -> Some v | _ -> None)
                |> List.choose id

            let l1 =
                i
                |> List.map (fun (v, e) -> match e with | OneSubst s1 -> Some (v, m.[s1]) | _ -> None)
                |> List.choose id

            let l2 =
                i
                |> List.map (fun (v, e) -> match e with | TwoSubst (s1, s2) -> Some (v, m.[s1], m.[s2]) | _ -> None)
                |> List.choose id

            let l3 =
                i
                |> List.map (fun (v, e) -> match e with | ThreeSubst (s1, s2, s3) -> Some (v, m.[s1], m.[s2], m.[s3]) | _ -> None)
                |> List.choose id

            {
                level0 = l0 |> Array.ofList
                level1 = l1 |> Array.ofList
                level2 = l2 |> Array.ofList
                level3 = l3 |> Array.ofList
            }


    let calculateTotalSubst (totalSubst : array<LevelOne>) (x: double[]) =
        let mutable sum = 0.0

        for (coeff, j1) in totalSubst do
            sum <- sum + coeff * x.[j1]

        sum


    let calculateTotals (totals : array<array<LevelOne> * array<LevelOne>>) (x: double[]) =
        totals
        |> Array.map (fun (l, r) -> calculateTotalSubst l x, calculateTotalSubst r x)


    let calculateDerivativeValue (indicies : ModelIndices) (x: double[]) =
        let mutable sum = 0.0

        for coeff in indicies.level0 do
            sum <- sum + coeff

        for (coeff, j1) in indicies.level1 do
            sum <- sum + coeff * x.[j1]

        for (coeff, j1, j2) in indicies.level2 do
            sum <- sum + coeff * x.[j1] * x.[j2]

        for (coeff, j1, j2, j3) in indicies.level3 do
            sum <- sum + coeff * x.[j1] * x.[j2] * x.[j3]

        sum


    type ModelCalculationData =
        {
            totalSubst : array<LevelOne>
            totals : array<array<LevelOne> * array<LevelOne>>
            derivative : array<ModelIndices>
        }

        static member defaultValue =
            {
                totalSubst = [||]
                totals = [||]
                derivative = [||]
            }

        member md.getDerivative x = md.derivative |> Array.map (fun i -> calculateDerivativeValue i x)
        member md.getTotalSubst x = calculateTotalSubst md.totalSubst x
        member md.getTotals x = calculateTotals md.totals x

        static member createTotalSubst (si : SubstInfo) =
            si.allSubst
            |> List.map (fun s -> (double s.atoms, si.allInd.[s] ))
            |> Array.ofList

        static member createTotals (si : SubstInfo) : array<array<LevelOne> * array<LevelOne>> =
            let g a =
                si.allSubst
                |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (double i, si.allInd.[s]) | None -> None)
                |> List.choose id
                |> Array.ofList

            let y =
                si.aminoAcids
                |> List.map (fun a -> L a |> g, R a |> g)
                |> Array.ofList

            y

        static member createDerivative (si : SubstInfo) (allReac : list<AnyReaction>) =
            let normalized = allReac |> List.map (fun e -> e.reaction.info.normalized(), e.forwardRate, e.backwardRate)

            let processReaction i o (ReactionRate v) =
                let r = i |> SubstUpdateInfo.create

                (i |> List.map(fun e -> e, -1))
                @
                (o |> List.map(fun e -> e, 1))
                |> List.groupBy (fun (s, _) -> s)
                |> List.map (fun (s, e) -> s, e |> List.map (fun (_, i) -> i) |> List.sum)
                |> List.filter (fun (_, e) -> e <> 0)
                |> List.map (fun (s, m) -> s, ((double m) * v, r))

            let getRates chooser =
                normalized
                |> List.map (fun (r, f, b) -> r, chooser (f, b))
                |> List.choose (fun (e, r) -> r |> Option.bind (fun v -> Some (e, v)))

            let allMap =
                (getRates fst |> List.map (fun (r, v) -> processReaction r.inputNormalized r.outputNormalized v))
                @
                (getRates snd |> List.map (fun (r, v) -> processReaction r.outputNormalized r.inputNormalized v))
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> List.map (fun (s, l) -> si.allInd.[s], l |> List.map (fun (_, e) -> e) |> ModelIndices.create si.allInd)
                |> Map.ofList

            [| for i in 0..(si.allSubst.Length - 1) -> allMap.TryFind i |]
            |> Array.map (fun e -> match e with | Some v -> v | None -> ModelIndices.defaultValue)

        static member create (si : SubstInfo) (allReac : list<AnyReaction>) =
            {
                totalSubst = ModelCalculationData.createTotalSubst si
                totals = ModelCalculationData.createTotals si
                derivative = ModelCalculationData.createDerivative si allReac
            }


    type ModelBinaryData =
        {
            calculationData : ModelCalculationData
            allRawReactions : list<ReactionName * int64>
            allReactions : list<ReactionName * int64>
        }


    type ModelAllData =
        {
            modelDataParams : ModelDataParams
            modelBinaryData : ModelBinaryData
        }

        member this.getModelDataParamsWithExtraData() : ModelDataParamsWithExtraData =
            let numberOfAminoAcids = this.modelDataParams.modelInfo.numberOfAminoAcids
            let maxPeptideLength = this.modelDataParams.modelInfo.maxPeptideLength
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
            let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids

            let allSubst =
                Substance.allSimple
                @
                (chiralAminoAcids |> List.map (fun a -> Chiral a))
                @
                (peptides |> List.map (fun p -> PeptideChain p))

            let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList

            {
                regularParams =
                    {
                        modelDataParams = this.modelDataParams
                        allSubstData =
                            {
                                allSubst = allSubst
                                allInd = allInd
                                allRawReactions = this.modelBinaryData.allRawReactions
                                allReactions = this.modelBinaryData.allReactions
                            }
                    }

                funcParams =
                    {
                        getTotals = this.modelBinaryData.calculationData.getTotals
                        getTotalSubst = this.modelBinaryData.calculationData.getTotalSubst
                        getDerivative = this.modelBinaryData.calculationData.getDerivative
                    }
            }


    type ModelData =
        {
            modelDataId : ModelDataId
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int option
            fileStructureVersion : string
            modelData : ModelAllData
            defaultSetIndex : int
        }

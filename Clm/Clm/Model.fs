namespace Clm

open System
open FSharp.Collections
open Clm.VersionInfo
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.DataLocation

module Model = 
    open System.IO

    type ModelInfo = 
        {
            fileStructureVersionNumber : string
            versionNumber : string
            seedValue : int
            modelName : string
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
        }


    type ModelInfoWithModels = 
        {
            modelInfo : ModelInfo
            allModels : list<ReactionRateModel>
        }


    type ModelDataParams = 
        {
            modelInfo : ModelInfo
            allParams : list<ReactionRateModelParam>
        }


    type ModelDataParamsWithExtraData = 
        {
            modelDataParams : ModelDataParams
            getTotals : array<double> -> array<double * double>
            getTotalSubst : array<double> -> double
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int>
            allReactions : list<ReactionName * int>
        }


    type UpdateFuncType = 
        | UseArray
        | UseVariables
        | UseFunctions


    type ModelGenerationParams = 
        {
            fileStructureVersionNumber : string
            versionNumber : string
            seedValue : int option
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRateModels : List<ReactionRateModel>
            updateFuncType : UpdateFuncType
            modelLocationData : ModelLocationInputData
            updateAllModels : bool
        }


    type ModelInitValuesParams = 
        {
            modelDataParams : ModelDataParamsWithExtraData
            distr : Distribution
            multiplier : double option
            multEe : double option
        }

        static member defaultMult = 0.001
        static member defaultMultEe = 0.001

        static member getDefaultValue p so = 
            {
                modelDataParams = p
                distr = 
                    let seed = 
                        match so with 
                        | Some s -> s
                        | None -> 0

                    UniformDistribution seed |> Uniform
                multiplier = None
                multEe = None
            }


    let defaultInit (p : ModelInitValuesParams) y0 = 
        let mult = 
            match p.multiplier with 
            | Some m -> m
            | None -> ModelInitValuesParams.defaultMult

        let allIndRev = 
            p.modelDataParams.allInd
            |> Map.toList
            |> List.map (fun (s, i) -> (i, s))
            |> Map.ofList

        let multEe = 
            match p.multEe with 
            | Some m -> m
            | None -> ModelInitValuesParams.defaultMultEe

        let allSubst = p.modelDataParams.allSubst

        let nextValue (s : Substance) = 
            let n = p.modelDataParams.modelDataParams.modelInfo.numberOfAminoAcids.length
            let noOfSubstOnLevel = pown (2 * n) s.atoms
            //y0 * mult *p.distr.nextDouble() / (double p.modelDataParams.modelDataParams.modelInfo.numberOfSubstances)
            y0 * mult *p.distr.nextDouble() / (double noOfSubstOnLevel)

        let nextEe() = multEe * (2.0 * p.distr.nextDoubleFromZeroToOne() - 1.0)

        let initVals =
            allSubst
            |> List.filter (fun s -> s.isFood |> not)
            |> List.map (fun s -> orderPairs (s.aminoAcids, s.enantiomer.aminoAcids) |> fst |> Substance.fromList)
            |> List.distinct
            |> List.map (fun s -> (s, (nextValue s, nextEe())))

        let initValsMap = initVals |> Map.ofList
        let total = initVals |> List.map (fun (s, (v, _)) -> v * (double s.atoms)) |> List.sum

        let getValue i = 
            let s = allIndRev.[i]
            match s.isFood with 
            | true -> y0 - 2.0 * total
            | false ->
                let len = s.atoms
                match initValsMap.TryFind s, initValsMap.TryFind s.enantiomer with 
                | Some _, Some _ -> 
                    printfn "Duplicate init value for i: %A, substance: %A." i s.name
                    0.0
                | Some (v, e), None -> v * (1.0 + e)
                | None, Some (v, e) -> v * (1.0 - e)
                | None, None -> 
                    printfn "Cannot find init value for i: %A, substance: %A." i s.name
                    0.0

        [| for i in 0..(p.modelDataParams.modelDataParams.modelInfo.numberOfSubstances - 1) -> getValue i |]


    type ClmModel (modelParams : ModelGenerationParams) = 

        /// As of 20181122 F# / FSI still have a problem with a new line.
        let nl = "\r\n"

        let reactionShift = 
            match modelParams.updateFuncType with
            | UseArray -> "    "
            | UseVariables -> "    "
            | UseFunctions -> ""

        let seedValue = 
            match modelParams.seedValue with 
            | Some s -> s
            | None -> 
                let r = new Random()
                r.Next()

        let modelLocationInfo = createModelLocationInfo modelParams.modelLocationData

        let rateProviderParams = { rateModels = modelParams.reactionRateModels }
        let rateProvider = ReactionRateProvider rateProviderParams
        let allParamsCode shift = rateProvider.toParamFSharpCode shift

        let aminoAcids = AminoAcid.getAminoAcids modelParams.numberOfAminoAcids
        let chiralAminoAcids = ChiralAminoAcid.getAminoAcids modelParams.numberOfAminoAcids
        let peptides = Peptide.getPeptides modelParams.maxPeptideLength modelParams.numberOfAminoAcids
        let synthCatalysts = peptides |> List.map (fun p -> SynthCatalyst p)
        let ligCatalysts = peptides |> List.map (fun p -> LigCatalyst p)

        let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))

        let allPairs =
            List.allPairs allChains allChains
            |> List.map (fun (a, b) -> orderPairs (a, b))
            |> List.filter (fun (a, _) -> a.Head.isL)
            |> List.distinct

        let ligationPairs = allPairs |> List.filter (fun (a, b) -> a.Length + b.Length <= modelParams.maxPeptideLength.length)

        //do
        //    ligationPairs
        //    |> List.map (fun (a, b) -> printfn "a: %A, b: %A" a b)
        //    |> ignore

        let catSynthPairs = List.allPairs (chiralAminoAcids |> List.map (fun c -> SynthesisReaction c)) synthCatalysts
        let catLigPairs = List.allPairs (ligationPairs |> List.map (fun c -> LigationReaction c)) ligCatalysts

        let noOfRawReactions n = 
            match n with 
            | SynthesisName -> chiralAminoAcids.Length
            | CatalyticSynthesisName -> catSynthPairs.Length
            | LigationName -> ligationPairs.Length
            | CatalyticLigationName -> catLigPairs.Length
            | SedimentationDirectName -> allPairs.Length
            | SedimentationAllName -> chiralAminoAcids.Length


        let allSubst = 
            [ Substance.food ]
            @
            (chiralAminoAcids |> List.map (fun a -> Chiral a))
            @
            (peptides |> List.map (fun p -> PeptideChain p))

        let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList

        let generateSubst() = 
            @"
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
"

        let allNamesMap = 
            allSubst
            |> List.map (fun s -> s, s.name)
            |> Map.ofList

        let createReactions c l = 
            let create a = c a |> AnyReaction.tryCreateReaction rateProvider

            l
            |> List.map create
            |> List.choose id
            |> List.concat


        let synth = createReactions (fun a -> SynthesisReaction a |> Synthesis) chiralAminoAcids
        let lig = createReactions (fun x -> LigationReaction x |> Ligation) ligationPairs

        //do
        //    lig
        //    |> List.map (fun r -> printfn "r: %A" r)
        //    |> ignore

        let sedDir = createReactions (fun x -> SedimentationDirectReaction x |> SedimentationDirect) allPairs
        let catSynth = createReactions (fun x -> CatalyticSynthesisReaction x |> CatalyticSynthesis) catSynthPairs
        let catLig = createReactions (fun x -> CatalyticLigationReaction x |> CatalyticLigation) catLigPairs

        let allReac = 
            synth @ catSynth @ lig @ catLig @ sedDir
            |> List.distinct

        let kW = 
            SedimentationAllReaction |> SedimentationAll
            |> rateProvider.getRates
            |> fst

        let allRawReactionsData = 
            ReactionName.all
            |> List.map (fun n -> n, noOfRawReactions n)
            |> List.map (fun (n, c) -> "                    " + "(" + n.ToString() + ", " + c.ToString() + ")")
            |> String.concat nl

        let allReactionsData = 
            let shift = "                    "

            (
                allReac
                |> List.groupBy (fun r -> r.name)
                |> List.map (fun (n, l) -> (n, l.Length))
                |> List.map (fun (n, c) -> shift + "(" + n.ToString() + ", " + c.ToString() + ")")
            )
            @
            (
                // TODO kk:20181130 A little hack. Do it properly.
                match kW with 
                | Some _ -> [ shift + "(" + ReactionName.SedimentationAllName.ToString() + ", " + (2 * modelParams.numberOfAminoAcids.length).ToString() + ")" ]
                | None -> []
            )
            |> String.concat nl


        let allReacMap = 
            allReac
            |> List.map (fun e -> e, e.fullName)
            |> Map.ofList


        let substToString s = allNamesMap.[s]
        let reactToString r = allReacMap.[r]
        let lstToString (l : list<Substance * int>) = 
            l
            |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + (substToString s))
            |> String.concat " + "

        let xName = "x"
        let xSumName = "xSum"
        let xSumNameN = "xSumN"
        let xSumSquaredNameN = "xSumSquaredN"
        let dName = "d"

        let coeffSedAllName = "kW"

        let substComment (s : Substance) shift = shift + "    // " + (allInd.[s]).ToString() + " - " + (substToString s) + nl
        //let reactionComment (r : Reaction) = " // " + (reactToString r) + nl
        let x (s : Substance) = xName + ".[" + (allInd.[s]).ToString() + "]"
        let d (s : Substance) = dName + "" + (allInd.[s]).ToString()

        let rate (l : list<Substance * int>) (ReactionRate r) = 
            let toPown s n = 
                match n with 
                | 1 -> x s
                | _ -> "(pown " + (x s) + " " + n.ToString() + ")"

            let a = l |> List.fold(fun acc (s, n) -> acc + (if acc <> "" then " * " else "") + (toPown s n)) ""
            (r.ToString() |> toFloat) + " * " + a + " // " + (lstToString l) // + nl

        let toMult i = 
            match i with 
            | 1 -> String.Empty
            | _ -> i.ToString() + ".0 * "

        let processReaction (r : AnyReaction) : list<Substance * string> =
            let update i o r f rc : list<Substance * string> = 
                let catalysts = 
                    (o |> List.map (fun (s, n) -> s, -n)) @ i
                    |> List.groupBy (fun (s, _) -> s)
                    |> List.map (fun (s, e) -> (s, e |> List.fold (fun acc (_, n) -> acc + n) 0))
                    |> List.filter (fun (_, n) -> n = 0)
                    |> List.map (fun (s, _) -> s)
                    |> Set.ofList

                let totalShift = reactionShift + "            "
                let (iSign, oSign) = if f then "-", "" else "", "-"
                let fwd = rate i r
                (
                    i
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (totalShift + iSign + (toMult n) + fwd + " | " + rc + nl)))
                )
                @
                (
                    o
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (totalShift + oSign + (toMult n) + fwd+ " | " + rc + nl)))
                )

            let rc = reactToString r

            match r with
            | Forward f -> update f.reaction.info.input f.reaction.info.output f.forwardRate true rc
            | Backward b -> update b.reaction.info.input b.reaction.info.output b.backwardRate false rc
            | Reversible rv ->
                (update rv.reaction.info.input rv.reaction.info.output rv.forwardRate true rc)
                @
                (update rv.reaction.info.output rv.reaction.info.input rv.backwardRate true rc)

        let generateTotals () = 
            let g a =
                allSubst
                |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (s, i) | None -> None)
                |> List.choose id
                |> List.map (fun (s, i) -> "                    " + (toMult i) + (x s) + " // " + (substToString s))

            let gg (v : list<string>) = 
                let a = v |> String.concat nl
                "                [|" + nl + a + nl + "                |]" + nl + "                |> Array.sum" + nl

            let gg1 ((a : AminoAcid), l, r) = 
                "            // " + a.name + nl + "            (" + nl + (gg l) + "                ," + nl + (gg r) + "            )" + nl

            let y =
                aminoAcids
                |> List.map (fun a -> a, L a |> g, R a |> g)
                |> List.map (fun (a, l, r) -> gg1 (a, l, r))

            let x =
                y
                |> String.concat nl

            "    let getTotals (x : array<double>) = " + nl +
            "        [|" + nl +
            x +
            "        |]" + nl

        let generateTotalSubst() = 
            let x =
                allSubst
                |> List.map (fun s -> s, s.atoms)
                |> List.map (fun (s, i) -> "            " + (toMult i) + (x s) + " // " + (substToString s))
                |> String.concat nl

            "    let getTotalSubst (x : array<double>) = " + nl +
            "        [|" + nl +
            x +
            nl + "        |]" + nl + "        |> Array.sum" + nl


        let generate () = 
            let t0 = DateTime.Now
            printfn "t0 = %A" t0

            let r0 = 
                allReac 
                |> List.map (fun r -> processReaction r)

            printfn "r0.Length = %A" r0.Length
            let t11 = DateTime.Now
            printfn "t11 = %A" t11
            printfn "t11 - t0 = %A" (t11 - t0).TotalSeconds

            let reactions = 
                r0
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> Map.ofList

            let t1 = DateTime.Now
            printfn "t1 = %A" t1
            printfn "t1 - t11 = %A" (t1 - t11).TotalSeconds

            let getReaction s = 
                match reactions.TryFind s with 
                //| Some r -> r |> List.rev |> List.fold (fun acc (s, e) -> acc + e) ""
                | Some r -> r |> List.rev |> List.map (fun (_, e) -> e) |> String.concat ""
                | None -> String.Empty

            let getTotalSedReac (s : Substance) shift = 
                match kW with
                | Some (ReactionRate _) -> 
                    match s with 
                    | Food _ -> shift + "            " + coeffSedAllName + " * (2.0 * " + xSumName + " * " + xSumNameN + " - " + xSumSquaredNameN + ")"
                    | _ -> shift + "            " + "-" + coeffSedAllName + " * (2.0 * " + xSumName + " - " + (x s) + ") * " + (x s)
                | None -> String.Empty

            let coeffSedAllCode = 
                match kW with
                | Some (ReactionRate k) -> 
                    "    let " + coeffSedAllName + " = " + k.ToString() + " / " + (allSubst.Length - 1).ToString() + ".0" + nl
                | None -> String.Empty

            let a = 
                allSubst
                |> List.map (fun s -> nl + "    " + (substComment s "    ") +  "            [|" + nl + (getTotalSedReac s "    ") + nl + (getReaction s) + "            |]" + nl + "            |> Array.sum" + nl)

            let dInitCode xPar = 
                let shift, g = 
                    match xPar with 
                    | "" -> "    ", d 
                    | _ -> 
                        let d1 s = 
                            (d s) + " (" + xName + " : array<double>) " + xSumName + " " + xSumNameN + " " + xSumSquaredNameN
                        String.Empty, d1

                allSubst
                |> List.map (fun s -> nl + (substComment s shift) + shift + "    let " + (g s) + " = " + nl + shift + "        [|" + nl + (getTotalSedReac s shift) + nl + (getReaction s) + shift + "        |]" + nl + shift + "        |> Array.sum" + nl)

            let dArrayCode xPar = 
                let shift, g = 
                    match xPar with 
                    | "" -> "    ", d
                    | _ -> 
                        let d1 s = (d s) + " " + xPar + " " + xSumName + " " + xSumNameN + " " + xSumSquaredNameN
                        "", d1

                allSubst |> List.map (fun s -> shift + "            " + (g s)) |> String.concat nl


            let t2 = DateTime.Now
            printfn "t2 = %A" t2
            printfn "t2 - t1 = %A" (t2 - t1).TotalSeconds

            let totalCode = generateTotals()
            let totalSubstCode = generateTotalSubst()

            let sc = 
                allSubst
                |> List.filter (fun s -> not s.isFood)
                |> List.map (fun s -> "                " + (s.atoms.ToString()) + ".0 * " + (x s) + " // " + (substToString s))
                |> String.concat nl

            let sc2 = 
                allSubst
                |> List.filter (fun s -> not s.isFood)
                |> List.map (fun s -> "                " + (s.atoms.ToString()) + ".0 * " + (x s) + " * " + (x s) + " // " + (substToString s))
                |> String.concat nl

            let sumCode = "        let " + xSumName + " = (" + xName + " |> Array.sum) - " + xName + ".[0]" + nl + nl
            let sumCodeN = "        let " + xSumNameN + " = " + nl + "            [|" + nl + sc + nl + "            |]" + nl + "            |> Array.sum" + nl + nl
            let sumSquaredCodeN = "        let " + xSumSquaredNameN + " = " + nl + "            [|" + nl + sc2 + nl + "            |]" + nl + "            |> Array.sum" + nl

            let modelDataParamsCode = 
                @"
    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = """ + modelParams.fileStructureVersionNumber + @"""
                            versionNumber = """ + modelParams.versionNumber + @"""
                            seedValue = seedValue
                            modelName = """ + modelLocationInfo.modelName + @"""
                            numberOfSubstances = " + allSubst.Length.ToString() + @"
                            numberOfAminoAcids = " + modelParams.numberOfAminoAcids.ToString() + @"
                            maxPeptideLength = " + modelParams.maxPeptideLength.ToString() + @"
                        }

                    allParams = 
                        [
" 
                        + 
                        (allParamsCode "                ") + @"
                        ]
                }

            getTotals = getTotals
            getTotalSubst = getTotalSubst
            allSubst = allSubst
            allInd = allInd

            allRawReactions = 
                [" + 
                nl + allRawReactionsData + @"
                ]

            allReactions = 
                [" + 
                nl + allReactionsData + @"
                ]
        }
"

            let updateOuterCode = 
                match modelParams.updateFuncType with 
                | UseArray -> []
                | UseVariables -> []
                | UseFunctions -> 
                    dInitCode xName


            let updateInnerCode = 
                match modelParams.updateFuncType with 
                | UseArray ->
                    [ "        [|" ] 
                    @ 
                    a 
                    @ 
                    [ "        |]" + nl ]
                | UseVariables ->
                    dInitCode String.Empty
                    @
                    [
                        "        // printfn \"update::Assembling d...\"" + nl
                        "        let d = "
                        "            [|" 
                        dArrayCode String.Empty
                        "            |]" + nl
                        "        // printfn \"update::Completed.\"" + nl
                        "        d" + nl
                    ]
                | UseFunctions -> 
                    [ "        [|" ] 
                    @
                    [ dArrayCode xName ]
                    @
                    [ "        |]" + nl ]

            let updateCode = 
                updateOuterCode
                @
                [ 
                    "    let update (x : array<double>) : array<double> = " + nl
                    "        // printfn \"update::Starting...\"" + nl
                    sumCode
                    sumCodeN
                    sumSquaredCodeN
                ]
                @
                updateInnerCode

            let paramCode = 
                "    let seedValue = " + seedValue.ToString() + nl + 
                "    let numberOfAminoAcids = NumberOfAminoAcids." + (modelParams.numberOfAminoAcids.ToString()) + nl + 
                "    let maxPeptideLength = MaxPeptideLength." + (modelParams.maxPeptideLength.ToString()) + nl +
                "    let numberOfSubstances = " + (allSubst.Length).ToString() + nl +
                generateSubst() + 
                coeffSedAllCode

            [
                "namespace Model" + nl
                "open Clm.Substances"
                "open Clm.Model"
                "open Clm.ReactionTypes"
                "open Clm.ReactionRates" + nl
                "module ModelData = "
                paramCode + nl
                totalSubstCode + nl
                totalCode + nl
            ]
            @ updateCode
            @ [ modelDataParamsCode ]


        let allModelDataImpl = @"
        @
        [
            {
                modelInfo = 
                    {
                        fileStructureVersionNumber = """ + modelParams.fileStructureVersionNumber + @"""
                        versionNumber = """ + modelParams.versionNumber + @"""
                        seedValue = " + seedValue.ToString() + @"
                        modelName = """ + modelLocationInfo.modelName + @"""
                        numberOfSubstances = " + (allSubst.Length).ToString() + @"
                        numberOfAminoAcids = NumberOfAminoAcids." + (modelParams.numberOfAminoAcids.ToString()) + @"
                        maxPeptideLength = MaxPeptideLength." + (modelParams.maxPeptideLength.ToString()) + @"
                    }

                allParams = 
                    [
"
                                + (allParamsCode "            ") + @"
                    ]
            }
        ]"

        let generateAndSave() = 
            printfn "Generating..."
            let s = generate()

            printfn "Writing..."
            File.WriteAllLines(modelLocationInfo.outputFile, s)
            printfn "Done."

            match modelParams.updateAllModels with
            | true -> 
                printfn "Updating %A..." modelParams.modelLocationData.allModelsFile
                File.AppendAllLines(modelParams.modelLocationData.allModelsFile, [ allModelDataImpl ])
            | false -> printfn "NOT updating %A." modelParams.modelLocationData.allModelsFile

            printfn "Done."

        member model.allSubstances = allSubst
        member model.synthesis = synth
        member model.catalyticSynthesis = catSynth
        member model.ligation = lig
        member model.catalyticLigation = catLig
        member model.sedimentationDirect = sedDir
        member model.allReactions = allReac
        member model.allModelData = allModelDataImpl
        member model.locationInfo = modelLocationInfo
        member model.generateCode() = generateAndSave()

namespace Clm.Generator

open System
open System.IO
open FSharp.Collections

open ClmSys.VersionInfo
open Clm.Distributions
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.ModelParams
open Clm.DataLocation
open ClmSys.GeneralData
open Clm.Generator.FSharpCodeExt
open Clm.Generator.ReactionRatesExt
open ClmDefaults.DefaultValuesExt

module ClmModelData =

    let newSeed() = (new Random()).Next()


    [<Literal>]
    let UpdateFuncTypeName = "UpdateFuncType"

    [<Literal>]
    let UseArrayName = "UseArray"

    [<Literal>]
    let UseVariablesName = "UseVariables"

    [<Literal>]
    let UseFunctionsName = "UseFunctions"

    type UpdateFuncType = 
        | UseArray
        | UseVariables
        | UseFunctions


    [<Literal>]
    let ModelGenerationParamsName = "ModelGenerationParams"

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
            defaultSetIndex : int
        }


    type AllParams =
        {
            modelGenerationParams : ModelGenerationParams
            modelCommandLineParams : list<ModelCommandLineParam>
        }

        static member getDefaultValue rnd (d : ClmDefaultValue) numberOfAminoAcids maxPeptideLength i =
            let rates = d.getDefaultRateModels rnd numberOfAminoAcids

            {
                modelGenerationParams = 
                    {
                        fileStructureVersionNumber = FileStructureVersionNumber
                        versionNumber = VersionNumber
                        seedValue = rnd.Next() |> Some
                        numberOfAminoAcids = numberOfAminoAcids
                        maxPeptideLength = maxPeptideLength
                        reactionRateModels = rates.rateModels
                        updateFuncType = UseFunctions
                        modelLocationData = ModelLocationInputData.defaultValue
                        updateAllModels = false
                        defaultSetIndex = i
                    }
                modelCommandLineParams = d.modelCommandLineParams
            }

    let reactionShift updateFuncType =
        match updateFuncType with
        | UseArray -> "    "
        | UseVariables -> "    "
        | UseFunctions -> ""


    let getSeedValue sv =
        match sv with
        | Some s -> s
        | None -> 
            let r = new Random()
            r.Next()


    type SubstInfo =
        {
            maxPeptideLength : MaxPeptideLength
            numberOfAminoAcids : NumberOfAminoAcids
            aminoAcids : list<AminoAcid>
            chiralAminoAcids : list<ChiralAminoAcid>
            peptides : list<Peptide>
            synthCatalysts : list<SynthCatalyst>
            destrCatalysts : list<DestrCatalyst>
            ligCatalysts : list<LigCatalyst>
            ligationPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
            racemCatalysts : list<RacemizationCatalyst>
            allChains : list<list<ChiralAminoAcid>>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allNamesMap : Map<Substance, string>
        }

        static member create maxPeptideLength numberOfAminoAcids =
            let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
            let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))
            let allLigChains = allChains |> List.filter(fun a -> a.Length < maxPeptideLength.length)

            let allSubst =
                    Substance.allSimple
                    @
                    (chiralAminoAcids |> List.map (fun a -> Chiral a))
                    @
                    (peptides |> List.map (fun p -> PeptideChain p))

            {
                maxPeptideLength = maxPeptideLength
                numberOfAminoAcids = numberOfAminoAcids
                aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
                chiralAminoAcids = chiralAminoAcids
                peptides = peptides
                synthCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> SynthCatalyst p)
                destrCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> DestrCatalyst p)
                ligCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> LigCatalyst p)

                ligationPairs =
                    List.allPairs allLigChains allLigChains
                    |> List.filter (fun (a, b) -> a.Length + b.Length <= maxPeptideLength.length)
                    |> List.map (fun (a, b) -> orderPairs (a, b))
                    |> List.filter (fun (a, _) -> a.Head.isL)
                    |> List.distinct

                racemCatalysts = peptides |> List.filter (fun p -> p.length > 2) |> List.map (fun p -> RacemizationCatalyst p)
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


    let generateSubst() =
            @"
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
"

    let getAminoAcidsCode (modelParams : ModelGenerationParams) = "AminoAcid.getAminoAcids NumberOfAminoAcids." + modelParams.numberOfAminoAcids.ToString()


    type RateGenerationCommonData =
        {
            substInfo : SubstInfo
            catSynthPairs : list<SynthesisReaction * SynthCatalyst>
            catDestrPairs : list<DestructionReaction * DestrCatalyst>
            catLigPairs : list<LigationReaction * LigCatalyst>
            catRacemPairs : list<RacemizationReaction * RacemizationCatalyst>
        }

        member data.getReactions sdp rateProvider t n =
            let x = AnyReaction.tryCreateReactionFromRateData

            let createReactions c l =
                let create a = c a |> AnyReaction.tryCreateReaction rateProvider t

                l
                |> List.map create
                |> List.choose id
                |> List.concat

            match n with
            | FoodCreationName -> [ AnyReaction.tryCreateReaction rateProvider t (FoodCreationReaction |> FoodCreation) ] |> List.choose id |> List.concat
            | WasteRemovalName -> [ AnyReaction.tryCreateReaction rateProvider t (WasteRemovalReaction |> WasteRemoval) ] |> List.choose id |> List.concat
            | WasteRecyclingName -> [ AnyReaction.tryCreateReaction rateProvider t (WasteRecyclingReaction |> WasteRecycling) ] |> List.choose id |> List.concat
            | SynthesisName -> createReactions (fun a -> SynthesisReaction a |> Synthesis) data.substInfo.chiralAminoAcids
            | DestructionName -> createReactions (fun a -> DestructionReaction a |> Destruction) data.substInfo.chiralAminoAcids
            | CatalyticSynthesisName -> createReactions (fun x -> CatalyticSynthesisReaction x |> CatalyticSynthesis) data.catSynthPairs
            | CatalyticDestructionName -> createReactions (fun x -> CatalyticDestructionReaction x |> CatalyticDestruction) data.catDestrPairs
            | LigationName -> createReactions (fun x -> LigationReaction x |> Ligation) data.substInfo.ligationPairs
            | CatalyticLigationName -> createReactions (fun x -> CatalyticLigationReaction x |> CatalyticLigation) data.catLigPairs
            | SedimentationDirectName -> createReactions (fun x -> SedimentationDirectReaction x |> SedimentationDirect) sdp
            | SedimentationAllName -> []
            | RacemizationName -> createReactions (fun a -> RacemizationReaction a |> Racemization) data.substInfo.chiralAminoAcids
            | CatalyticRacemizationName -> createReactions (fun x -> CatalyticRacemizationReaction x |> CatalyticRacemization) data.catRacemPairs


    type BruteForceModelData =
        {
            commonData : RateGenerationCommonData
            allPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
        }

        member data.noOfRawReactions n =
            match n with
            | FoodCreationName -> 1
            | WasteRemovalName -> 1
            | WasteRecyclingName -> 1
            | SynthesisName -> data.commonData.substInfo.chiralAminoAcids.Length
            | DestructionName -> data.commonData.substInfo.chiralAminoAcids.Length
            | CatalyticSynthesisName -> data.commonData.catSynthPairs.Length
            | CatalyticDestructionName -> data.commonData.catDestrPairs.Length
            | LigationName -> data.commonData.substInfo.ligationPairs.Length
            | CatalyticLigationName -> data.commonData.catLigPairs.Length
            | SedimentationDirectName -> data.allPairs.Length
            | SedimentationAllName -> data.commonData.substInfo.chiralAminoAcids.Length
            | RacemizationName -> data.commonData.substInfo.chiralAminoAcids.Length
            | CatalyticRacemizationName -> data.commonData.catRacemPairs.Length


        member data.getReactions rateProvider n =
            data.commonData.getReactions data.allPairs rateProvider BruteForce n


        static member create si =
            let allPairs =
                List.allPairs si.allChains si.allChains
                |> List.map (fun (a, b) -> orderPairs (a, b))
                |> List.filter (fun (a, _) -> a.Head.isL)
                |> List.distinct

            {
                allPairs = allPairs
                commonData =
                    {
                        substInfo = si
                        catSynthPairs = List.allPairs si.synthesisReactions si.synthCatalysts
                        catDestrPairs = List.allPairs si.destructionReactions si.destrCatalysts
                        catLigPairs = List.allPairs si.ligationReactions si.ligCatalysts
                        catRacemPairs = List.allPairs si.racemizationReactions si.racemCatalysts
                    }
            }

    //let getModelDataParamsCode (modelParams : ModelGenerationParams)


    let generatePairs<'A, 'B> (i : RateGeneratorInfo<'A, 'B>) (rateProvider : ReactionRateProvider) =
        let noOfTries = i.a.Length * i.b.Length
        printfn "generatePairs.noOfTries = %A" noOfTries

        match rateProvider.getPrimaryDistribution i.reactionName with
        | Some d ->
            let sn = d.successNumber noOfTries
            printfn "generatePairs.sn = %A" sn
            [ for _ in 1..sn -> (i.a.[d.next i.a.Length], i.b.[d.next i.b.Length]) ]
        | None -> []


    type RandomChoiceModelData =
        {
            commonData : RateGenerationCommonData
            sedDirPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
        }

        member data.noOfRawReactions n =
            let si = data.commonData.substInfo

            match n with
            | FoodCreationName -> 1
            | WasteRemovalName -> 1
            | WasteRecyclingName -> 1
            | SynthesisName -> si.chiralAminoAcids.Length
            | DestructionName -> si.chiralAminoAcids.Length
            | CatalyticSynthesisName -> si.synthesisReactions.Length * si.synthCatalysts.Length
            | CatalyticDestructionName -> si.destructionReactions.Length * si.destrCatalysts.Length
            | LigationName -> si.ligationPairs.Length
            | CatalyticLigationName -> si.ligationReactions.Length * si.ligCatalysts.Length
            | SedimentationDirectName -> si.allChains.Length * si.allChains.Length
            | SedimentationAllName -> si.chiralAminoAcids.Length
            | RacemizationName -> si.chiralAminoAcids.Length
            | CatalyticRacemizationName -> si.racemizationReactions.Length * si.racemCatalysts.Length

        member data.getReactions rateProvider n =
            data.commonData.getReactions data.sedDirPairs rateProvider RandomChoice n

        static member create rateProvider si =
            {
                sedDirPairs = []

                commonData =
                    {
                        substInfo = si
                        catSynthPairs = generatePairs si.catSynthInfo rateProvider
                        catDestrPairs = generatePairs si.catDestrInfo rateProvider
                        catLigPairs = generatePairs si.catLigInfo rateProvider
                        catRacemPairs = generatePairs si.catRacemInfo rateProvider
                    }
            }


    type RateGenerationData =
        | BruteForceModel of BruteForceModelData
        | RandomChoiceModel of RandomChoiceModelData

        member data.noOfRawReactions n =
            match data with
            | BruteForceModel m -> m.noOfRawReactions n
            | RandomChoiceModel m -> m.noOfRawReactions n

        member data.getReactions rateProvider n =
            match data with
            | BruteForceModel m -> m.getReactions rateProvider n
            | RandomChoiceModel m ->
                let x = 
                    m.getReactions rateProvider n

                let b =
                    match rateProvider.getModel n |> Option.bind (fun m -> m.getAllReactions() |> Some) with
                    | Some v -> v
                    | None -> []

                b

        member data.getAllReactions() =
            match data with
            | BruteForceModel _ -> failwith ""
            | RandomChoiceModel m -> failwith ""

        static member create t rateProvider si =
            match t with
            | BruteForce -> BruteForceModelData.create si |> BruteForceModel
            | RandomChoice -> RandomChoiceModelData.create rateProvider si |> RandomChoiceModel

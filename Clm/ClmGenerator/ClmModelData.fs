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


    type BruteForceModelData =
        {
            substInfo : SubstInfo
            allPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
            ligationPairs : list<list<ChiralAminoAcid> * list<ChiralAminoAcid>>
            catSynthPairs : list<SynthesisReaction * SynthCatalyst>
            catDestrPairs : list<DestructionReaction * DestrCatalyst>
            catLigPairs : list<LigationReaction * LigCatalyst>
            catRacemPairs : list<RacemizationReaction * RacemizationCatalyst>
        }

        member data.noOfRawReactions n =
            match n with
            | FoodCreationName -> 1
            | WasteRemovalName -> 1
            | WasteRecyclingName -> 1
            | SynthesisName -> data.substInfo.chiralAminoAcids.Length
            | DestructionName -> data.substInfo.chiralAminoAcids.Length
            | CatalyticSynthesisName -> data.catSynthPairs.Length
            | CatalyticDestructionName -> data.catDestrPairs.Length
            | LigationName -> data.ligationPairs.Length
            | CatalyticLigationName -> data.catLigPairs.Length
            | SedimentationDirectName -> data.allPairs.Length
            | SedimentationAllName -> data.substInfo.chiralAminoAcids.Length
            | RacemizationName -> data.substInfo.chiralAminoAcids.Length
            | CatalyticRacemizationName -> data.catRacemPairs.Length


        member data.getReactions rateProvider n =
            let t = RateGenerationType.BruteForce

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
            | LigationName -> createReactions (fun x -> LigationReaction x |> Ligation) data.ligationPairs
            | CatalyticLigationName -> createReactions (fun x -> CatalyticLigationReaction x |> CatalyticLigation) data.catLigPairs
            | SedimentationDirectName -> createReactions (fun x -> SedimentationDirectReaction x |> SedimentationDirect) data.allPairs
            | SedimentationAllName -> []
            | RacemizationName -> createReactions (fun a -> RacemizationReaction a |> Racemization) data.substInfo.chiralAminoAcids
            | CatalyticRacemizationName -> createReactions (fun x -> CatalyticRacemizationReaction x |> CatalyticRacemization) data.catRacemPairs


        static member create si =
            let allPairs =
                List.allPairs si.allChains si.allChains
                |> List.map (fun (a, b) -> orderPairs (a, b))
                |> List.filter (fun (a, _) -> a.Head.isL)
                |> List.distinct

            let ligationPairs = allPairs |> List.filter (fun (a, b) -> a.Length + b.Length <= si.maxPeptideLength.length)

            {
                substInfo = si
                allPairs = allPairs
                ligationPairs = allPairs |> List.filter (fun (a, b) -> a.Length + b.Length <= si.maxPeptideLength.length)
                catSynthPairs = List.allPairs (si.chiralAminoAcids |> List.map (fun c -> SynthesisReaction c)) si.synthCatalysts
                catDestrPairs = List.allPairs (si.chiralAminoAcids |> List.map (fun c -> DestructionReaction c)) si.destrCatalysts
                catLigPairs = List.allPairs (ligationPairs |> List.map (fun c -> LigationReaction c)) si.ligCatalysts
                catRacemPairs = List.allPairs (si.chiralAminoAcids |> List.map (fun c -> RacemizationReaction c)) si.racemCatalysts
            }

    //let getModelDataParamsCode (modelParams : ModelGenerationParams)


/////////////////////////////////

    type RateGeneratorInfo<'A, 'B> =
        {
            a : array<'A>
            b : array<'B>
            distr : Distribution
            rateProvider : ReactionRateProvider
            getReaction : 'A -> 'B -> Reaction
            reactionName : ReactionName
        }


    let generateReactions<'A, 'B> (i : RateGeneratorInfo<'A, 'B>) =
        let noOfTries = i.a.Length * i.b.Length

        match i.rateProvider.getPrimaryDistribution i.reactionName with
        | Some d ->
            let sn = d.successNumber noOfTries
            let idx = [ for _ in 1..sn -> (d.next i.a.Length, d.next i.b.Length) ]
            failwith ""
        | None -> []

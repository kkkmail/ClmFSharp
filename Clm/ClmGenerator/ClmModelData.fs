namespace Clm.Generator

open System
open FSharp.Collections

open ClmSys.VersionInfo
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.ModelParams
open Clm.CalculationData
open ClmDefaults.DefaultValuesExt
open ClmImpure.RateProvider
open ClmImpure.ReactionsExt

module ClmModelData =
    open Clm.Distributions

    let newSeed() = (new Random()).Next()


    type UpdateFuncType = 
        | UseArray
        | UseVariables
        | UseFunctions


    type ModelGenerationParams =
        {
            fileStructureVersionNumber : string
            versionNumber : string
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRateModelParams : List<ReactionRateModelParam>
            updateFuncType : UpdateFuncType
            defaultSetIndex : int
        }


    type AllParams =
        {
            modelGenerationParams : ModelGenerationParams
            modelCommandLineParams : list<ModelCommandLineParam>
        }

        static member getDefaultValue (d : ClmDefaultValue) numberOfAminoAcids maxPeptideLength i =
            let rates = d.defaultRateParams

            {
                modelGenerationParams =
                    {
                        fileStructureVersionNumber = FileStructureVersionNumber
                        versionNumber = VersionNumber
                        numberOfAminoAcids = numberOfAminoAcids
                        maxPeptideLength = maxPeptideLength
                        reactionRateModelParams = rates.rateParams
                        updateFuncType = UseFunctions
                        defaultSetIndex = i
                    }

                modelCommandLineParams = d.modelCommandLineParams
            }

    let reactionShift updateFuncType =
        match updateFuncType with
        | UseArray -> "    "
        | UseVariables -> "    "
        | UseFunctions -> ""


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

        member data.getReactions rnd sdp rateProvider t n =
            let createReactions c l =
                let create a = c a |> AnyReaction.tryCreateReaction rnd rateProvider t

                l
                |> List.map create
                |> List.choose id
                |> List.concat

            match n with
            | FoodCreationName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (FoodCreationReaction |> FoodCreation) ] |> List.choose id |> List.concat
            | WasteRemovalName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (WasteRemovalReaction |> WasteRemoval) ] |> List.choose id |> List.concat
            | WasteRecyclingName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (WasteRecyclingReaction |> WasteRecycling) ] |> List.choose id |> List.concat
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


        member data.getReactions rnd rateProvider n =
            data.commonData.getReactions rnd data.allPairs rateProvider BruteForce n


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


    let generatePairs<'A, 'B> rnd (i : RateGeneratorInfo<'A, 'B>) (rateProvider : ReactionRateProvider) =
        // !!! must adjust for 4x reduction due to grouping of (A + B, A + E(B), E(A) + E(B), E(A) + B)
        let noOfTries = i.a.Length * i.b.Length / 4
        printfn "generatePairs: noOfTries = %A, typedefof<'A> = %A, typedefof<'A> = %A\n" noOfTries (typedefof<'A>) (typedefof<'B>)

        let d = Distribution.createUniform DistributionParams.defaultValue
        let sn = d.successNumber rnd noOfTries
        printfn "generatePairs.sn = %A" sn
        [ for _ in 1..sn -> (i.a.[d.nextN rnd i.a.Length], i.b.[d.nextN rnd i.b.Length]) ]


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

        member data.getReactions rnd rateProvider n =
            data.commonData.getReactions rnd data.sedDirPairs rateProvider RandomChoice n

        static member create rnd rateProvider si =
            {
                sedDirPairs = []

                commonData =
                    {
                        substInfo = si
                        catSynthPairs = generatePairs rnd si.catSynthInfo rateProvider
                        catDestrPairs = generatePairs rnd si.catDestrInfo rateProvider
                        catLigPairs = generatePairs rnd si.catLigInfo rateProvider
                        catRacemPairs = generatePairs rnd si.catRacemInfo rateProvider
                    }
            }


    type RateGenerationData =
        | BruteForceModel of BruteForceModelData
        | RandomChoiceModel of RandomChoiceModelData

        member data.noOfRawReactions n =
            match data with
            | BruteForceModel m -> m.noOfRawReactions n
            | RandomChoiceModel m -> m.noOfRawReactions n

        member data.getReactions rnd rateProvider n =
            match data with
            | BruteForceModel m -> m.getReactions rnd rateProvider n
            | RandomChoiceModel m ->
                let x = 
                    m.getReactions rnd rateProvider n

                let b =
                    match rateProvider.tryGetModel n |> Option.bind (fun m -> m.getAllReactions() |> Some) with
                    | Some v -> v
                    | None -> []

                b

        static member create rnd t rateProvider si =
            match t with
            | BruteForce -> BruteForceModelData.create si |> BruteForceModel
            | RandomChoice -> RandomChoiceModelData.create rnd rateProvider si |> RandomChoiceModel

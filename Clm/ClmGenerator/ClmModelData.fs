namespace Clm.Generator

open System
open FSharp.Collections

open ClmSys.VersionInfo
open ClmSys.ContGenPrimitives
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.ModelParams
open Clm.CalculationData
open ClmImpure.RateProvider
open ClmImpure.ReactionsExt
open Clm.Generator.ReactionRatesExt
open ClmSys.GeneralData
open Clm.Distributions

module ClmModelData =

    let newSeed() = (new Random()).Next()


    type UpdateFuncType =
        | UseArray
        | UseVariables
        | UseFunctions


    type ModelGenerationParams =
        {
            fileStructureVersion : decimal
            versionNumber : string
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRateModelParams : List<ReactionRateModelParam>
            updateFuncType : UpdateFuncType
            clmDefaultValueId : ClmDefaultValueId
            successNumberType : SuccessNumberType
        }


    type AllParams =
        {
            modelGenerationParams : ModelGenerationParams
            modelCommandLineParams : list<ModelCommandLineParam>
        }

        static member create g (c : ClmTask) =
            match g c.clmTaskInfo.clmDefaultValueId with
            | Ok v ->
                {
                    modelGenerationParams =
                        {
                            fileStructureVersion = FileStructureVersion
                            versionNumber = VersionNumberValue
                            numberOfAminoAcids = c.clmTaskInfo.numberOfAminoAcids
                            maxPeptideLength = c.clmTaskInfo.maxPeptideLength
                            reactionRateModelParams = v.defaultRateParams.rateParams
                            updateFuncType = UseFunctions
                            clmDefaultValueId = c.clmTaskInfo.clmDefaultValueId
                            successNumberType = v.defaultRateParams.successNumberType
                        }

                    modelCommandLineParams = c.commandLineParams
                }
                |> Ok
            | Error e -> Error e

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


    type RateGenerationCommonData =
        {
            substInfo : SubstInfo
            catSynthPairs : list<SynthesisReaction * SynthCatalyst>
            catDestrPairs : list<DestructionReaction * DestrCatalyst>
            catLigPairs : list<LigationReaction * LigCatalyst>
            catRacemPairs : list<RacemizationReaction * RacemizationCatalyst>
            sedDirPairs : list<ChiralAminoAcid * SedDirAgent>
        }

        member data.getReactions rnd rateProvider t n =
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
            | LigationName -> createReactions (fun x -> x |> Ligation) data.substInfo.ligationPairs
            | CatalyticLigationName -> createReactions (fun x -> CatalyticLigationReaction x |> CatalyticLigation) data.catLigPairs
            | SedimentationDirectName -> createReactions (fun (c, r) -> SedimentationDirectReaction ([ c ] |> SedDirReagent, r) |> SedimentationDirect) data.sedDirPairs
            | SedimentationAllName -> []
            | RacemizationName -> createReactions (fun a -> RacemizationReaction a |> Racemization) data.substInfo.chiralAminoAcids
            | CatalyticRacemizationName -> createReactions (fun x -> CatalyticRacemizationReaction x |> CatalyticRacemization) data.catRacemPairs


    let generatePairs<'A, 'B> (rnd : RandomValueGetter) (i : RateGeneratorInfo<'A, 'B>) (rateProvider : ReactionRateProvider) =
        // !!! must adjust for 4x reduction due to grouping of (A + B, A + E(B), E(A) + B, E(A) + E(B))
        let noOfTries = (int64 i.a.Length) * (int64 i.b.Length) / 4L
        printfn "generatePairs: noOfTries = %A, typedefof<'A> = %A, typedefof<'A> = %A\n" noOfTries (typedefof<'A>) (typedefof<'B>)

        let sng =
            rnd
            |>
            match i.successNumberType with
            | RandomValueBased -> RandomValueGetterBased
            | ThresholdBased -> ThresholdValueBased

        match rateProvider.tryGetPrimaryDistribution i.reactionName with
        | Some d ->
            let sn = d.successNumber sng noOfTries
            printfn "generatePairs: successNumberType = %A, sn = %A" i.successNumberType sn
            [ for _ in 1..sn -> (i.a.[d.nextN rnd i.a.Length], i.b.[d.nextN rnd i.b.Length]) ]
        | None -> []


    type RandomChoiceModelData =
        {
            commonData : RateGenerationCommonData
        }

        member data.noOfRawReactions n =
            let si = data.commonData.substInfo

            match n with
            | FoodCreationName -> 1L
            | WasteRemovalName -> 1L
            | WasteRecyclingName -> 1L
            | SynthesisName -> int64 si.chiralAminoAcids.Length
            | DestructionName -> int64 si.chiralAminoAcids.Length
            | CatalyticSynthesisName -> (int64 si.synthesisReactions.Length) * (int64 si.synthCatalysts.Length)
            | CatalyticDestructionName -> (int64 si.destructionReactions.Length) * (int64 si.destrCatalysts.Length)
            | LigationName -> int64 si.ligationPairs.Length
            | CatalyticLigationName -> (int64 si.ligationReactions.Length) * (int64 si.ligCatalysts.Length)
            | SedimentationDirectName -> (int64 si.allChains.Length) * (int64 si.allChains.Length)
            | SedimentationAllName -> int64 si.chiralAminoAcids.Length
            | RacemizationName -> int64 si.chiralAminoAcids.Length
            | CatalyticRacemizationName -> (int64 si.racemizationReactions.Length) * (int64 si.racemCatalysts.Length)

        member data.getReactions rnd rateProvider n = data.commonData.getReactions rnd rateProvider RandomChoice n

        /// Note that currently all generators share the same success number type.
        static member create rnd rateProvider (si : SubstInfo) st =
            let generatePairs x = generatePairs rnd x rateProvider

            {
                commonData =
                    {
                        substInfo = si
                        catSynthPairs = generatePairs (si.catSynthInfo st)
                        catDestrPairs = generatePairs (si.catDestrInfo st)
                        catLigPairs = generatePairs (si.catLigInfo st)
                        catRacemPairs = generatePairs (si.catRacemInfo st)
                        sedDirPairs = generatePairs (si.sedDirInfo st)
                    }
            }


    type RateGenerationData =
        | RandomChoiceModel of RandomChoiceModelData

        member data.noOfRawReactions n =
            match data with
            | RandomChoiceModel m -> m.noOfRawReactions n

        member data.getReactions rnd rateProvider n =
            match data with
            | RandomChoiceModel m ->
                let x = m.getReactions rnd rateProvider n

                let b =
                    match rateProvider.tryGetModel n |> Option.bind (fun m -> m.getAllReactions() |> Some) with
                    | Some v -> v
                    | None -> []

                b

        static member create rnd t rateProvider si st =
            match t with
            | RandomChoice -> RandomChoiceModelData.create rnd rateProvider si st |> RandomChoiceModel

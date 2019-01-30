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

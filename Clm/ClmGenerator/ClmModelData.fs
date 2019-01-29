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
            racemCatalysts : list<RacemizationCatalyst>
            allChains : list<list<ChiralAminoAcid>>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allNamesMap : Map<Substance, string>
        }

        static member create maxPeptideLength numberOfAminoAcids =
            let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids

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
                synthCatalysts = peptides |> List.map (fun p -> SynthCatalyst p)
                destrCatalysts = peptides |> List.map (fun p -> DestrCatalyst p)
                ligCatalysts = peptides |> List.map (fun p -> LigCatalyst p)
                racemCatalysts = peptides |> List.map (fun p -> RacemizationCatalyst p)
                allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))
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


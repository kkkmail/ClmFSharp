namespace Clm.Generator

open Clm.Substances
open Clm.DataLocation
open Clm.ReactionRates
open Clm.RateModelsExt
open Clm.SettingsExt
open Clm.ModelParams
open Clm.Generator.ClmModelData

module SettingGenExt =

    type UpdateFuncType
        with
        static member tryGet (m : SettingMap) po =
            match getTextOpt m po UpdateFuncTypeName with
            | Some s -> 
                match s with
                | UseArrayName -> Some UseArray
                | UseVariablesName -> Some UseVariables
                | UseFunctionsName -> Some UseFunctions
                | _ -> None
            | None -> None

        member this.setValue po s =
            add s [ setText po UpdateFuncTypeName (this.ToString()) ]


    [<Literal>]
    let updateFuncTypeName = "updateFuncType"

    [<Literal>]
    let modelLocationDataName = "modelLocationData"


    type ModelGenerationParams
        with

        static member tryGet (m : SettingMap) po =
            let a() = getTextOpt m po fileStructureVersionNumberName
            let b() = getTextOpt m po versionNumberName
            let c() = getIntOpt m po seedValueName
            let d() = addParent po numberOfAminoAcidsName |> NumberOfAminoAcids.tryGet m
            let e() = addParent po maxPeptideLengthName |> MaxPeptideLength.tryGet m
            let g() = addParent po updateFuncTypeName |> UpdateFuncType.tryGet m
            let h() = addParent po modelLocationDataName |> ModelLocationInputData.tryGet m
            let j() = getIntOpt m po defaultSetIndexName

            match a(), b(), d(), e(), g(), h() with
            | Some a1, Some b1, Some d1, Some e1, Some g1, Some h1 ->
                let p = ReactionRateModelParamWithUsage.getAll m po

                let models =
                    ReactionRateModel.createAll p d1
                    |> List.choose (fun e -> match e.usage with | PrimaryParam -> Some e.model | DependsOnParam -> None)

                let j1 =
                    match j() with
                    | Some v -> v
                    | None ->
                        printfn "ModelGenerationParams.defaultSetIndex is not found. Setting it to -1."
                        -1

                {
                    fileStructureVersionNumber = a1
                    versionNumber = b1
                    seedValue = c()
                    numberOfAminoAcids = d1
                    maxPeptideLength = e1
                    reactionRateModels = models
                    updateFuncType = g1
                    modelLocationData = h1
                    defaultSetIndex = j1
                }
                |> Some
            | _ -> None

        member this.setValue po s =
            let rates =
                {
                    rateModels = this.reactionRateModels
                }.allParams
                |> List.sort

            [
                setText po fileStructureVersionNumberName this.fileStructureVersionNumber |> Some
                setText po versionNumberName this.versionNumber |> Some
                setIntOpt po seedValueName this.seedValue
                setInt po defaultSetIndexName this.defaultSetIndex |> Some
            ]
            |> List.choose id
            |> add s
            |> this.numberOfAminoAcids.setValue (addParent po numberOfAminoAcidsName)
            |> this.maxPeptideLength.setValue (addParent po maxPeptideLengthName)
            |> this.updateFuncType.setValue (addParent po updateFuncTypeName)
            |> this.modelLocationData.setValue (addParent po modelLocationDataName)
            |> ReactionRateModelParamWithUsage.setAll rates po

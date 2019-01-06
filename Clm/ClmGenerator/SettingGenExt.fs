namespace Clm.Generator

open Clm.Substances
open Clm.Distributions
open Clm.DataLocation
open Clm.ReactionRates
open Clm.GeneralData
open Clm.RateModelsExt
open Clm.SettingsExt
open Clm.ModelParams
open ClmModel


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
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            let a() = 
                let x = getTextOpt m po fileStructureVersionNumberName
                x
            let b() = 
                let x = getTextOpt m po versionNumberName
                x
            let c() = 
                let x = getIntOpt m po seedValueName
                x
            let d() = 
                let x = addParent po numberOfAminoAcidsName |> NumberOfAminoAcids.tryGet m
                x
            let e() = 
                let x = addParent po maxPeptideLengthName |> MaxPeptideLength.tryGet m
                x
            let g() = 
                let x = addParent po updateFuncTypeName |> UpdateFuncType.tryGet m
                x
            let h() = 
                let x = addParent po modelLocationDataName |> ModelLocationInputData.tryGet m
                x
            let i() = 
                let x = getBoolOpt m po updateAllModelsName
                x

            match a(), b(), d(), e(), g(), h(), i() with
            | Some a1, Some b1, Some d1, Some e1, Some g1, Some h1, Some i1 ->
                let p = ReactionRateModelParamWithUsage.getAll m seeder po

                let models =
                    ReactionRateModel.createAll p d1
                    |> List.choose (fun e -> match e.usage with | PrimaryParam -> Some e.model | DependsOnParam -> None)

                {
                    fileStructureVersionNumber = a1
                    versionNumber = b1
                    seedValue = c()
                    numberOfAminoAcids = d1
                    maxPeptideLength = e1
                    reactionRateModels = models
                    updateFuncType = g1
                    modelLocationData = h1
                    updateAllModels = i1
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
                setBool po updateAllModelsName this.updateAllModels |> Some
            ]
            |> List.choose id
            |> add s
            |> this.numberOfAminoAcids.setValue (addParent po numberOfAminoAcidsName)
            |> this.maxPeptideLength.setValue (addParent po maxPeptideLengthName)
            |> this.updateFuncType.setValue (addParent po updateFuncTypeName)
            |> this.modelLocationData.setValue (addParent po modelLocationDataName)
            |> ReactionRateModelParamWithUsage.setAll rates po

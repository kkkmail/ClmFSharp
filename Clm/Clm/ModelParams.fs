namespace Clm

open System
open FSharp.Collections
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.GeneralData
open Clm.CommandLine
open Argu

module ModelParams =

    type ModelInfo =
        {
            fileStructureVersionNumber : string
            versionNumber : string
            seedValue : int
            modelName : string
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            updateAllModels : bool // true if updating AllModels.fs file was done. This is needed to update / do not update all results.
            allResultsFile : string
        }


    type ModelInfoWithModels =
        {
            modelInfo : ModelInfo
            allModels : list<ReactionRateModel>
        }


    type ModelDataParams =
        {
            modelInfo : ModelInfo
            allParams : list<ReactionRateModelParamWithUsage>
        }


    type ResultData =
        {
            resultDataId : int64 option
            modelDataId : int64
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength

            aminoAcids : list<AminoAcid>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int>
            allReactions : list<ReactionName * int>

            y0 : decimal
            tEnd : decimal
            useAbundant : bool
            x : double [,]
            t : double []
            maxEe : double
        }

        member rd.getTotals x = getTotalsValue rd.allInd rd.allSubst rd.aminoAcids x
        member rd.getTotalSubst x = getTotalSubstValue rd.allInd rd.allSubst x


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


    [<Literal>]
    let ModelCommandLineParamName = "ModelCommandLineParam"

    type ModelCommandLineParam =
        {
            tEnd : double
            y0 : double
            useAbundant : bool
        }

        override this.ToString() =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = "SolverRunner.exe")
            [
                EndTime this.tEnd
                TotalAmount this.y0
                UseAbundant this.useAbundant
                PlotResults true
            ]
            |> parser.PrintCommandLineArgumentsFlat

        static member name = ModelCommandLineParamName
        static member variableName = ModelCommandLineParam.name |> toVariableName


    type Setting =
        {
            settingId : Guid option
            settingPath : list<string * int>
            settingBit : bool
            settingLong : int64
            settingMoney : decimal
            settingFloat : float
            settingDate : DateTime option
            settingText : string option
            settingMemo : string option
            settingGUID : Guid option
        }

        static member defaultValue() =
            {
                settingId = Guid.NewGuid() |> Some
                settingPath = []
                settingBit = false
                settingLong = 0L
                settingMoney = 0m
                settingFloat = 0.0
                settingDate = None
                settingText = None
                settingMemo = None
                settingGUID = None
            }


    type SettingMap = Map<list<string * int>, Setting>


    type ResultSettings =
        {
            resultDataId : int64
            settings : SettingMap
        }


    type ModelData =
        {
            modelDataId : int64
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int option
            fileStructureVersion : string
            modelData : string
        }

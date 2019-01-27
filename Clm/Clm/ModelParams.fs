namespace Clm

open System
open FSharp.Collections
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmSys.GeneralData
open Clm.CommandLine
open Argu

module ModelParams =

    type ModelInfo =
        {
            fileStructureVersionNumber : string
            versionNumber : string
            modelDataId : int64
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int
            updateAllModels : bool // true if updating AllModels.fs file was done. This is needed to update / do not update all results.
            allResultsFile : string
            defaultSetIndex : int
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

            y0 : decimal
            tEnd : decimal
            useAbundant : bool

            maxEe : double
            maxAverageEe : double

            aminoAcids : list<AminoAcid>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int>
            allReactions : list<ReactionName * int>

            x : double [,]
            t : double []
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
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
            saveModelSettings : bool
        }

        override this.ToString() =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = "SolverRunner.exe")
            [
                EndTime this.tEnd
                TotalAmount this.y0
                UseAbundant this.useAbundant
                PlotResults false
                SaveModelSettings this.saveModelSettings
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


    type ModelSettings =
        {
            modelDataId : int64
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
            defaultSetIndex : int
        }


    type RunQueueInfo =
        {
            modelDataId : int64
            y0 : decimal
            tEnd : decimal
            useAbundant : bool
        }

        static member fromModelCommandLineParam (p : ModelCommandLineParam) (modelDataId : int64) =
            {
                modelDataId = modelDataId
                y0 = p.y0
                tEnd = p.y0
                useAbundant = p.useAbundant
            }


    type RunQueue =
        {
            runQueueId : int64
            info : RunQueueInfo
            statusId : int
        }

        member q.modelCommandLineParam =
            {
                tEnd = q.info.tEnd
                y0 = q.info.y0
                useAbundant = q.info.useAbundant
                saveModelSettings = false
            }


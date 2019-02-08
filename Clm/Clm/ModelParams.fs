namespace Clm

open System
open FSharp.Collections
open ClmSys.GeneralData
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
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
            allParams : array<ReactionRateModelParamWithUsage>
        }


    type SimpleResultData =
        {
            resultDataId : ResultDataId option
            modelDataId : ModelDataId
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength

            y0 : decimal
            tEnd : decimal
            useAbundant : bool

            maxEe : double
            maxAverageEe : double
        }

    type BinaryResultData =
        {
            aminoAcids : list<AminoAcid>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int>
            allReactions : list<ReactionName * int>

            x : double [,]
            t : double []
        }

    type ResultData =
        {
            simpleData : SimpleResultData
            binaryDataOpt : BinaryResultData option
        }


    type FullResultData =
        {
            simpleData : SimpleResultData
            binaryData : BinaryResultData

        }

        member resultData.getTotals x = getTotalsValue resultData.binaryData.allInd resultData.binaryData.allSubst resultData.binaryData.aminoAcids x
        member resultData.getTotalSubst x = getTotalSubstValue resultData.binaryData.allInd resultData.binaryData.allSubst x

        member frd.resultData =
            {
                simpleData = frd.simpleData
                binaryDataOpt = Some frd.binaryData
            }

        member frd.resultDataWithoutBinary = { frd.resultData with binaryDataOpt = None }


    type ResultData
        with
        member rd.fullResultData =
            match rd.binaryDataOpt with
            | Some b ->
                {
                    simpleData = rd.simpleData
                    binaryData = b
                }
                |> Some
            | None -> None


    type ModelDataRegularParams =
        {
            modelDataParams : ModelDataParams
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int>
            allReactions : list<ReactionName * int>
        }


    type ModelDataFuncParams =
        {
            getTotals : array<double> -> array<double * double>
            getTotalSubst : array<double> -> double
            getDerivative : array<double> -> array<double>
        }


    type ModelDataParamsWithExtraData =
        {
            regularParams : ModelDataRegularParams
            funcParams : ModelDataFuncParams
        }


    [<Literal>]
    let ModelCommandLineParamName = "ModelCommandLineParam"

    type ModelCommandLineParam =
        {
            modelDataId : ModelDataId
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
        }

        override this.ToString() =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = "SolverRunner.exe")
            let (ModelDataId modelDataId) = this.modelDataId
            [
                EndTime this.tEnd
                TotalAmount this.y0
                UseAbundant this.useAbundant
                PlotResults false
                ModelId modelDataId
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
            resultDataId : ResultDataId
            settings : SettingMap
        }


    type ModelSettings =
        {
            modelDataId : ModelDataId
            settings : SettingMap
        }


    type RunQueueInfo = ModelCommandLineParam


    type RunQueue =
        {
            runQueueId : RunQueueId
            info : RunQueueInfo
            statusId : int
        }

        member q.modelCommandLineParam = q.info

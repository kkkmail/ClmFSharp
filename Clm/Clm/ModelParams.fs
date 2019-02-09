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


    type ResultData =
        {
            modelDataId : ModelDataId

            y0 : decimal
            tEnd : decimal
            useAbundant : bool

            maxEe : double
            maxAverageEe : double
        }


    type ResultDataWithId =
        {
            resultDataId : ResultDataId
            resultData : ResultData
        }


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
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
        }

        member this.toCommandLine (ModelDataId modelDataId) =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)

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


    type RunQueueInfo =
        {
            modelDataId : ModelDataId
            modelCommandLineParam : ModelCommandLineParam
        }

        static member fromModelCommandLineParam modelDataId p =
            {
                modelDataId = modelDataId
                modelCommandLineParam = p
            }


    type RunQueue =
        {
            runQueueId : RunQueueId
            info : RunQueueInfo
            statusId : int
        }

        member q.modelCommandLineParam = q.info.modelCommandLineParam

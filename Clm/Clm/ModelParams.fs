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

    let toModelName (n : int64) = n.ToString().PadLeft(6, '0')

    /// TODO kk:20190107 - This should be exposed as a command line parameter.
    [<Literal>]
    let DefaultRootFolder = @"C:\Clm\"

    [<Literal>]
    let DefaultResultLocationFolder = DefaultRootFolder + @"Results"

    [<Literal>]
    let DefaultModelDataFile = __SOURCE_DIRECTORY__ + @"\..\Model\ModelData.fs"


    type ModelInfo =
        {
            fileStructureVersionNumber : string
            versionNumber : string
            modelDataId : ModelDataId
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int
            defaultSetIndex : int
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


    type FullResultData =
        {
            resultData : ResultData
            binaryResultData : BinaryResultData
            maxPeptideLength : MaxPeptideLength // Cannot be easily inferred from binary data but is needed here and there.
        }

        member resultData.getTotals x =
            getTotalsValue resultData.binaryResultData.allInd resultData.binaryResultData.allSubst resultData.binaryResultData.aminoAcids x

        member resultData.getTotalSubst x =
            getTotalSubstValue resultData.binaryResultData.allInd resultData.binaryResultData.allSubst x


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
                ModelId modelDataId
            ]
            |> parser.PrintCommandLineArgumentsFlat


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


    type ResultInfo =
        {
            resultLocation : string
            separator : string
        }

        static member defautlValue =
            {
                resultLocation = DefaultResultLocationFolder
                separator = "_"
            }

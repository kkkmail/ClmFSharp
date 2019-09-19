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

    [<Literal>]
    let DefaultRootFolder = DefaultRootDrive + @":\" + ClmBaseName + @"\"

    [<Literal>]
    let DefaultResultLocationFolder = DefaultRootFolder + @"Results"

    [<Literal>]
    let DefaultFileStorageFolder = DefaultRootFolder + @"FileStorage"

    [<Literal>]
    let DefaultModelDataFile = __SOURCE_DIRECTORY__ + @"\..\Model\ModelData.fs"


    let toModelName (n : Guid) = n.ToString()


    type ModelInfo =
        {
            fileStructureVersion : decimal
            versionNumber : string
            modelDataId : ModelDataId
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int
            clmDefaultValueId : ClmDefaultValueId
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

            // All are using abs. Averaging is perfomred first, then abs is applied.
            maxEe : double // max ee over all data points and all pairs of chiral substances.
            maxAverageEe : double // max value of ee averaged over evolution period per each pair of chiral substances.
            maxWeightedAverageAbsEe : double // the same as above but using linear weighted average and abs of ee.
            maxLastEe : double // max ee at the last point.
        }


    type ResultDataWithId =
        {
            resultDataId : ResultDataId
            resultData : ResultData
        }


    type AllSubstData =
        {
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int64>
            allReactions : list<ReactionName * int64>
        }


    type BinaryInfo =
        {
            aminoAcids : list<AminoAcid>
            maxPeptideLength : MaxPeptideLength // Cannot be easily inferred from the binary data but is needed here and there.
            allSubstData : AllSubstData
        }

        member info.getTotals x =
            getTotalsValue info.allSubstData.allInd info.allSubstData.allSubst info.aminoAcids x

        member info.getTotalSubst x =
            getTotalSubstValue info.allSubstData.allInd info.allSubstData.allSubst x


    type BinaryResultData =
        {
            binaryInfo : BinaryInfo

            x : double [,]
            t : double []
        }


    type FullResultData =
        {
            resultData : ResultData
            binaryResultData : BinaryResultData
        }

        member resultData.getTotals x = resultData.binaryResultData.binaryInfo.getTotals x
        member resultData.getTotalSubst x = resultData.binaryResultData.binaryInfo.getTotalSubst x


    type ModelDataRegularParams =
        {
            modelDataParams : ModelDataParams
            allSubstData : AllSubstData
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

        member info.binaryInfo =
            {
                aminoAcids = AminoAcid.getAminoAcids info.regularParams.modelDataParams.modelInfo.numberOfAminoAcids
                maxPeptideLength = info.regularParams.modelDataParams.modelInfo.maxPeptideLength
                allSubstData = info.regularParams.allSubstData
            }


    type ClmTaskId =
        | ClmTaskId of Guid

        member this.value = let (ClmTaskId v) = this in v


    type ClmDefaultValue =
        {
            clmDefaultValueId : ClmDefaultValueId
            defaultRateParams : ReactionRateProviderParams
            description : string option
        }


    /// Additional information needed to produce command line params for solver runner.
    type ModelCommandLineData =
        {
            modelDataId : ModelDataId
            resultDataId : ResultDataId
            minUsefulEe : MinUsefulEe
            remote : bool
        }


    /// Parameters, which come from ClmTask & related data.
    type ModelCommandLineTaskParam =
        {
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
        }


    type ModelCommandLineParam =
        {
            taskParam : ModelCommandLineTaskParam
            serviceAccessInfo : SolverRunnerAccessInfo
        }

        member this.toCommandLine (d : ModelCommandLineData) =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)

            [
                EndTime this.taskParam.tEnd
                TotalAmount this.taskParam.y0
                UseAbundant this.taskParam.useAbundant
                ModelId d.modelDataId.value
                NotifyAddress this.serviceAccessInfo.serviceAddress.value
                NotifyPort this.serviceAccessInfo.servicePort.value
                MinimumUsefulEe d.minUsefulEe.value
                Remote d.remote
                ResultId d.resultDataId.value
            ]
            |> parser.PrintCommandLineArgumentsFlat


    type RunQueueInfo =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            modelCommandLineParam : ModelCommandLineParam
        }


    type RunQueue =
        {
            runQueueId : RunQueueId
            info : RunQueueInfo
            statusId : int
        }

        member q.modelCommandLineParam = q.info.modelCommandLineParam

        static member fromModelCommandLineParam modelDataId defaultValueId p =
            {
                runQueueId = Guid.NewGuid() |> RunQueueId

                info =
                    {
                        modelDataId = modelDataId
                        defaultValueId = defaultValueId
                        modelCommandLineParam = p
                    }

                statusId = 0
            }


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


    type ClmTaskInfo =
        {
            clmTaskId : ClmTaskId
            clmDefaultValueId : ClmDefaultValueId
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
        }


    type ClmTask =
        {
            clmTaskInfo : ClmTaskInfo
            commandLineParams : list<ModelCommandLineParam>
            numberOfRepetitions : int
            remainingRepetitions : int
            createdOn : DateTime
        }

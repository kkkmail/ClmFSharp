﻿namespace Clm

open System
open FSharp.Collections
open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.SolverRunnerData
open ClmSys.ContGenData
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.CommandLine

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
            workerNodeId : WorkerNodeId
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


    type ClmDefaultValue =
        {
            clmDefaultValueId : ClmDefaultValueId
            defaultRateParams : ReactionRateProviderParams
            description : string option
        }


    /// TODO kk:20200214 - Possibly add NotifyAddress DefaultWorkerNodeServiceAddress
    /// and NotifyPort DefaultWorkerNodeServicePort here.
    /// Currently it is hard coded below in ModelCommandLineParam.
    /// Additional information needed to produce command line params for solver runner.
    type ModelCommandLineData =
        {
            modelDataId : ModelDataId
            resultDataId : ResultDataId
            workerNodeId : WorkerNodeId
            minUsefulEe : MinUsefulEe
            remote : bool
        }


    /// Parameters, which come from ClmTask & related data.
    type ModelCommandLineParam =
        {
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
        }


        member this.toCommandLine (d : ModelCommandLineData) =
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)

            [
                EndTime this.tEnd
                TotalAmount this.y0
                UseAbundant this.useAbundant
                ModelId d.modelDataId.value
                MinimumUsefulEe d.minUsefulEe.value
                Remote d.remote
                ResultId d.resultDataId.value
                WrkNodeId d.workerNodeId.messagingClientId.value

                // TODO kk:20200214 - Currently hard coded to use worker node.
                NotifyAddress DefaultWorkerNodeServiceAddress
                NotifyPort DefaultWorkerNodeServicePort
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
            runQueueStatus : RunQueueStatus
            workerNodeIdOpt : WorkerNodeId option
            progress : TaskProgress
            createdOn : DateTime
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

                runQueueStatus = NotStartedRunQueue
                workerNodeIdOpt = None
                progress = NotStarted
                createdOn = DateTime.Now
            }

        override r.ToString() =
            let (ModelDataId modelDataId) = r.info.modelDataId
            let (ClmDefaultValueId defaultValueId) = r.info.defaultValueId
            let (RunQueueId runQueueId) = r.runQueueId
            let s = (DateTime.Now - r.createdOn).ToString("d\.hh\:mm")

            let estCompl =
                match r.runQueueStatus, r.progress.estimateEndTime r.createdOn with
                | InProgressRunQueue, Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
                | _ -> EmptyString

            sprintf "{ T: %s;%s DF: %A; MDID: %A; PID: %A; %A }" s estCompl defaultValueId modelDataId runQueueId r.progress


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

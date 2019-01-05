namespace Clm

open FSharp.Collections

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.GeneralData

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

        //member mdp.getModelDataInfo() =
        //    {
        //        numberOfAminoAcids = mdp.modelDataParams.modelInfo.numberOfAminoAcids
        //        maxPeptideLength = mdp.modelDataParams.modelInfo.maxPeptideLength
        //        aminoAcids = AminoAcid.getAminoAcids mdp.modelDataParams.modelInfo.numberOfAminoAcids
        //        allSubst = mdp.allSubst
        //        allInd =  mdp.allInd
        //        allRawReactions = mdp.allRawReactions
        //        allReactions =  mdp.allReactions
        //    }


    [<Literal>]
    let ModelCommandLineParamName = "ModelCommandLineParam"

    type ModelCommandLineParam =
        {
            tEnd : double
            y0 : double
            useAbundant : bool option
        }

        override this.ToString() =
            [
                this.tEnd.ToString() |> Some
                this.y0.ToString() |> Some
                this.useAbundant |> Option.bind (fun e -> (if e then Some "1" else None))
            ]
            |> List.choose id
            |> String.concat " "

        static member name = ModelCommandLineParamName
        static member variableName = ModelCommandLineParam.name |> toVariableName


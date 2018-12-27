namespace Clm

open FSharp.Collections

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates

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
            allParams : list<ReactionRateModelParam>
        }


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

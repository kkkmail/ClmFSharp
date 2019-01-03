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
            allParams : list<ReactionRateModelParamWithUsage>
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


    [<Literal>]
    let ModelCommandLineParamName = "ModelCommandLineParam"

    type ModelCommandLineParam =
        {
            tEnd : double
            y0 : double
            useAbundant : bool option
        }

        static member defaultValues =
            [
                {
                    tEnd = 10_000.0
                    y0 = 10.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 10.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 5.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 20.0
                    useAbundant = None
                }
            ]

        override this.ToString() =
            [
                this.tEnd.ToString() |> Some
                this.y0.ToString() |> Some
                this.useAbundant |> Option.bind (fun e -> (if e then Some "1" else None))
            ]
            |> List.choose id
            |> String.concat " "

        static member name = ModelCommandLineParamName
        static member variableName = ModelCommandLineParam.name |> Distributions.toVariableName

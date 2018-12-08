#load @"..\Clm\VersionInfo.fs"
#load @"..\Clm\Substances.fs"
#load @"..\Clm\ReactionTypes.fs"
#load @"..\Clm\Distributions.fs"
#load @"..\Clm\ReactionRates.fs"
#load @"..\Clm\Reactions.fs"
#load @"..\Clm\DataLocation.fs"
#load @"..\Clm\ModelParams.fs"
#load "FSharpCodeExt.fs"
#load "ClmModel.fs"

open System
open Clm.VersionInfo
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation
open ClmGenerator.ClmModel
//===========================================================
let updateAllModels = true

let numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
let maxPeptideLength = MaxPeptideLength.ThreeMax
//===========================================================
let seed = newSeed()
let rnd = new Random(seed)
//===========================================================
let synthModel = ReactionRateProvider.defaultSynthRndModel rnd (0.001, 0.0001)
let catSynthRndParams = (synthModel, (Some 0.0005), 1000.0)
//let catSynthRndParams = (synthModel, (Some 0.02), 1000.0)
//let catSynthModel = ReactionRateProvider.defaultCatSynthRndModel rnd catSynthRndParams
let catSynthModel = ReactionRateProvider.defaultCatSynthSimModel rnd catSynthRndParams (Some 0.2, numberOfAminoAcids)
//===========================================================
let ligModel = ReactionRateProvider.defaultLigRndModel rnd (0.001, 0.0001)
//let ligModel = ReactionRateProvider.defaultLigRndModel rnd (1.0, 0.1)
let catLigModel = ReactionRateProvider.defaultCatLigRndModel rnd (ligModel, (Some 0.00005), 2000.0)
//===========================================================
let sedDirModel = ReactionRateProvider.defaultSedDirRndModel rnd (0.00002, 10000.0)
let sedAllModel = ReactionRateProvider.defaultSedAllRndModel rnd 0.1
//===========================================================
let rates = 
    [
         synthModel |> SynthesisRateModel
         catSynthModel |> CatalyticSynthesisRateModel

         ligModel |> LigationRateModel
         catLigModel |> CatalyticLigationRateModel

         sedDirModel |> SedimentationDirectRateModel
         //sedAllModel |> SedimentationAllRateModel
    ]
//===========================================================
let modelGenerationParams = 
    {
        fileStructureVersionNumber = FileStructureVersionNumber
        versionNumber = VersionNumber
        seedValue = Some seed
        numberOfAminoAcids = numberOfAminoAcids
        maxPeptideLength = maxPeptideLength
        reactionRateModels = rates
        updateFuncType = UseFunctions
        modelLocationData = ModelLocationInputData.defaultValue
        updateAllModels = updateAllModels
    }
//===========================================================
printfn "Creating model..."
printfn "Starting at: %A" DateTime.Now
#time
let model = ClmModel modelGenerationParams
#time

printfn "allSubstances.Length = %A" model.allSubstances.Length
printfn "allReactions.Length = %A" model.allReactions.Length

#time
do model.generateCode()
#time
printfn "... completed."
//===========================================================
//// TODO kk:20181206 This should be converted into a test to ensure that the dictionary contains proper data.
//catSynthModel.rateDictionary
//|> Seq.toList
//|> List.filter (fun e -> match e.Value with | (None, None) -> false | _ -> true)
//|> List.map (fun e -> printfn "r: %A, v: %A" e.Key e.Value)

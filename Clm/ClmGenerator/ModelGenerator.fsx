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
open Clm.Generator.ClmModel
//===========================================================
let updateAllModels = false

let numberOfAminoAcids = NumberOfAminoAcids.FourAminoAcids
let maxPeptideLength = MaxPeptideLength.ThreeMax
//===========================================================
let seed = newSeed()
printfn "seed = %A" seed
let rnd = new Random(seed)
let rates = ReactionRateProvider.getDefaultRateModels rnd numberOfAminoAcids
//===========================================================
let modelGenerationParams = 
    {
        fileStructureVersion = FileStructureVersion
        versionNumber = VersionNumber
        seedValue = Some seed
        numberOfAminoAcids = numberOfAminoAcids
        maxPeptideLength = maxPeptideLength
        reactionRateModels = rates.rateModels
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
let code = model.generateCode()
#time
printfn "... completed."
//===========================================================
//// TODO kk:20181206 This should be converted into a test to ensure that the dictionary contains proper data.
//catSynthModel.rateDictionary
//|> Seq.toList
//|> List.filter (fun e -> match e.Value with | (None, None) -> false | _ -> true)
//|> List.map (fun e -> printfn "r: %A, v: %A" e.Key e.Value)

#load "VersionInfo.fs"
#load "Substances.fs"
#load "ReactionTypes.fs"
#load "ReactionRates.fs"
#load "Reactions.fs"
#load "DataLocation.fs"
#load "Model.fs"

open System
open Clm.VersionInfo
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation
open Clm.Model
//===========================================================
let updateAllModels = true

let n = NumberOfAminoAcids.NineAminoAcids
let m = MaxPeptideLength.ThreeMax
//===========================================================
let seed = (new Random()).Next()
let rnd = new Random(seed)
//===========================================================
let synthModel = ReactionRateProvider.defaultSynthesisModel rnd 0.001 0.01
//let synthModel = ReactionRateProvider.defaultSynthesisModel rnd (0.0001 / (double n.length)) 0.001
let ligModel = ReactionRateProvider.defaultLigationModel rnd 0.001 0.001

let catSynthModel = ReactionRateProvider.defaultCatalyticSynthesisModel rnd synthModel (Some 0.0005) 10000.0
let catLigModel = ReactionRateProvider.defaultCatalyticLigationModel rnd ligModel (Some 0.0001) 10000.0

let sdModel = ReactionRateProvider.defaultSedimentationDirectModel rnd 0.0001 1000.0
let saModel = ReactionRateProvider.defaultSedimentationAllModel rnd 0.1
//===========================================================
let rates = 
    [
         synthModel |> SynthesisRateModel
         ligModel |> LigationRateModel

         catSynthModel |> CatalyticSynthesisRateModel
         catLigModel |> CatalyticLigationRateModel
         sdModel |> SedimentationDirectRateModel

         //saModel |> SedimentationAllRateModel
    ]
//===========================================================
let modelGenerationParams = 
    {
        fileStructureVersionNumber = FileStructureVersionNumber
        versionNumber = VersionNumber
        seedValue = Some seed
        numberOfAminoAcids = n
        maxPeptideLength = m
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

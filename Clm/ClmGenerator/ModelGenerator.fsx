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
let updateAllModels = false

let n = NumberOfAminoAcids.OneAminoAcid
let m = MaxPeptideLength.ThreeMax
//===========================================================
let seed = (new Random()).Next()
let rnd = new Random(seed)
let aminoAcids = AminoAcid.getAminoAcids n
//===========================================================
let synthModel = ReactionRateProvider.defaultSynthRndModel rnd (0.001, 0.0001)
let ligModel = ReactionRateProvider.defaultLigRndModel rnd (0.001, 0.0001)

//let catSynthRndParams = (synthModel, (Some 0.0005), 1000.0)
let catSynthRndParams = (synthModel, (Some 0.05), 1000.0)
//let catSynthModel = ReactionRateProvider.defaultCatSynthRndModel rnd catSynthRndParams
let catSynthModel = ReactionRateProvider.defaultCatSynthSimModel rnd catSynthRndParams (None, aminoAcids)

let catLigModel = ReactionRateProvider.defaultCatLigRndModel rnd (ligModel, (Some 0.0001), 1000.0)

let sdModel = ReactionRateProvider.defaultSedDirRndModel rnd (0.0001, 1000.0)
let saModel = ReactionRateProvider.defaultSedAllRndModel rnd 0.1
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

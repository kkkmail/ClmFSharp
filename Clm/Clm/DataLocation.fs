namespace Clm

open System.IO
open System

module DataLocation =
    //[<Literal>]
    //let DefaultStartingFolder = __SOURCE_DIRECTORY__ + @"\..\Model\Models"
    //
    //[<Literal>]
    //let DefaultAllModelsFile = __SOURCE_DIRECTORY__ + @"\..\Model\AllModels.fs"
    //
    //[<Literal>]
    //let DefaultAllResultsFile = __SOURCE_DIRECTORY__ + @"\..\Results\AllResults.fs"


    //[<Literal>]
    //let ModeNameName = "ModeName"

    //[<Literal>]
    //let GenerateNameName = "GenerateName"

    //[<Literal>]
    //let ConsecutiveNameName = "ConsecutiveName"

    //type ModeName =
    //    | GenerateName
    //    | ConsecutiveName of int64

    //    member this.name =
    //        match this with
    //        | GenerateName -> GenerateNameName
    //        | ConsecutiveName _ -> ConsecutiveNameName


    //[<Literal>]
    //let ModelLocationInputDataName = "ModelLocationInputData"

    //type ModelLocationInputData = 
    //    {
    //        startingFolder : string
    //        separator : string
    //        padLength : int
    //        allModelsFile : string
    //        allResultsFile : string
    //        modelName : ModeName
    //        useDefaultModeData : bool
    //    }

    //    static member defaultValue =
    //        {
    //            startingFolder = DefaultStartingFolder
    //            separator = "_"
    //            padLength = 3
    //            allModelsFile = DefaultAllModelsFile
    //            allResultsFile = DefaultAllResultsFile
    //            modelName = GenerateName
    //            useDefaultModeData = false
    //        }


    //type ModelLocationInfo =
    //    {
    //        modelFolder : string
    //        modelDataId : int64
    //        useDefaultModeData : bool
    //    }

    //    //member this.modelName = toModelName this.modelDataId


    //    static member modelDataName = "ModelData"

    //    member location.outputFile =
    //        if location.useDefaultModeData then DefaultModelDataFile
    //        else Path.Combine(location.modelFolder, ModelLocationInfo.modelDataName) + ".fs"


    //let createModelLocationInfo (i : ModelLocationInputData) =
    //    match i.modelName with
    //    | ConsecutiveName n -> 
    //        {
    //            modelFolder = String.Empty
    //            modelDataId = n
    //            useDefaultModeData = i.useDefaultModeData
    //        }
    //    | GenerateName ->
    //        let dirs = Directory.EnumerateDirectories(i.startingFolder) |> List.ofSeq |> List.map Path.GetFileName

    //        let todayMaxDirNumber =
    //            (
    //                dirs
    //                |> List.map (fun d -> d.Replace("_", ""))
    //                |> List.choose (fun n -> match Int64.TryParse n with | (true, i) -> Some i | (false, _) -> None)
    //            )
    //            @ [ 0L ]
    //            |> List.max

    //        let modelDataId = todayMaxDirNumber + 1L
    //        let modelName = modelDataId |> toModelName
    //        let fullNextDir = Path.Combine(i.startingFolder, modelName)
    //        Directory.CreateDirectory(fullNextDir) |> ignore

    //        {
    //            modelFolder = fullNextDir
    //            modelDataId = modelDataId
    //            useDefaultModeData = false
    //        }

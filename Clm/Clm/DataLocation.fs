namespace Clm

open System.IO
open System

module DataLocation = 

    [<Literal>]
    let DefaultStartingFolder = __SOURCE_DIRECTORY__ + @"\..\Model\Models"


    [<Literal>]
    let DefaultAllModelsFile = __SOURCE_DIRECTORY__ + @"\..\Model\AllModels.fs"


    [<Literal>]
    let DefaultAllResultsFile = __SOURCE_DIRECTORY__ + @"\..\Results\AllResults.fs"


    [<Literal>]
    let DefaultResultLocationFolder = __SOURCE_DIRECTORY__ + @"\..\Results\Data"


    [<Literal>]
    let ModelLocationInputDataName = "ModelLocationInputData"

    type ModelLocationInputData = 
        {
            startingFolder : string
            separator : string
            padLength : int
            allModelsFile : string
            allResultsFile : string
        }

        static member defaultValue = 
            {
                startingFolder = DefaultStartingFolder
                separator = "_"
                padLength = 3
                allModelsFile = DefaultAllModelsFile
                allResultsFile = DefaultAllResultsFile
            }


    type ModelLocationInfo = 
        {
            modelFolder : string
            modelName : string
        }

        static member modelDataName = "ModelData"
        member location.outputFile = Path.Combine(location.modelFolder, ModelLocationInfo.modelDataName) + ".fs"


    type ResultInfo =
        {
            resultLocation : string
            allResultsFile : string
            separator : string
        }

        static member defautlValue = 
            {
                resultLocation = DefaultResultLocationFolder
                allResultsFile = DefaultAllResultsFile
                separator = "_"
            }

    let createModelLocationInfo (i : ModelLocationInputData) =
        let dirs = Directory.EnumerateDirectories(i.startingFolder) |> List.ofSeq |> List.map Path.GetFileName
        let today = DateTime.Now
        let todayPrefix = today.ToString "yyyyMMdd"

        let todayMaxDirNumber = 
            (
                dirs 
                |> List.filter(fun d -> d.StartsWith(todayPrefix))
                |> List.map (fun d -> d.Substring(todayPrefix.Length).Replace("_", ""))
                |> List.choose (fun n -> match Int32.TryParse n with | (true, i) -> Some i | (false, _) -> None)
            )
            @ [ 0 ]
            |> List.max

        let modelName = todayPrefix + i.separator + (todayMaxDirNumber + 1).ToString().PadLeft(i.padLength, '0')
        let fullNextDir = Path.Combine(i.startingFolder, modelName)
        Directory.CreateDirectory(fullNextDir) |> ignore

        {
            modelFolder = fullNextDir
            modelName = modelName
        }

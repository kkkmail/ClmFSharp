namespace Clm

open System.IO
open System

module DataLocation = 

    type ModelLocationInputData = 
        {
            startingFolder : string
            separator : string
            padLength : int
            allModelsFile : string
        }

        static member defaultValue = 
            {
                startingFolder = @"C:\GitHub\CLM\Clm\Model\Models"
                separator = "_"
                padLength = 3
                allModelsFile = @"C:\GitHub\CLM\Clm\Model\AllModels.fs"
            }


    type ModelLocationInfo = 
        {
            modelFolder : string
            modelName : string
        }

        member location.modelDataName = "ModelData"
        member location.outputFile = Path.Combine(location.modelFolder, location.modelDataName) + ".fs"


    type ResultInfo =
        {
            resultLocation : string
            separator : string
        }

        static member defautlValue = 
            {
                resultLocation =  @"C:\GitHub\CLM\Clm\Results"
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

        //printfn "todayMaxDirNumber = %A" todayMaxDirNumber
        //printfn "nextDir = %A" nextDir
        //printfn "fullNextDir = %A" fullNextDir
        Directory.CreateDirectory(fullNextDir) |> ignore

        {
            modelFolder = fullNextDir
            modelName = modelName
        }

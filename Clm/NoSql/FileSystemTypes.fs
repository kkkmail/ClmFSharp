namespace NoSql

open Newtonsoft.Json
open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open System.IO

module FileSystemTypes =

    let fileStorageFolder = DefaultFileStorageFolder

    let modelDataTblName = "ModelData"
    let resultDataTblName = "ResultData"
    let chartTblName = "Chart"


    let getFileName<'A> tableName (objectId : 'A) =
        let folder = fileStorageFolder + "\\" + tableName
        Directory.CreateDirectory(folder) |> ignore
        Path.Combine(folder, objectId.ToString() + ".json")


    let tryLoadData<'T, 'A> tableName (objectId : 'A) =
        let f = getFileName tableName objectId

        if File.Exists f
        then File.ReadAllText(f) |> JsonConvert.DeserializeObject<'T> |> Some
        else None


    let saveData<'T, 'A> tableName (objectId : 'A) (t : 'T) =
        let f = getFileName tableName objectId
        let d = t |> JsonConvert.SerializeObject
        File.WriteAllText(f, d)
        true


    let saveModelDataFs (m : ModelData) = saveData<ModelData, Guid> modelDataTblName m.modelDataId.value m
    let tryLoadModelDataFs (ModelDataId modelDataId) = tryLoadData<ModelData, Guid> modelDataTblName modelDataId
    let saveResultDataFs (r : ResultDataWithId) = saveData<ResultDataWithId, Guid> resultDataTblName r.resultDataId.value r
    let tryLoadResultDataFs (ResultDataId resultDataId) = tryLoadData<ResultDataWithId, Guid> resultDataTblName resultDataId
    let saveChartsFs (ResultDataId resultDataId) (p : list<string>) = saveData<ChartInfo, Guid> chartTblName resultDataId { chartFileNames = p |> Array.ofList }

    let tryLoadCharts (ResultDataId resultDataId) =
        tryLoadData<ChartInfo, Guid> chartTblName resultDataId
        |> Option.bind (fun e -> e.chartFileNames |> List.ofArray |> Some)

namespace Analytics
open System.IO
open Microsoft.FSharp.Core

open Clm.Substances
open Clm.ModelParams
open Clm.ChartData
open FSharp.Plotly

module ChartExt =

    type PlotDataInfo =
        {
            resultInfo : ResultInfo
            useTempFolder : bool
        }

        static member defaultValue =
            {
                resultInfo = ResultInfo.defautlValue
                useTempFolder = false
            }


    type ChartType
        with

        member ct.fileSuffix =
            match ct with
            | PlotAminoAcids -> "aa"
            | PlotEnantiomericExcess -> "ee"
            | PlotTotalSubst -> "ts"

        member ct.getFileName (i : PlotDataInfo, r : FullResultData) =
            let suff = ct.fileSuffix

            let fileName =
                [
                    r.resultData.modelDataId.value |> toModelName
                    i.resultInfo.separator
                    (int r.resultData.y0).ToString().PadLeft(3, '0')
                    (int r.resultData.tEnd).ToString().PadLeft(5, '0')
                    suff
                ]
                |> String.concat i.resultInfo.separator

            Directory.CreateDirectory(i.resultInfo.resultLocation) |> ignore
            Path.Combine(i.resultInfo.resultLocation, fileName + ".html")

        member ct.getFileName (i : PlotDataInfo, d : ChartData) =
            failwith ""


    let showChart (i : PlotDataInfo) show fileName =
        match i.useTempFolder with
        | true -> Chart.ShowWithDescription show
        | false -> Chart.ShowFileWithDescription show fileName

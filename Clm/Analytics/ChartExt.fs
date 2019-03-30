﻿namespace Analytics
open System.IO
open Microsoft.FSharp.Core

open ClmSys.GeneralData
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

        member private ct.getFileNameImpl (i : PlotDataInfo) (modelDataId : ModelDataId) (y0 : decimal) (tEnd : decimal) =
            let suff = ct.fileSuffix

            let fileName =
                [
                    modelDataId.value |> toModelName
                    i.resultInfo.separator
                    (int y0).ToString().PadLeft(3, '0')
                    (int tEnd).ToString().PadLeft(5, '0')
                    suff
                ]
                |> String.concat i.resultInfo.separator

            Directory.CreateDirectory(i.resultInfo.resultLocation) |> ignore
            Path.Combine(i.resultInfo.resultLocation, fileName + ".html")

        member ct.getFileName (i : PlotDataInfo, r : FullResultData) = ct.getFileNameImpl i r.resultData.modelDataId r.resultData.y0 r.resultData.tEnd
        member ct.getFileName (i : PlotDataInfo, d : ChartData) = ct.getFileNameImpl i d.initData.modelDataId d.initData.y0 d.initData.tEnd


    let showChart (i : PlotDataInfo) show fileName =
        match i.useTempFolder with
        | true -> Chart.ShowWithDescription show
        | false -> Chart.ShowFileWithDescription show fileName
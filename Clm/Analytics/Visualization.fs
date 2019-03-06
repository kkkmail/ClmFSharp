﻿namespace Analytics

open System.IO
open Microsoft.FSharp.Core

open Clm.Substances
open Clm.ModelParams
open Clm.ChartData
open FSharp.Plotly

module Visualization =

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
            //| PlotAllSubst -> "as"
            | PlotChiralAminoAcids -> "ca"
            | PlotAminoAcids -> "aa"
            | PlotEnantiomericExcess -> "ee"
            | PlotTotalSubst -> "ts"

        member ct.getFileName (i : PlotDataInfo) (r : FullResultData) =
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


    type Plotter(i : PlotDataInfo, p : FullResultData) =
        let foodIdx = p.binaryResultData.binaryInfo.allSubstData.allInd.TryFind (AchiralSubst.Food |> Simple)
        let wasteIdx = p.binaryResultData.binaryInfo.allSubstData.allInd.TryFind (AchiralSubst.Waste |> Simple)
        let getFileName (ct : ChartType) = ct.getFileName i p
        let noOfOutputPoints = p.binaryResultData.t.Length - 1
        let minMax = (0.0, float p.resultData.tEnd)


        let description =
            [
                //"Comleted at", sprintf "%A" (o.endTime)
                //"run time", sprintf "%A" (o.endTime - o.startTime)
                "model name", p.resultData.modelDataId.value |> toModelName
                //"end time", sprintf "%A" o.endTime
                "y0", sprintf "%A" p.resultData.y0
                "number of amino acids", sprintf "%A" p.binaryResultData.binaryInfo.aminoAcids.Length
                "max peptide length", sprintf "%A" p.binaryResultData.binaryInfo.maxPeptideLength.length
                "number of substances", sprintf "%A" p.binaryResultData.binaryInfo.allSubstData.allSubst.Length
            ]
            @
            (p.binaryResultData.binaryInfo.allSubstData.allReactions |> List.map (fun (r, c) -> r.name, c.ToString()))
            @
            (p.binaryResultData.binaryInfo.allSubstData.allRawReactions |> List.map (fun (r, c) -> r.name + " (raw)", c.ToString()))
            |> List.map (fun (n, d) -> n + ": " + d)
            |> String.concat ", "


        let showChart show fileName =
            match i.useTempFolder with 
            | true -> Chart.ShowWithDescription show
            | false -> Chart.ShowFileWithDescription show fileName


        //let plotAllImpl show =
        //    let fn = [ for i in 0..p.binaryResultData.allSubst.Length - 1 -> i ]
        //    let tIdx = [ for i in 0..noOfOutputPoints -> i ]

        //    let getFuncData i = 
        //        tIdx
        //        |> List.map (fun t -> p.binaryResultData.t.[t], p.binaryResultData.x.[t,i])

        //    Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = i.ToString())))
        //    |> Chart.withX_AxisStyle("t", MinMax = minMax)
        //    |> showChart show (getFileName PlotAllSubst) description


        let plotChiralAminoAcidsImpl show =
            let fn = [ for i in 0..(p.binaryResultData.binaryInfo.aminoAcids.Length * 2 - 1) -> i ]

            let name i = 
                let idx = i / 2
                if idx * 2 = i then AminoAcid.toString idx else (AminoAcid.toString idx).ToLower()

            let tIdx = [ for i in 0..noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals p.binaryResultData.x.[t,*])

            let d t i = 
                let idx = i / 2
                if idx * 2 = i then a.[t].[idx] |> fst else a.[t].[idx] |> snd

            let getFuncData i = tIdx |> List.map (fun t -> p.binaryResultData.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = minMax)
            |> showChart show (getFileName PlotChiralAminoAcids) description


        let plotAminoAcidsImpl show =
            let fn = [ for i in 0..(p.binaryResultData.binaryInfo.aminoAcids.Length - 1) -> i ]
            let name (i : int) = (AminoAcid.toString i) + " + " + (AminoAcid.toString i).ToLower()
            let tIdx = [ for i in 0..noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals p.binaryResultData.x.[t,*])
            let d t i = (a.[t].[i] |> fst) + (a.[t].[i] |> snd)
            let getFuncData i = tIdx |> List.map (fun t -> p.binaryResultData.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = minMax)
            |> showChart show (getFileName PlotAminoAcids) description


        let plotEnantiomericExcessImpl show =
            let fn = [ for i in 0..(p.binaryResultData.binaryInfo.aminoAcids.Length - 1) -> i ]

            let name (i : int) = 
                let l = AminoAcid.toString i
                let d = l.ToLower()
                "(" + l + " - " + d + ") / (" + l + " + " + d + ")"

            let tIdx = [ for i in 0..noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals p.binaryResultData.x.[t,*])

            let d t i =
                let (l, d) = a.[t].[i]
                if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

            let getFuncData i = tIdx |> List.map (fun t -> p.binaryResultData.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = minMax)
            |> showChart show (getFileName PlotEnantiomericExcess) description


        let plotTotalSubstImpl show =
            let tIdx = [ for i in 0..noOfOutputPoints -> i ]
            let totalData = tIdx |> List.map (fun t -> p.binaryResultData.t.[t], p.getTotalSubst p.binaryResultData.x.[t,*])
            let minData = tIdx |> List.map (fun t -> p.binaryResultData.t.[t], p.binaryResultData.x.[t,*] |> Array.min)

            let foodData = Option.bind (fun i -> tIdx |> List.map (fun t -> p.binaryResultData.t.[t], p.binaryResultData.x.[t,i]) |> Some) foodIdx
            let wasteData = Option.bind (fun i -> tIdx |> List.map (fun t -> p.binaryResultData.t.[t], p.binaryResultData.x.[t,i]) |> Some) wasteIdx

            let levelData level =
                let levelSubst =
                    p.binaryResultData.binaryInfo.allSubstData.allSubst
                    |> List.filter (fun s -> s.length = level)
                    |> List.map (fun s -> p.binaryResultData.binaryInfo.allSubstData.allInd.[s])

                let xData t =
                    let d = p.binaryResultData.x.[t,*]
                    levelSubst |> List.sumBy (fun i -> (double level) * d.[i])

                tIdx |> List.map (fun t -> p.binaryResultData.t.[t], xData t)

            let charts =
                [ Chart.Line(totalData, Name = "Total") |> Some; Chart.Line(minData, Name = "Min") |> Some ]
                @ [ Option.bind (fun d -> Chart.Line(d, Name = AchiralSubst.Food.name)|> Some) foodData ]
                @ [ Option.bind (fun d -> Chart.Line(d, Name = AchiralSubst.Waste.name)|> Some) wasteData ]
                @ [ for level in 1..p.binaryResultData.binaryInfo.maxPeptideLength.length -> Chart.Line(levelData level, Name = level.ToString()) |> Some ]
                |> List.choose id


            Chart.Combine(charts)
            |> Chart.withX_AxisStyle("t", MinMax = minMax)
            |> showChart show (getFileName PlotTotalSubst) description


        //member __.plotAll (show : bool) = plotAllImpl show
        member __.plotChiralAminoAcids (show : bool) = plotChiralAminoAcidsImpl show
        member __.plotAminoAcids (show : bool) = plotAminoAcidsImpl show
        member __.plotTotalSubst (show : bool) = plotTotalSubstImpl show
        member __.plotEnantiomericExcess (show : bool) = plotEnantiomericExcessImpl show

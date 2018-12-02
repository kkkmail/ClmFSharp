namespace OdeSolvers

open System
open System.IO
open Clm.Substances
open Clm.Model
open Clm.DataLocation
open OdeSolvers.Solver
open Microsoft.FSharp.Core
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


    type ChartType = 
        | PlotAllSubst
        | PlotChiralAminoAcids
        | PlotAminoAcids
        | PlotEnantiomericExcess
        | PlotTotalSubst

        member ct.fileSuffix = 
            match ct with 
            | PlotAllSubst -> "as"
            | PlotChiralAminoAcids -> "ca"
            | PlotAminoAcids -> "aa"
            | PlotEnantiomericExcess -> "ee"
            | PlotTotalSubst -> "ts"

        member ct.getFileName (i : PlotDataInfo) (p : ModelDataParamsWithExtraData) (o : OdeResult) = 
            let suff = ct.fileSuffix

            let fileName = 
                [
                    p.modelDataParams.modelInfo.modelName
                    i.resultInfo.separator
                    (int o.y0).ToString().PadLeft(3, '0')
                    (int o.endTime).ToString().PadLeft(5, '0')
                    suff
                ]
                |> String.concat i.resultInfo.separator

            Path.Combine(i.resultInfo.resultLocation, fileName + ".html")


    type Plotter(i : PlotDataInfo, p : ModelDataParamsWithExtraData, o : OdeResult) =
        let description = 
            [
                "Comleted at", sprintf "%A" (DateTime.Now)
                "model name", p.modelDataParams.modelInfo.modelName
                "end time", sprintf "%A" o.endTime
                "y0", sprintf "%A" o.y0
                "number of amino acids", sprintf "%A" p.modelDataParams.modelInfo.numberOfAminoAcids.length
                "max peptide length", sprintf "%A" p.modelDataParams.modelInfo.maxPeptideLength.length
                "number of substances", sprintf "%A" p.modelDataParams.modelInfo.numberOfSubstances
            ]
            @
            (p.allReactions |> List.map (fun (r, c) -> r.name, c.ToString()))
            @
            (p.allRawReactions |> List.map (fun (r, c) -> r.name + " (raw)", c.ToString()))
            |> List.map (fun (n, d) -> n + ": " + d)
            |> String.concat ", "


        let showChart fileName = 
            match i.useTempFolder with 
            | true -> Chart.ShowWithDescription
            | false -> Chart.ShowFileWithDescription fileName


        let getFileName (ct : ChartType) = ct.getFileName i p o


        let plotAllImpl (r : OdeResult) =
            let fn = [ for i in 0..p.modelDataParams.modelInfo.numberOfSubstances - 1 -> i ]
            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]

            let getFuncData i = 
                tIdx
                |> List.map (fun t -> r.t.[t], r.x.[t,i])

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = i.ToString())))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> showChart (getFileName PlotAllSubst) description


        let plotChiralAminoAcidsImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.modelDataParams.modelInfo.numberOfAminoAcids.length * 2 - 1) -> i ]

            let name i = 
                let idx = i / 2
                if idx * 2 = i then AminoAcid.toString idx else (AminoAcid.toString idx).ToLower()

            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])

            let d t i = 
                let idx = i / 2
                if idx * 2 = i then a.[t].[idx] |> fst else a.[t].[idx] |> snd

            let getFuncData i = tIdx |> List.map (fun t -> r.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> showChart (getFileName PlotChiralAminoAcids) description


        let plotAminoAcidsImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.modelDataParams.modelInfo.numberOfAminoAcids.length - 1) -> i ]
            let name (i : int) = (AminoAcid.toString i) + " + " + (AminoAcid.toString i).ToLower()
            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])
            let d t i = (a.[t].[i] |> fst) + (a.[t].[i] |> snd)
            let getFuncData i = tIdx |> List.map (fun t -> r.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> showChart (getFileName PlotAminoAcids) description


        let plotEnantiomericExcessImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.modelDataParams.modelInfo.numberOfAminoAcids.length - 1) -> i ]

            let name (i : int) = 
                let l = AminoAcid.toString i
                let d = l.ToLower()
                "(" + l + " - " + d + ") / (" + l + " + " + d + ")"

            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])

            let d t i = 
                let (l, d) = a.[t].[i]
                if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

            let getFuncData i = tIdx |> List.map (fun t -> r.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> showChart (getFileName PlotEnantiomericExcess) description


        let plotTotalSubstImpl (r : OdeResult) =
            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let totalData = tIdx |> List.map (fun t -> r.t.[t], p.getTotalSubst r.x.[t,*])
            let yData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,0])
            let minData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,*] |> Array.min)

            let levelData level = 
                let levelSubst = 
                    p.allSubst
                    |> List.filter (fun s -> s.length = level)
                    |> List.map (fun s -> p.allInd.[s])

                let xData t =
                    let d = r.x.[t,*]
                    levelSubst |> List.sumBy (fun i -> (double level) * d.[i])

                tIdx |> List.map (fun t -> r.t.[t], xData t)


            Chart.Combine(
                    [ Chart.Line(totalData, Name = "Total"); Chart.Line(minData, Name = "Min"); Chart.Line(yData, Name = Substance.food.name) ]
                    @ [ for level in 1..p.modelDataParams.modelInfo.maxPeptideLength.length -> Chart.Line(levelData level, Name = level.ToString()) ]
                    )
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> showChart (getFileName PlotTotalSubst) description


        member __.plotAll() = plotAllImpl o
        member __.plotChiralAminoAcids() = plotChiralAminoAcidsImpl o
        member __.plotAminoAcids() = plotAminoAcidsImpl o
        member __.plotTotalSubst() = plotTotalSubstImpl o
        member __.plotEnantiomericExcess() = plotEnantiomericExcessImpl o

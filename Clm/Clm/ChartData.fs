namespace Clm

open Clm.Substances
open Clm.ModelParams

module ChartData =

    type TotalSubstData =
        {
            minData : double
            foodData : double option
            wasteData : double option
            levelData : list<double>
        }

    type CharTypeData =
        | ChiralAminoAcidsData
        | AminoAcidsData of list<double>
        | EnantiomericExcessData of list<double>
        | TotalSubstData of TotalSubstData


    type ChartType =
        //| PlotAllSubst
        | PlotChiralAminoAcids
        | PlotAminoAcids
        | PlotEnantiomericExcess
        | PlotTotalSubst

        member chart.data (i : BinaryInfo) x =
            match chart with
            //| PlotAllSubst ->
            //    failwith ""
            | PlotChiralAminoAcids ->
                failwith ""
            | PlotAminoAcids ->
                i.getTotals x
                |> List.map (fun (l, d) -> l + d)
                |> AminoAcidsData
            | PlotEnantiomericExcess ->
                i.getTotals x
                |> List.map (fun (l, d) -> if (l + d) > 0.0 then (l - d) / (l + d) else 0.0)
                |> EnantiomericExcessData
            | PlotTotalSubst ->
                let foodIdx = i.allSubstData.allInd.TryFind (AchiralSubst.Food |> Simple)
                let wasteIdx = i.allSubstData.allInd.TryFind (AchiralSubst.Waste |> Simple)

                let levelData level =
                    let levelSubst =
                        i.allSubstData.allSubst
                        |> List.filter (fun s -> s.length = level)
                        |> List.map (fun s -> i.allSubstData.allInd.[s])

                    levelSubst |> List.sumBy (fun i -> (double level) * x.[i])

                {
                    minData = x |> Array.min
                    foodData = Option.bind (fun i -> x.[i] |> Some) foodIdx
                    wasteData = Option.bind (fun i -> x.[i] |> Some) wasteIdx
                    levelData = [ for level in 1..p.maxPeptideLength.length -> levelData level ]
                }
                |> TotalSubstData

namespace Clm

open ClmSys.GeneralData
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


    type ChartType =
        //| PlotAllSubst
        | PlotChiralAminoAcids
        | PlotAminoAcids
        | PlotEnantiomericExcess
        | PlotTotalSubst


    type ChartSliceData =
        {
            t : double
            aminoAcidsData : list<double>
            enantiomericExcess : list<double>
            totalSubst : TotalSubstData
        }

        static member create (i : BinaryInfo) t x =
            let totals = i.getTotals x

            {
                t = t
                aminoAcidsData = totals |> List.map (fun (l, d) -> l + d)
                enantiomericExcess = totals |> List.map (fun (l, d) -> if (l + d) > 0.0 then (l - d) / (l + d) else 0.0)

                totalSubst =
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
                        levelData = [ for level in 1..i.maxPeptideLength.length -> levelData level ]
                    }
            }


    type ChartData =
        {
            todoSomeGeneralChartInfo : string
            allChartData : list<ChartSliceData>
        }

        static member defaultValue =
            {
                todoSomeGeneralChartInfo = EmptyString
                allChartData = []
            }


    type ChartDataUpdater () =
        interface IUpdater<ChartSliceData, ChartData> with
            member __.init () = ChartData.defaultValue
            member __.add a m = { m with allChartData = a :: m.allChartData }

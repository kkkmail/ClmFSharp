namespace Clm

open ClmSys.GeneralData
open Clm.Substances
open Clm.ModelParams

module ChartData =

    type TotalSubstData =
        {
            totalData : double
            minData : double
            foodData : double option
            wasteData : double option
            levelData : array<double>
        }


    type ChartType =
        //| PlotChiralAminoAcids
        | PlotAminoAcids
        | PlotEnantiomericExcess
        | PlotTotalSubst


    type ChartInitData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            binaryInfo : BinaryInfo
            y0 : decimal
            tEnd : decimal
        }


    type ChartSliceData =
        {
            t : double
            aminoAcidsData : array<double>
            enantiomericExcess : array<double>
            totalSubst : TotalSubstData
        }

        static member create (i : BinaryInfo) t x =
            let totals = i.getTotals x

            {
                t = t
                aminoAcidsData = totals |> List.map (fun (l, d) -> l + d) |> Array.ofList
                enantiomericExcess = totals |> List.map (fun (l, d) -> if (l + d) > 0.0 then (l - d) / (l + d) else 0.0) |> Array.ofList

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
                        totalData = i.getTotalSubst x
                        minData = x |> Array.min
                        foodData = Option.bind (fun i -> x.[i] |> Some) foodIdx
                        wasteData = Option.bind (fun i -> x.[i] |> Some) wasteIdx
                        levelData = [| for level in 1..i.maxPeptideLength.length -> levelData level |]
                    }
            }


    type ChartData =
        {
            initData : ChartInitData
            allChartData : list<ChartSliceData>
        }

        static member create i =
            {
                initData = i
                allChartData = []
            }

        member cd.maxEe =
            cd.allChartData
            |> List.map (fun e -> e.enantiomericExcess |> List.ofArray)
            |> List.concat
            |> List.map (fun e -> abs e)
            |> List.max

        member cd.maxAverageEe =
            match cd.allChartData with
            | [] -> 0.0
            | h :: _ ->
                let getData i = cd.allChartData |> List.map (fun e -> e.enantiomericExcess.[i])

                h.enantiomericExcess
                |> Array.mapi (fun i _ -> getData i)
                |> Array.map (fun e -> List.average e |> abs)
                |> Array.max

        member cd.maxWeightedAverageAbsEe =
            match cd.allChartData with
            | [] -> 0.0
            | h :: _ ->
                let totalWeight =
                    cd.allChartData
                    |> List.mapi(fun i _ -> double i)
                    |> List.sum

                let weigh i e = e |> Array.map (fun x -> (double i) * (abs x) / totalWeight)

                let ee =
                    cd.allChartData
                    |> List.rev
                    |> List.mapi (fun i e -> weigh i e.enantiomericExcess)

                let getData i = ee |> List.map (fun e -> e.[i])

                h.enantiomericExcess
                |> Array.mapi (fun i _ -> getData i)
                |> Array.map (fun e -> List.average e)
                |> Array.max

        member cd.maxLastEe =
            match cd.allChartData |> List.rev with
            | [] -> 0.0
            | h :: _ ->
                h.enantiomericExcess
                |> Array.map (fun e -> abs e)
                |> Array.max

    type ChartDataUpdater () =
        interface IUpdater<ChartInitData, ChartSliceData, ChartData> with
            member __.init i = ChartData.create i
            member __.add a m = { m with allChartData = a :: m.allChartData }

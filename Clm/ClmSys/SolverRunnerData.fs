namespace ClmSys

open System.IO
open GeneralPrimitives
open GeneralErrors
open ClmErrors
open ContGenPrimitives

module SolverRunnerData =

    type SingleChartInfo =
        {
            chartName : string
            chartContent : string
        }


    type ChartInfo =
        {
            resultDataId : ResultDataId
            defaultValueId : ClmDefaultValueId
            charts : list<SingleChartInfo>
        }

        static member tryCreate r d c =
            try
                {
                    resultDataId = r
                    defaultValueId = d
                    charts = c |> List.map (fun e -> { chartName = e; chartContent = File.ReadAllText e })
                }
                |> Ok
            with
            | e -> e |> CreateChartsExn |> FileErr |> Error

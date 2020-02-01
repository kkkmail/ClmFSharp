namespace ClmSys

open System.IO
open ContGenData
open WorkerNodeData
open GeneralPrimitives
open GeneralErrors
open ClmErrors
open ContGenPrimitives

module SolverRunnerData =

    type SolverRunnerAccessInfo =
        | ContGenSvcAccessInfo of ContGenServiceAccessInfo
        | WorkerNodeSvcAccessInfo of WrkNodeServiceAccessInfo

        member this.minUsefulEe =
            match this with
            | ContGenSvcAccessInfo c -> c.minUsefulEe
            | WorkerNodeSvcAccessInfo w -> w.minUsefulEe

        member this.serviceAddress =
            match this with
            | ContGenSvcAccessInfo c -> c.contGenServiceAccessInfo.serviceAddress
            | WorkerNodeSvcAccessInfo w -> w.wrkNodeServiceAccessInfo.serviceAddress

        member this.servicePort =
            match this with
            | ContGenSvcAccessInfo c -> c.contGenServiceAccessInfo.servicePort
            | WorkerNodeSvcAccessInfo w -> w.wrkNodeServiceAccessInfo.servicePort


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

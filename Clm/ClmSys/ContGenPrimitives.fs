namespace ClmSys

open System
open GeneralPrimitives
open WorkerNodePrimitives

module ContGenPrimitives =

    [<Literal>]
    let DefaultMinEe = 0.000_1


    type MinUsefulEe =
        | MinUsefulEe of double

        member this.value = let (MinUsefulEe v) = this in v
        static member defaultValue = MinUsefulEe DefaultMinEe


    type ModelDataId =
        | ModelDataId of Guid

        member this.value = let (ModelDataId v) = this in v
        static member getNewId() = Guid.NewGuid() |> ModelDataId


    type ClmDefaultValueId =
        | ClmDefaultValueId of int64

        member df.value = let (ClmDefaultValueId v) = df in v
        override df.ToString() = df.value.ToString().PadLeft(9, '0')


    type ClmTaskStatus =
        | ActiveClmTask
        | InactiveClmTask

        member s.value =
            match s with
            | ActiveClmTask -> 0
            | InactiveClmTask -> 1

        static member tryCreate i =
            match i with
            | 0 -> Some ActiveClmTask
            | 1 -> Some InactiveClmTask
            | _ -> None


    type ClmTaskId =
        | ClmTaskId of Guid

        member this.value = let (ClmTaskId v) = this in v
        static member getNewId() = Guid.NewGuid() |> ClmTaskId


    type ChartGenerationResult =
        | GeneratedCharts
        | NotGeneratedCharts


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed of ChartGenerationResult
        | Failed of WorkerNodeId * RemoteProcessId

        static member failedValue = -1000m

        static member create d =
            match d with
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> InProgress 1.0m

        member progress.value =
            match progress with
            | NotStarted -> 0m
            | InProgress d -> max 0m (min d 1m)
            | Completed _ -> 1.0m
            | Failed _ -> TaskProgress.failedValue

namespace ClmSys

open System

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

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

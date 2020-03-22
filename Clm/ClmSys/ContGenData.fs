namespace ClmSys

open System
open GeneralData
open ContGenPrimitives

module ContGenData =

    type TaskProgress
        with

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed _ -> Some DateTime.Now
            | Failed _ -> None


    type ContGenServiceAccessInfo =
        {
            contGenServiceAccessInfo : ServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }

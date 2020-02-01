namespace ClmSys

open System
open GeneralData
open ContGenPrimitives
open GeneralPrimitives
open WorkerNodePrimitives

module ContGenData =

    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed
        | Failed of WorkerNodeId * RemoteProcessId

        static member create d =
            match d with
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> InProgress 1.0m

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed -> Some DateTime.Now
            | Failed _ -> None


    type ContGenServiceAccessInfo =
        {
            contGenServiceAccessInfo : ServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }

namespace ClmSys

open System
open MessagingClientErrors

module WorkerNodeErrors =

    type OnSaveResultError =
        | LoadResultDataErr of Guid
        | SendResultMessageError of (Guid * Guid)
        | DeleteResultDataError of Guid


    type OnSaveChartsError =
        | LoadChartInfoError of Guid
        | SendChartMessageError of (Guid * Guid)
        | DeleteChartError of exn
        | DeleteChartInfoError of Guid


    type OnUpdateProgressError =
        | UnableToFindMappingError of int


    type OnRunModelError =
        | CannotSaveWorkerNodeRunModelData of string
        | CannotRunModel of string


    type OnProcessMessageError =
        | CannotSaveModelData
        | ModelAlreadyRunning of Guid
        | InvalidMessage of string



    type WorkerNodeError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
        | OnRunModelErr of OnRunModelError
        | OnProcessMessageErr of OnProcessMessageError
        | OnGetMessagesErr of OnGetMessagesError


    type WorkerNodeServiceError =
        | UnableToStartMessagingClientError
        | UnableToCreateWorkerNodeServiceError
        | ServiceUnavailable
        | UpdateLocalProgressError of string
        | ConfigureServiceError of string
        | MonitorServiceError of string


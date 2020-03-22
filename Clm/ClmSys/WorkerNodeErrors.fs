namespace ClmSys

open GeneralPrimitives
open MessagingClientErrors
open MessagingPrimitives

module WorkerNodeErrors =

    type OnSaveResultError =
        | LoadResultDataErr of ResultDataId
        | SendResultMessageError of (MessagingClientId * ResultDataId)
        | DeleteResultDataError of ResultDataId


    type OnSaveChartsError =
        | LoadChartInfoError of ResultDataId
        | SendChartMessageError of (MessagingClientId * ResultDataId)
        | DeleteChartError of (MessagingClientId * ResultDataId * exn)
        | DeleteChartInfoError of ResultDataId


    type OnUpdateProgressError =
        | UnableToFindMappingError of int


    type OnRunModelError =
        | CannotSaveWorkerNodeRunModelData of string
        | CannotRunModel of string


    type OnProcessMessageError =
        | CannotSaveModelData
        | ModelAlreadyRunning of RunQueueId
        | InvalidMessage of (MessageId * string)


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

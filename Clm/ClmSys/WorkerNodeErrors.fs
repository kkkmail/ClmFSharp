namespace ClmSys

open GeneralPrimitives
open MessagingClientErrors
open MessagingPrimitives

module WorkerNodeErrors =

    type OnSaveResultError =
        | SendResultMessageError of (MessagingClientId * ResultDataId)


    type OnSaveChartsError =
        | SendChartMessageError of (MessagingClientId * ResultDataId)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingError of RunQueueId


    type OnRunModelError =
        | CannotRunModelErr


    type OnProcessMessageError =
        | CannotSaveModelDataErr of MessageId * RunQueueId
        | OnRunModelFailedErr of MessageId * RunQueueId
        | ModelAlreadyRunningErr of MessageId * RunQueueId
        | InvalidMessageErr of (MessageId * string)
        | FailedToCancelErr of (MessageId * RunQueueId * exn)


    type WorkerNodeError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
        | OnRunModelErr of OnRunModelError
        | OnProcessMessageErr of OnProcessMessageError
        | OnGetMessagesErr of OnGetMessagesError


    type WorkerNodeServiceError =
        | UnableToStartMessagingClientErr
        | UnableToCreateWorkerNodeServiceErr
        | ServiceUnavailableErr
        | UpdateLocalProgressErr of string
        | ConfigureServiceErr of string
        | MonitorServiceErr of string

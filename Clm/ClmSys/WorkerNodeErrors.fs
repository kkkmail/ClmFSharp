﻿namespace ClmSys

open GeneralPrimitives
open MessagingClientErrors
open MessagingPrimitives

module WorkerNodeErrors =

    type OnSaveResultError =
        | SendResultMessageError of (MessagingClientId * ResultDataId)


    type OnSaveChartsError =
        | SendChartMessageError of (MessagingClientId * ResultDataId)


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

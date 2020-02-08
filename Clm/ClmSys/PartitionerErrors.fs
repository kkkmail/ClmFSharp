namespace ClmSys

open System
open MessagingClientErrors

module PartitionerErrors =

    type OnTryRunModelWithRemoteIdError =
        | UnableToGetWorkerNode of Guid
        | UnableToLoadModelData of Guid
        | UnableToSendRunModelMessage of Guid
        | TryRunModelWithRemoteIdErr of Guid
        | OnCompletedErr of Guid


    type OnProcessPartitionerMessageError =
        | ProcessedWithPartitionerErr
        | InvalidMessageTypeErr of Guid


    //type OnPartitionerRunModelError =
    //    | X

    type OnUnregisterError =
        | CannotLoadWorkerNodeInfo
        | CannotUpsertWorkerNodeInfo


    type PartitionerError =
        | OnTryRunModelWithRemoteIdErr of OnTryRunModelWithRemoteIdError
        | OnProcessPartitionerMessageErr of OnProcessPartitionerMessageError
        | OnGetMessagesPartitionerErr of OnGetMessagesError
        | OnUnregisterErr of OnUnregisterError

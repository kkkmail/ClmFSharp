namespace ClmSys

open System
open MessagingErrors

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


    type PartitionerError =
        | OnTryRunModelWithRemoteIdErr of OnTryRunModelWithRemoteIdError
        | OnProcessPartitionerMessageErr of OnProcessPartitionerMessageError
        | OnGetMessagesPartitionerErr of OnGetMessagesError

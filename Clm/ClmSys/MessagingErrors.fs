namespace ClmSys

open System
open VersionInfo
open GeneralErrors

module MessagingErrors =

    type VersionMismatchInfo =
        {
            localVersion : int
            remoteVersion : int
        }


    type GetVersionError =
        | GetVersionWcfError of WcfError
        | VersionMismatchError of VersionMismatchInfo


    type MessageDeliveryError =
        | ServiceNotStarted
        | ServerIsShuttingDown
        | DataVersionMismatch of MessagingDataVersion
        | MsgWcfError of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfError of WcfError

    type OnGetMessagesError =
        | ProcessedSucessfullyWithInnerError
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveError
        | FailedToProcessError
        | BusyProcessingError


    type TryPeekMessageError =
        | TryPeekMsgWcfError of WcfError
        | UnableToLoadMessageError of (Guid * Guid)


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfError of WcfError
        | CannotFindClientError of Guid
        | UnableToDeleteMessageError of (Guid * Guid)


    type GetStateError =
        | GetStateWcfError of WcfError


    type MessageNotFoundError =
        | MessageNotFoundError of Guid


    type MessagingServiceError =
        | GetVersionErr of GetVersionError
        | MessageDeliveryErr of MessageDeliveryError
        | ConfigureServiceErr of ConfigureServiceError
        | TryPeekMessageErr of TryPeekMessageError
        | TryDeleteFromServerErr of TryDeleteFromServerError
        | GetStateErr of GetStateError


    type MessagingClientError =
        | MessageNotFoundErr of MessageNotFoundError
        | TryProcessMessageErr of exn

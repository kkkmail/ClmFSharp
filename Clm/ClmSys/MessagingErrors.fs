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
        | GetVersionWcfErr of WcfError
        | VersionMismatchErr of VersionMismatchInfo


    type MessageDeliveryError =
        | ServiceNotStartedErr
        | ServerIsShuttingDownErr
        | DataVersionMismatchErr of MessagingDataVersion
        | MsgWcfErr of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfErr of WcfError


    type OnGetMessagesError =
        | ProcessedSucessfullyWithInnerErr
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveErr
        | FailedToProcessErr
        | BusyProcessingErr


    type TryPeekMessageError =
        | TryPeekMsgWcfErr of WcfError
        | UnableToLoadMessageError of (Guid * Guid)


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfErr of WcfError
        | CannotFindClientErr of Guid
        | UnableToDeleteMessageErr of (Guid * Guid)


    type GetStateError =
        | GetStateWcfErr of WcfError


    type MessageNotFoundError =
        | MessageNotFoundErr of Guid


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

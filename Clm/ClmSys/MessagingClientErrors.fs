namespace ClmSys

open System
open VersionInfo
open GeneralErrors
open MessagingPrimitives
open MessagingCommonErrors
open MessagingServiceErrors

module MessagingClientErrors =

    type GetVersionError =
        | GetVersionWcfErr
        | VersionMismatchErr of VersionMismatchInfo


    type OnGetMessagesError =
        | ProcessedSucessfullyWithInnerErr
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveErr
        | FailedToProcessErr
        | BusyProcessingErr


    type SendMessageError =
        | SendMessageFaileErr


    type TryReceiveSingleMessageError =
        | TryPeekMessageErr
        | SaveMessageErr
        | TryDeleteFromServerErr


    type MessageDeliveryError =
        | ServiceNotStartedErr
        | ServerIsShuttingDownErr
        | DataVersionMismatchErr of MessagingDataVersion
        | MsgWcfErr of WcfError


    type OnTryRemoveReceivedMessageError =
        | MessageNotFoundErr of MessageId


    type OnTryProcessMessageError =
        | OnTryProcessMessageExn of exn


    type MessagingClientError =
        | GetVersionErr of GetVersionError
        | SendMessageErr of SendMessageError
        //| MessageNotFoundErr of MessageId
        //| TryProcessMessageErr of exn
        | TryReceiveSingleMessageErr of TryReceiveSingleMessageError
        | MessageDeliveryErr of MessageDeliveryError
        | OnTryRemoveReceivedMessageErr of OnTryRemoveReceivedMessageError
        | OnTryProcessMessageErr of OnTryProcessMessageError

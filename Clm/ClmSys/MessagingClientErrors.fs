namespace ClmSys

open System
open VersionInfo
open GeneralErrors
open MessagingPrimitives

module MessagingClientErrors =

    type VersionMismatchInfo =
        {
            localVersion : int
            remoteVersion : int
        }


    type GetVersionError =
        | GetVersionWcfErr
        | VersionMismatchErr of VersionMismatchInfo


    type OnGetMessagesError =
        | ProcessedSucessfullyWithInnerErr
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveErr
        | FailedToProcessErr
        | BusyProcessingErr


    type TryReceiveSingleMessageError =
        | TryPeekMessageErr
        | SaveMessageErr
        | TryDeleteFromServerErr


    type MessagingClientError =
        | GetVersionErr of GetVersionError
        //| MessageNotFoundErr of MessageId
        //| TryProcessMessageErr of exn
        | TryReceiveSingleMessageErr of TryReceiveSingleMessageError


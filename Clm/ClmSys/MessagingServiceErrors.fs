﻿namespace ClmSys

open System
open VersionInfo
open GeneralErrors
open MessagingPrimitives
open MessagingCommonErrors


module MessagingServiceErrors =

    type MessageCreateError =
        | InvalidDeliveryTypeErr of int
        | InvalidDataVersionErr of VersionMismatchInfo
        | InvalidDeliveryTypeAndDataVersionErr of int * VersionMismatchInfo


    type MessageDeleteError =
        | CannotDeleteMessageErr of MessageId

    type MsgSvcDbError =
        | MessageCreateErr of MessageCreateError
        | MessageDeleteErr of MessageDeleteError


    type GetVersionSvcError =
        | GetVersionSvcWcfErr of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfErr of WcfError


    type TryPeekMessageError =
        | TryPeekMsgWcfErr of WcfError
        | UnableToLoadMessageErr of (MessagingClientId * MessageId)


    type MessageDeliveryError =
        | ServiceNotStartedErr
        | ServerIsShuttingDownErr
        | DataVersionMismatchErr of MessagingDataVersion
        | MsgWcfErr of WcfError


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfErr of WcfError
        | CannotFindClientErr of Guid
        | UnableToDeleteMessageErr of (MessagingClientId * MessageId)


    type GetStateError =
        | GetStateWcfErr of WcfError


    type MessagingServiceError =
        | MsgSvcDbErr of MsgSvcDbError
        | GetVersionSvcErr of GetVersionSvcError
        | MessageDeliveryErr of MessageDeliveryError
        | ConfigureServiceErr of ConfigureServiceError
        | TryPeekMessageErr of TryPeekMessageError
        | TryDeleteFromServerErr of TryDeleteFromServerError
        | GetStateErr of GetStateError

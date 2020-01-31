namespace ClmSys

open System
open VersionInfo
open GeneralErrors
open MessagingPrimitives

module MessagingServiceErrors =

    type GetVersionSvcError =
        | GetVersionSvcWcfErr of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfErr of WcfError


    type TryPeekMessageError =
        | TryPeekMsgWcfErr of WcfError
        | UnableToLoadMessageError of (Guid * Guid)


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfErr of WcfError
        | CannotFindClientErr of Guid
        | UnableToDeleteMessageErr of (Guid * Guid)


    type GetStateError =
        | GetStateWcfErr of WcfError


    type MessagingServiceError =
        | GetVersionSvcErr of GetVersionSvcError
        //| MessageDeliveryErr of MessageDeliveryError
        | ConfigureServiceErr of ConfigureServiceError
        | TryPeekMessageErr of TryPeekMessageError
        | TryDeleteFromServerErr of TryDeleteFromServerError
        | GetStateErr of GetStateError

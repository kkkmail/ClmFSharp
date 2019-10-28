namespace Messaging

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Logging

module ServiceResponse =

    type MsgResponseHandler private (logger : Logger, url) =
        let className = "MsgResponseHandler"
        let getMethodName n = className + "." + n
        let tryGetServiceName = getMethodName "tryGetService"


        let tryGetService() =
            try
                Activator.GetObject (typeof<IMessagingService>, url) :?> IMessagingService |> Some
            with
            | exn ->
                logger.logExn tryGetServiceName exn
                None

        member __.tryGetMessagingService() = tryGetService()

        new (i : MessagingClientAccessInfo) = MsgResponseHandler(logger, i.msgSvcAccessInfo.serviceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(logger, i.messagingServiceAccessInfo.serviceUrl)

        static member tryCreate(logger : Logger, i : MessagingClientAccessInfo) =
            try
                MsgResponseHandler(i) |> Some
            with
            | exn ->
                logger.logExn "MsgResponseHandler.tryCreate" exn
                None

        static member tryCreate(logger : Logger, i : MessagingServiceAccessInfo) =
            try
                MsgResponseHandler(i) |> Some
            with
            | exn ->
                logger.logExn "MsgResponseHandler.tryCreate" exn
                None

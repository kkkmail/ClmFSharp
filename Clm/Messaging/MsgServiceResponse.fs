namespace Messaging

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Logging

module ServiceResponse =

    type MsgResponseHandler private (url) =
        let service = Activator.GetObject (typeof<IMessagingService>, url) :?> IMessagingService

        member __.messagingService = service
        new (i : MessagingClientAccessInfo) = MsgResponseHandler(i.msgSvcAccessInfo.serviceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(i.messagingServiceAccessInfo.serviceUrl)

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

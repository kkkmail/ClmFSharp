namespace Messaging

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo

module ServiceResponse =

    type MsgResponseHandler private (url) =
        let service = Activator.GetObject (typeof<IMessagingService>, url) :?> IMessagingService

        member __.messagingService = service
        new (i : MessagingClientAccessInfo) = MsgResponseHandler(i.msgSvcAccessInfo.serviceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(i.messagingServiceAccessInfo.serviceUrl)

        static member tryCreate(i : MessagingClientAccessInfo) =
            try
                MsgResponseHandler(i) |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

        static member tryCreate(i : MessagingServiceAccessInfo) =
            try
                MsgResponseHandler(i) |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

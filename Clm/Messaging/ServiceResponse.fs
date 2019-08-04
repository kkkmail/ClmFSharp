namespace Messaging

open System
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo

module ServiceResponse =

    type ResponseHandler (i : MessagingServiceAccessInfo) =
        let service = Activator.GetObject (typeof<IClmMessagingService>, getServiceUrl i.messagingServiceAccessInfo) :?> IClmMessagingService

        member __.messagingService = service

        static member tryCreate i =
            try
                ResponseHandler i |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None

namespace Messaging

open System
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo

module ServiceResponse =

    type MsgResponseHandler<'T> (i : MessagingClientAccessInfo) =
        let service = Activator.GetObject (typeof<IMessagingService<'T>>, getServiceUrl i.msgSvcAccessInfo) :?> IMessagingService<'T>

        member __.messagingService = service

        static member tryCreate i =
            try
                MsgResponseHandler i |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None


    type ClmMsgResponseHandler (i) =
        inherit MsgResponseHandler<ClmMesage>(i)
        //IClmMessagingService

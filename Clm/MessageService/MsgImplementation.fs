namespace MessagingService

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy

module ServiceImplementation =

    let createServiceImpl (i : MessagingServiceAccessInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.defaultValue
            }

        MessagingService d


    let mutable serviceAccessInfo = getServiceAccessInfo []


    type MessagingRemoteService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IMessagingService with
            member __.sendMessage m = a.sendMessage m
            member __.getMessages n = a.getMessages n
            member __.configureService x = a.configureService x
            member __.tryPeekMessage n = a.tryPeekMessage n
            member __.tryDeleteFromServer n m = a.tryDeleteFromServer n m

namespace MessagingService

open System
open ClmSys.Logging
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
                logger = Logger.log4net
            }

        let service = MessagingService d
        do service.start()
        service


    let mutable serviceAccessInfo = getServiceAccessInfo []


    type MessagingRemoteService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IMessagingService with
            member __.getVersion() = a.getVersion()
            member __.sendMessage m = a.sendMessage m
            member __.configureService x = a.configureService x
            member __.tryPeekMessage n = a.tryPeekMessage n
            member __.tryDeleteFromServer n m = a.tryDeleteFromServer n m
            member __.getState() = a.getState()


    type MessagingWcfService() =
        let a = createServiceImpl serviceAccessInfo

        interface IMessagingWcfService with
            member __.getVersion() = a.getVersion()
            member __.sendMessage m = a.sendMessage m
            member __.configureService x = a.configureService x
            member __.tryPeekMessage n = a.tryPeekMessage n
            member __.tryDeleteFromServer n m = a.tryDeleteFromServer n m
            member __.getState() = a.getState()

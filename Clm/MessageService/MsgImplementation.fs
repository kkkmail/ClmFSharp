namespace MessagingService

open System
open ClmSys.Logging
open ClmSys.MessagingData
open ClmSys.GeneralErrors
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.Rop
open ClmSys.GeneralData
open System.ServiceModel
open ClmSys.Wcf

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
            member __.tryDeleteFromServer x = a.tryDeleteFromServer x
            member __.getState() = a.getState()


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true)>]
    type MessagingWcfService() =
        let a = createServiceImpl serviceAccessInfo

        interface IMessagingWcfService with
            member __.getVersion() = a.getVersion()
            member __.sendMessage b = tryReply a.sendMessage MsgWcfError b
            member __.configureService b = tryReply a.configureService CfgSvcWcfError b
            member __.tryPeekMessage n = tryReply a.tryPeekMessage TryPeekMsgWcfError n
            member __.tryDeleteFromServer x = tryReply a.tryDeleteFromServer TryDeleteMsgWcfError x
            member __.getState() = a.getState()

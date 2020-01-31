﻿namespace Messaging

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Wcf
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors


module ServiceResponse =

    /// Low level WCF messaging client.
    /// It seems nearly imposible to bake in "tryCommunicate tryGetWcfService" into a single function due to generics + type inference interplay.
    /// It does look very simple. However, tryCommunicate is an implicit generics with 4 generic parameters. Thread carefully...
    type MsgResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IMessagingWcfService> url

        let getVersionWcfErr e = e |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
        let msgWcfErr e = e |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
        let cfgSvcWcfErr e = e |> CfgSvcWcfErr |> ConfigureServiceErr |> MessagingServiceErr
        let tryPeekMsgWcfErr e = e |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
        let tryDeleteMsgWcfErr e = e |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr
        let getStateWcfErr e = e |> GetStateWcfErr |> GetStateErr |> MessagingServiceErr

        let getVersionImpl() = tryCommunicate tryGetWcfService (fun service -> service.getVersion) getVersionWcfErr ()
        let sendMessageImpl m = tryCommunicate tryGetWcfService (fun service -> service.sendMessage) msgWcfErr m
        let configureServiceImpl x = tryCommunicate tryGetWcfService (fun service -> service.configureService) cfgSvcWcfErr x
        let tryPeekMessageImpl n = tryCommunicate tryGetWcfService (fun service -> service.tryPeekMessage) tryPeekMsgWcfErr n
        let tryDeleteFromServerImpl x = tryCommunicate tryGetWcfService (fun service -> service.tryDeleteFromServer) tryDeleteMsgWcfErr x
        let getStateImpl() = tryCommunicate tryGetWcfService (fun service -> service.getState) getStateWcfErr ()

        interface IMessagingService with
            member __.getVersion() = getVersionImpl()
            member __.sendMessage m = sendMessageImpl m
            member __.configureService x = configureServiceImpl x
            member __.tryPeekMessage n = tryPeekMessageImpl n
            member __.tryDeleteFromServer x = tryDeleteFromServerImpl x
            member __.getState() = getStateImpl()

        new (i : MessagingClientAccessInfo) = MsgResponseHandler(i.msgSvcAccessInfo.wcfServiceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(i.messagingServiceAccessInfo.wcfServiceUrl)

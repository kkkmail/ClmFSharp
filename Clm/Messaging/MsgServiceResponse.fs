namespace Messaging

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Wcf


module ServiceResponse =

    /// Low leverl WCF messaging client.
    /// It seems imposible to bake in "tryCommunicate tryGetWcfService" into a function due to generics + type inference interplay.
    type MsgResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IMessagingWcfService> url

        let getVersionImpl() = tryCommunicate tryGetWcfService (fun service -> service.getVersion) GetVersionWcfError ()
        let sendMessageImpl m = tryCommunicate tryGetWcfService (fun service -> service.sendMessage) MsgWcfError m
        let configureServiceImpl x = tryCommunicate tryGetWcfService (fun service -> service.configureService) CfgSvcWcfError x
        let tryPeekMessageImpl n = tryCommunicate tryGetWcfService (fun service -> service.tryPeekMessage) TryPeekMsgWcfError n
        let tryDeleteFromServerImpl x = tryCommunicate tryGetWcfService (fun service -> service.tryDeleteFromServer) TryDeleteMsgWcfError x
        let getStateImpl() = tryCommunicate tryGetWcfService (fun service -> service.getState) GetStateWcfError ()

        interface IMessagingService with
            member __.getVersion() = getVersionImpl()
            member __.sendMessage m = sendMessageImpl m
            member __.configureService x = configureServiceImpl x
            member __.tryPeekMessage n = tryPeekMessageImpl n
            member __.tryDeleteFromServer x = tryDeleteFromServerImpl x
            member __.getState() = getStateImpl()

        new (i : MessagingClientAccessInfo) = MsgResponseHandler(i.msgSvcAccessInfo.wcfServiceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(i.messagingServiceAccessInfo.wcfServiceUrl)

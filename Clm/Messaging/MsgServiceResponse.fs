namespace Messaging

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Logging
open ClmSys.Rop
open System.ServiceModel
open System.ServiceModel.Description
open System.Runtime.Remoting.Channels.Tcp
open System.Runtime.Remoting.Channels
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open ClmSys.VersionInfo
open ClmSys.Wcf


module ServiceResponse =

    /// It seems imposible to bake in "tryCommunicate tryGetWcfService" into a function due to generics + type inference interplay.
    type MsgWcfClient (url) =
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


        let tryGetWcfService() =
            try
                let server = MsgWcfClient url
                Some (server :> IMessagingService)
            with
            | exn ->
                logger.logExn tryGetServiceName exn
                None


        member __.tryGetMessagingService() = tryGetWcfService()

        //new (i : MessagingClientAccessInfo) = MsgResponseHandler(logger, i.msgSvcAccessInfo.serviceUrl)
        //new (i : MessagingServiceAccessInfo) = MsgResponseHandler(logger, i.messagingServiceAccessInfo.serviceUrl)
        new (i : MessagingClientAccessInfo) = MsgResponseHandler(logger, i.msgSvcAccessInfo.wcfServiceUrl)
        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(logger, i.messagingServiceAccessInfo.wcfServiceUrl)

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

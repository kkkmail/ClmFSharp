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

    type MsgWcfClient (url) =
        let className = "MsgWcfClient"
        let getMethodName n = className + "." + n
        //let getWcfServiceName = getMethodName "getWcfService"
        let tryGetWcfServiceName = getMethodName "tryGetWcfService"
        let sendMessageImplName = getMethodName "sendMessageImpl"

        let tryGetWcfService() = tryGetWcfService<IMessagingWcfService> url

        let getVersionImpl() = messagingDataVersion

        let sendMessageImpl m = tryCommunicate tryGetWcfService (fun service -> service.sendMessage) MsgWcfError m


        let configureServiceImpl x = failwith "configureServiceImpl is not implemented."
        let tryPeekMessageImpl n = None
        let tryDeleteFromServerImpl n m = false
        let getStateImpl() = failwith "getStateImpl is not implemented."

        interface IMessagingService with
            member __.getVersion() = getVersionImpl()
            member __.sendMessage m = sendMessageImpl m
            member __.configureService x = configureServiceImpl x
            member __.tryPeekMessage n = tryPeekMessageImpl n
            member __.tryDeleteFromServer n m = tryDeleteFromServerImpl n m
            member __.getState() = getStateImpl()


    type MsgResponseHandler private (logger : Logger, url) =
        let className = "MsgResponseHandler"
        let getMethodName n = className + "." + n
        let tryGetServiceName = getMethodName "tryGetService"


        let tryGetService() =
            try
                //let channel = new TcpChannel()
                //do ChannelServices.RegisterChannel(channel, true)
                Activator.GetObject (typeof<IMessagingService>, url) :?> IMessagingService |> Some
            with
            | exn ->
                logger.logExn tryGetServiceName exn
                None


        let tryGetWcfService() =
            try
                //printfn "tryGetWcfService: Creating WCF client..."
                //let binding = new NetTcpBinding()
                //let address = new EndpointAddress(url)
                //let channelFactory = new ChannelFactory<IMessagingWcfService>(binding, address)
                //let server1 = channelFactory.CreateChannel()
                //let x = server1 :?> IClientChannel
                //printfn "... completed."
                //Some server

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

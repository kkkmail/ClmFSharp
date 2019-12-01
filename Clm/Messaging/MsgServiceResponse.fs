namespace Messaging

open System
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.Logging
open System.ServiceModel
open System.ServiceModel.Description
open System.Runtime.Remoting.Channels.Tcp
open System.Runtime.Remoting.Channels


module ServiceResponse =

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
                printfn "tryGetWcfService: Creating WCF client..."
                let binding = new NetTcpBinding()
                let address = new EndpointAddress(url)
                let channelFactory = new ChannelFactory<IMessagingWcfService>(binding, address)
                let server = channelFactory.CreateChannel()
                printfn "... completed."
                Some server
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

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

    let tryCommunicate<'A, 'B> t (c : WcfCommunicator) (a : 'A) : Result<'B, WcfError> =
        let communicate service =
            match a |> trySerialize with
            | Ok b ->
                c service b
                |> tryDeserialize<'B>
                |> Result.mapError WcfSerializationError
            | Error e -> e |> WcfSerializationError |> Error

        try
            t() |> Result.bind communicate
        with
        | e -> e |> WcfException |> Error


    type MsgWcfClient (url) =
        let className = "MsgWcfClient"
        let getMethodName n = className + "." + n
        //let getWcfServiceName = getMethodName "getWcfService"
        let tryGetWcfServiceName = getMethodName "tryGetWcfService"
        let sendMessageImplName = getMethodName "sendMessageImpl"

        //let getWcfService() =
        //    printfn "%s: Creating WCF client..." getWcfServiceName
        //    let binding = new NetTcpBinding()
        //    let address = new EndpointAddress(url)
        //    let channelFactory = new ChannelFactory<IMessagingWcfService>(binding, address)
        //    let server = channelFactory.CreateChannel()
        //    printfn "%s: Completed." getWcfServiceName
        //    server

        let tryGetWcfService() =
            try
                printfn "%s: Creating WCF client..." tryGetWcfServiceName
                let binding = new NetTcpBinding()
                let address = new EndpointAddress(url)
                let channelFactory = new ChannelFactory<IMessagingWcfService>(binding, address)
                let server = channelFactory.CreateChannel()
                printfn "%s: Completed." tryGetWcfServiceName
                Ok server
            with
            | e -> e |> WcfException |> Error

        let getVersionImpl() = messagingDataVersion

        let sendMessageImpl m =
            let r = tryCommunicate<Message, MessageDeliveryResult> tryGetWcfService (fun service -> service.sendMessage) m

            match r with
            | Ok v -> v
            | Error e -> e |> WcfError |> Error


            //printfn "%s: Starting..." sendMessageImplName

            //try
            //    match tryGetWcfService() with
            //    | Ok service ->
            //        printfn "%s: Created service..." sendMessageImplName
            //        match m |> trySerialize with
            //        | Ok b ->
            //            printfn "%s: trySerialize succeeded..." sendMessageImplName
            //            let r = b |> service.sendMessage
            //            printfn "%s: r = %A" sendMessageImplName r
            //            let d = r |> tryDeserialize<MessageDeliveryResult>
            //            printfn "%s: d = %A" sendMessageImplName d
            //            match d with
            //            | Ok result ->
            //                printfn "%s: service.sendMessage succeeded. Result: %A." sendMessageImplName r
            //                result
            //            | Error e ->
            //                printfn "%s: service.sendMessage FAILED. Exception: %A" sendMessageImplName e
            //                ExceptionOccurred e
            //        | Error e ->
            //            printfn "%s: trySerialize FAILED. Exception: %A" sendMessageImplName e
            //            ExceptionOccurred e
            //    | Error e ->
            //        printfn "%s: FAILED to create service. Exception: %A" sendMessageImplName e
            //        ExceptionOccurred e
            //with
            //| e ->
            //    printfn "%s: Exception occured: %A" sendMessageImplName e
            //    ExceptionOccurred e

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
                //let server = channelFactory.CreateChannel()
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

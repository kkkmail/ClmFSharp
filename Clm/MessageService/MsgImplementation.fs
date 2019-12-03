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


    let tryReply p f a =
        let reply =
            match a |> tryDeserialize with
            | Ok m -> p m
            | Error e -> f e

        match reply |> trySerialize with
        | Ok r -> r
        | Error _ -> [||]


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true)>]
    type MessagingWcfService() =
        let className = "MessagingWcfService"
        let getMethodName n = className + "." + n
        let sendMessageImplName = getMethodName "sendMessageImpl"

        let a = createServiceImpl serviceAccessInfo

        let sendMessageImpl b =
            printfn "%s: Starting..." sendMessageImplName
            tryReply a.sendMessage (fun e -> e |> WcfSerializationError |> WcfError |> Error) b

            //let reply =
            //    match b |> tryDeserialize<Message> with
            //    | Ok m ->
            //        printfn "%s: Got message with id: %A" sendMessageImplName m.messageDataInfo.messageId
            //        a.sendMessage m
            //    | Error e ->
            //        printfn "%s: Failed to get message with exception: %A" sendMessageImplName e
            //        e |> WcfSerializationError |> WcfError |> Error

            //printfn "%s: reply = %A" sendMessageImplName reply

            //match reply |> trySerialize with
            //| Ok r ->
            //    printfn "%s: r = %A" sendMessageImplName r
            //    r
            //| Error f ->
            //    printfn "%s: f = %A" sendMessageImplName f
            //    [||]

        interface IMessagingWcfService with
            member __.getVersion() = a.getVersion()
            member __.sendMessage m = sendMessageImpl m
            member __.configureService x = a.configureService x
            member __.tryPeekMessage n = a.tryPeekMessage n
            member __.tryDeleteFromServer x = a.tryDeleteFromServer (fst x) (snd x)
            member __.getState() = a.getState()

            member __.testMethod name =
                printfn "MessagingWcfService.testMethod: %A" name
                "WCF: " + name + " - " + (DateTime.Now.ToString())

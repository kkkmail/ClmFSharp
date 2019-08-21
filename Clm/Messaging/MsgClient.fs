﻿namespace Messaging

open System
open ClmSys.MessagingData
open ClmSys.Logging
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.ServiceResponse

module Client =

    type MessagingClientData =
        {
            msgAccessInfo : MessagingClientAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }


    type MessagingClientState =
        {
            messageClientData : MessagingClientData
            incomingMessages : List<Message>
            outgoingMessages : List<Message>
        }

        /// kk:20190726 - Removing d makes F# compiler fail on type MessagingClient<'T> with:
        /// "This code is not sufficiently generic. The type variable 'T could not be generalized because it would escape its scope.". WTF!!!
        static member defaultValue d =
            {
                messageClientData = d
                incomingMessages = []
                outgoingMessages = []
            }


    type MessagingClientMessage =
        | Start
        | SendMessage of MessageInfo
        | GetMessages of AsyncReplyChannel<List<Message>>
        | TransmitMessages
        | ConfigureClient of MessagingClientConfigParam


    type MessagingClient(d : MessagingClientData) =
        let onStart s =
            let messages = s.messageClientData.msgClientProxy.loadMessages()
            let incoming = messages |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
            let outgoing = messages |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)
            { s with outgoingMessages = s.outgoingMessages @ outgoing; incomingMessages = s.incomingMessages @ incoming }


        let onSendMessage s m =
            let message =
                {
                    messageId = MessageId.create()
                    sender = d.msgAccessInfo.msgClientId
                    messageInfo = m
                    createdOn = DateTime.Now
                }

            match m.deliveryType with
            | GuaranteedDelivery -> s.messageClientData.msgClientProxy.saveMessage { messageType = OutgoingMessage; message = message }
            | NonGuaranteedDelivery -> ignore()

            { s with outgoingMessages = message :: s.outgoingMessages }


        let onGetMessages s (r : AsyncReplyChannel<List<Message>>) =
            r.Reply s.incomingMessages

            s.incomingMessages
            |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            |> List.map (fun e -> s.messageClientData.msgClientProxy.deleteMessage e.messageId)
            |> ignore

            { s with incomingMessages = [] }


        let sendMessageImpl s m =
            try
                s.messageClientData.msgResponseHandler.messagingService.sendMessage m

                match m.messageInfo.deliveryType with
                | GuaranteedDelivery -> s.messageClientData.msgClientProxy.deleteMessage m.messageId
                | NonGuaranteedDelivery -> ignore()
                Some m
            with
                | e ->
                    s.messageClientData.logger.logExn (sprintf "Failed to send message: %A" m.messageId) e
                    None


        let receiveMessagesImpl s =
            try
                s.messageClientData.msgResponseHandler.messagingService.getMessages s.messageClientData.msgAccessInfo.msgClientId
            with
                | e ->
                    s.messageClientData.logger.logExn "Failed to receive messages: " e
                    []


        let onTransmitMessages s =
            let result =
                s.outgoingMessages
                |> List.rev
                |> List.map (sendMessageImpl s)
                |> List.choose id

            let transmitted = result |> List.map (fun e -> e.messageId) |> Set.ofList
            let received = receiveMessagesImpl s
            let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageId) |> Set.ofList
            let failed = Set.difference outgoing transmitted
            let remaining = s.outgoingMessages |> List.filter (fun e -> failed.Contains e.messageId)
            { s with outgoingMessages = remaining; incomingMessages = received @ s.incomingMessages }


        let onConfigureClient s x =
            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages r -> return! onGetMessages s r |> loop
                            | TransmitMessages -> return! onTransmitMessages s |> loop
                            | ConfigureClient x -> return! onConfigureClient s x |> loop
                        }

                onStart (MessagingClientState.defaultValue d) |> loop
                )


        let eventHandler _ =
            printfn "Transmitting messages..."
            TransmitMessages |> messageLoop.Post

        let timer = new System.Timers.Timer(30_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()


        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages reply)
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member __.transmitMessages() = TransmitMessages |> messageLoop.Post
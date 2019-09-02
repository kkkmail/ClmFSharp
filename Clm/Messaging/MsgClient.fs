namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.Rop
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.ServiceResponse
open ClmSys.TimerEvents

module Client =

    /// Maximum number of messages to process in one go.
    let maxNumberOfMessages = 1000

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

        member s.msgClientId = s.messageClientData.msgAccessInfo.msgClientId
        member s.service = s.messageClientData.msgResponseHandler.messagingService
        member s.proxy = s.messageClientData.msgClientProxy
        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]


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
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        //| GetMessages of AsyncReplyChannel<List<Message>>
        | TransmitMessages
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>


    and MessagingClient(d : MessagingClientData) =
        let logger = d.logger
        let logErr = d.logger.logErr

        let onStart s =
            let messages = s.messageClientData.msgClientProxy.loadMessages()
            let incoming = messages |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
            let outgoing = messages |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)
            { s with outgoingMessages = s.outgoingMessages @ outgoing; incomingMessages = s.incomingMessages @ incoming }


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "MessagingService.onGetVersion"
            r.Reply messagingDataVersion
            s


        let onSendMessage (s : MessagingClientState) m =
            printfn "MessagingClient.onSendMessage..."
            let message =
                {
                    messageId = MessageId.create()
                    sender = d.msgAccessInfo.msgClientId
                    messageInfo = m
                    createdOn = DateTime.Now
                }

            match m.deliveryType with
            | GuaranteedDelivery -> s.proxy.saveMessage { messageType = OutgoingMessage; message = message }
            | NonGuaranteedDelivery -> ignore()

            { s with outgoingMessages = message :: s.outgoingMessages }


        //let onGetMessages s (r : AsyncReplyChannel<List<Message>>) =
        //    printfn "MessagingClient.onGetMessages..."
        //    r.Reply s.incomingMessages
        //
        //    s.incomingMessages
        //    |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
        //    |> List.map (fun e -> s.proxy.deleteMessage e.messageId)
        //    |> ignore
        //
        //    { s with incomingMessages = [] }


        let sendMessageImpl (s : MessagingClientState) m =
            printfn "MessagingClient.sendMessageImpl..."
            try
                match s.service.sendMessage m with
                | Success _ ->
                    match m.messageInfo.deliveryType with
                    | GuaranteedDelivery -> s.proxy.deleteMessage m.messageId
                    | NonGuaranteedDelivery -> ignore()
                    Some m
                | Failure e ->
                    logErr e
                    None
            with
                | e ->
                    s.messageClientData.logger.logExn (sprintf "Failed to send message: %A" m.messageId) e
                    None


        let tryReceiveSingleMessage (s : MessagingClientState) =
            printfn "MessagingClient.tryReceiveSingleMessage..."
            match s.service.tryPeekMessage s.msgClientId with
            | Some m ->
                printfn "MessagingClient.tryReceiveSingleMessage: Received message with id: %A" m.messageId
                match m.messageInfo.deliveryType with
                | GuaranteedDelivery ->
                    {
                        message = m
                        messageType = IncomingMessage
                    }
                    |> d.msgClientProxy.saveMessage
                | NonGuaranteedDelivery -> ignore()

                match s.service.tryDeleteFromServer s.msgClientId m.messageId with
                | true ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Deleted message from server. Message id: %A" m.messageId
                    ignore()
                | false ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Cannot delete message from server. Message id: %A" m.messageId
                    logErr (sprintf "tryReceiveSingleMessage: Unable to delete a message from server for client: %A, message id: %A." s.msgClientId m.messageId)
                Some m
            | None ->
                printfn "MessagingClient.tryReceiveSingleMessage: Did not receive a message."
                None


        let receiveMessagesImpl (s : MessagingClientState) =
            printfn "MessagingClient.receiveMessagesImpl..."
            try
                let serverVersion = s.service.getVersion()

                match serverVersion = messagingDataVersion with
                | true ->
                    MessagingClientState.maxMessages
                    |> List.mapWhileSome (fun _ -> tryReceiveSingleMessage s)
                | false ->
                    s.messageClientData.logger.logErr (sprintf "MessagingClient.receiveMessagesImpl - different data versions - client: %A, server: %A" messagingDataVersion.value serverVersion.value)
                    []
            with
                | e ->
                    s.messageClientData.logger.logExn "MessagingClient.receiveMessagesImpl: Failed to receive messages: " e
                    []


        let onTransmitMessages s =
            printfn "MessagingClient.onTransmitMessages..."
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
            let x = { s with outgoingMessages = remaining; incomingMessages = received @ s.incomingMessages }
            printfn "MessagingClient.onTransmitMessages: incomingMessages.Length = %A, outgoingMessages.Length = %A" x.incomingMessages.Length x.outgoingMessages.Length
            x


        let onConfigureClient s x =
            printfn "MessagingClient.onConfigureClient..."
            s


        let onTryPeekReceivedMessage (s : MessagingClientState) (r : AsyncReplyChannel<Message option>) =
            printfn "MessagingClient.onTryPeekReceivedMessage..."
            s.incomingMessages |> List.tryHead |> r.Reply
            s


        let onTryRemoveReceivedMessage (s : MessagingClientState) m (r : AsyncReplyChannel<bool>) =
            printfn "MessagingClient.onTryRemoveReceivedMessage..."
            s.proxy.deleteMessage m
            r.Reply true
            { s with incomingMessages = removeFirst (fun e -> e.messageId = m) s.incomingMessages }


        let onTryProcessMessage (w : MessagingClient) x f =
            printfn "MessagingClient.tryProcessMessageImpl - starting..."
            match w.tryPeekReceivedMessage() with
            | Some m ->
                try
                    printfn "    MessagingClient.tryProcessMessageImpl: calling f m, messageId: %A" m.messageId
                    let r = f x m
                    printfn "    MessagingClient.tryProcessMessageImpl: calling tryRemoveReceivedMessage, messageId: %A" m.messageId

                    match w.tryRemoveReceivedMessage m.messageId with
                    | true -> printfn "    MessagingClient.tryProcessMessageImpl: Successfully removed message with id: %A" m.messageId
                    | false -> printfn "    MessagingClient.tryProcessMessageImpl: !!! ERROR !!! removing message with id: %A" m.messageId

                    printfn "    MessagingClient.tryProcessMessageImpl - completed."
                    Some r
                with
                | ex ->
                    logger.logExn "tryProcessMessageImpl" ex
                    None
            | None -> None


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | GetVersion r -> return! onGetVersion s r |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            //| GetMessages r -> return! onGetMessages s r |> loop
                            | TransmitMessages -> return! onTransmitMessages s |> loop
                            | ConfigureClient x -> return! onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! onTryRemoveReceivedMessage s m r |> loop
                        }

                onStart (MessagingClientState.defaultValue d) |> loop
                )


        let eventHandler _ =
            printfn "MessagingClient: Transmitting messages..."
            TransmitMessages |> messageLoop.Post


        let h = new EventHandler(EventHandlerInfo.defaultValue eventHandler)
        do h.start()


        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage m = SendMessage m |> messageLoop.Post
        //member __.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages reply)
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member __.transmitMessages() = TransmitMessages |> messageLoop.Post
        member __.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member __.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member this.tryProcessMessage s f = onTryProcessMessage this s f

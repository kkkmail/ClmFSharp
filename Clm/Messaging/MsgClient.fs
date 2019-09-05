namespace Messaging

open System
open ClmSys.VersionInfo
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
        | MsgCliNotStarted
        | MsgCliIdle
        | MsgCliTransmitting


    type TransmissionData =
        {
            receivedMessages : List<Message>
            sentMessages : List<Message>
        }


    type MessagingClientStateData =
        {
            messagingClientState : MessagingClientState
            messageClientData : MessagingClientData
            incomingMessages : List<Message>
            outgoingMessages : List<Message>
        }

        member s.msgClientId = s.messageClientData.msgAccessInfo.msgClientId
        member s.service = s.messageClientData.msgResponseHandler.messagingService
        member s.proxy = s.messageClientData.msgClientProxy

        member s.logErr = s.messageClientData.logger.logErr
        member s.logExn = s.messageClientData.logger.logExn
        member s.logInfo = s.messageClientData.logger.logInfo

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue d =
            {
                messagingClientState = MsgCliNotStarted
                messageClientData = d
                incomingMessages = []
                outgoingMessages = []
            }


    /// Outgoing messages are stored with the newest at the head.
    let sortOutgoing m = m |> List.sortByDescending (fun e -> e.createdOn)


    /// Incoming messages are stored with the oldest at the head.
    let sortIncoming m = m |> List.sortBy (fun e -> e.createdOn)


    let onSendMessage (s : MessagingClientStateData) m =
        printfn "MessagingClient.onSendMessage..."
        let message =
            {
                messageId = MessageId.create()
                dataVersion = messagingDataVersion
                sender = s.messageClientData.msgAccessInfo.msgClientId
                messageInfo = m
                createdOn = DateTime.Now
            }

        match m.deliveryType with
        | GuaranteedDelivery -> s.proxy.saveMessage { messageType = OutgoingMessage; message = message }
        | NonGuaranteedDelivery -> ignore()

        { s with outgoingMessages = (message :: s.outgoingMessages) |> sortOutgoing }


    let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
        printfn "MessagingClient.onGetVersion"
        r.Reply messagingDataVersion
        s


    let sendMessageImpl (s : MessagingClientStateData) m =
        printfn "MessagingClient.sendMessageImpl, messageId = %A, createdOn = %A" m.messageId.value m.createdOn
        try
            match s.service.sendMessage m with
            | DeliveredSuccessfully _ ->
                match m.messageInfo.deliveryType with
                | GuaranteedDelivery -> s.proxy.deleteMessage m.messageId
                | NonGuaranteedDelivery -> ignore()
                Some m
            | DataVersionMismatch v ->
                s.logErr (sprintf "MessagingClient.sendMessageImpl: messageId = %A, data version mismatch server has: %A but client has: %A." m.messageId.value v messagingDataVersion)
                None
            | ServerIsShuttingDown ->
                s.logInfo (sprintf "MessagingClient.sendMessageImpl: messageId = %A - server is shutting down." m.messageId.value)
                None
            | ExceptionOccurred e ->
                s.logExn (sprintf "MessagingClient.sendMessageImpl: messageId = %A - exception occurred." m.messageId.value) e
                None
        with
            | e ->
                s.messageClientData.logger.logExn (sprintf "MessagingClient.sendMessageImpl:Failed to send message: %A" m.messageId) e
                None


    let tryReceiveSingleMessage (s : MessagingClientStateData) =
        async {
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
                    |> s.proxy.saveMessage
                | NonGuaranteedDelivery -> ignore()

                match s.service.tryDeleteFromServer s.msgClientId m.messageId with
                | true ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Deleted message from server. Message id: %A" m.messageId
                    ignore()
                | false ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Cannot delete message from server. Message id: %A" m.messageId
                    s.logErr (sprintf "tryReceiveSingleMessage: Unable to delete a message from server for client: %A, message id: %A." s.msgClientId m.messageId)
                return Some m
            | None ->
                printfn "MessagingClient.tryReceiveSingleMessage: Did not receive a message."
                return None
        }


    let receiveMessagesImpl (s : MessagingClientStateData) =
        async {
            printfn "MessagingClient.receiveMessagesImpl..."
            try
                let serverVersion = s.service.getVersion()

                match serverVersion = messagingDataVersion with
                | true ->
                    return! MessagingClientStateData.maxMessages |> List.mapWhileSomeAsync (fun _ -> tryReceiveSingleMessage s)
                | false ->
                    s.logErr (sprintf "MessagingClient.receiveMessagesImpl - different data versions - client: %A, server: %A" messagingDataVersion.value serverVersion.value)
                    return []
            with
                | e ->
                    s.logExn "MessagingClient.receiveMessagesImpl: Failed to receive messages: " e
                    return []
            }


    let onFinishTransmitting (s : MessagingClientStateData) (t : TransmissionData) =
        printfn "MessagingClient.onFinishTransmitting..."
        let received = t.receivedMessages
        let sent = t.sentMessages |> List.map (fun e -> e.messageId)

        let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageId) |> Set.ofList
        let notSent = Set.difference outgoing (sent |> Set.ofList)
        let remaining = s.outgoingMessages |> List.filter (fun e -> notSent.Contains e.messageId) |> sortOutgoing
        let x = { s with messagingClientState = MsgCliIdle; outgoingMessages = remaining; incomingMessages = (s.incomingMessages @ received) |> sortIncoming }
        printfn "MessagingClient.onFinishTransmitting: incomingMessages.Length = %A, outgoingMessages.Length = %A" x.incomingMessages.Length x.outgoingMessages.Length
        x


    let onConfigureClient s x =
        printfn "MessagingClient.onConfigureClient..."
        s


    let onTryPeekReceivedMessage (s : MessagingClientStateData) (r : AsyncReplyChannel<Message option>) =
        printfn "MessagingClient.onTryPeekReceivedMessage..."
        s.incomingMessages |> List.tryHead |> r.Reply
        s


    let onTryRemoveReceivedMessage (s : MessagingClientStateData) m (r : AsyncReplyChannel<bool>) =
        printfn "MessagingClient.onTryRemoveReceivedMessage..."
        s.proxy.deleteMessage m
        r.Reply true
        { s with incomingMessages = s.incomingMessages |> List.filter (fun e -> e.messageId <> m) |> sortIncoming }


    type MessagingClientMessage =
        | Start of MessagingClient
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        | StartTransmitting of MessagingClient
        | FinishTransmitting of TransmissionData
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>


    and MessagingClient(d : MessagingClientData) =
        let onStart (s : MessagingClientStateData) (w : MessagingClient) =
            match s.messagingClientState with
            | MsgCliNotStarted ->
                let messages = s.proxy.loadMessages()
                let incoming = messages |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
                let outgoing = messages |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)

                let eventHandler _ =
                    printfn "MessagingClient: Transmitting messages..."
                    w.startTransmitting()

                let h = new EventHandler(EventHandlerInfo.defaultValue eventHandler)
                do h.start()

                {
                    s
                    with
                        messagingClientState = MsgCliIdle
                        outgoingMessages = (s.outgoingMessages @ outgoing) |> sortOutgoing
                        incomingMessages = (s.incomingMessages @ incoming) |> sortIncoming
                }
            | MsgCliIdle | MsgCliTransmitting-> s


        let onStartTransmitting s (w : MessagingClient) =
            match s.messagingClientState with
            | MsgCliNotStarted -> s
            | MsgCliIdle ->
                async {
                    printfn "MessagingClient.onStartTransmitting..."
                    let sentMessages =
                        // Note that we need to apply List.rev to get to the first (the oldest) message in the outgoing queue.
                        s.outgoingMessages
                        |> List.rev
                        |> List.map (sendMessageImpl s)
                        |> List.choose id
                    let! received = receiveMessagesImpl s

                    {
                        receivedMessages = received
                        sentMessages = sentMessages
                    }
                    |> w.finishTransmitting
                }
                |> Async.Start

                { s with messagingClientState = MsgCliTransmitting }
            | MsgCliTransmitting -> s


        let onTryProcessMessage (w : MessagingClient) x f =
            async {
                printfn "MessagingClient.tryProcessMessageImpl - starting..."
                match! w.tryPeekReceivedMessage() with
                | Some m ->
                    try
                        printfn "    MessagingClient.tryProcessMessageImpl: calling f m, messageId: %A, createdOn: %A" m.messageId m.createdOn
                        let r = f x m
                        printfn "    MessagingClient.tryProcessMessageImpl: calling tryRemoveReceivedMessage, messageId: %A, createdOn: %A" m.messageId m.createdOn

                        match! w.tryRemoveReceivedMessage m.messageId with
                        | true -> printfn "    MessagingClient.tryProcessMessageImpl: Successfully removed messageId: %A, createdOn: %A" m.messageId m.createdOn
                        | false -> printfn "    MessagingClient.tryProcessMessageImpl: !!! ERROR !!! removing messageId: %A, createdOn: %A" m.messageId m.createdOn

                        printfn "    MessagingClient.tryProcessMessageImpl - completed."
                        return Some r
                    with
                    | ex ->
                        logger.logExn "MessagingClient.tryProcessMessageImpl" ex
                        return None
                | None -> return None
            }


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! timed "MessagingClient.onStart" onStart s w |> loop
                            | GetVersion r -> return! timed "MessagingClient.onGetVersion" onGetVersion s r |> loop
                            | SendMessage m -> return! timed "MessagingClient.onSendMessage" onSendMessage s m |> loop
                            | StartTransmitting w -> return! timed "MessagingClient.onTransmitMessages" onStartTransmitting s w |> loop
                            | FinishTransmitting d -> return! onFinishTransmitting s d |> loop
                            | ConfigureClient x -> return! timed "MessagingClient.onConfigureClient" onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! timed "MessagingClient.onTryPeekReceivedMessage" onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! timed "MessagingClient.onTryRemoveReceivedMessage (using timed)" onTryRemoveReceivedMessage s m r |> loop
                        }

                MessagingClientStateData.defaultValue d |> loop
                )


        member this.start() = Start this |> messageLoop.Post
        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member this.startTransmitting() = StartTransmitting this |> messageLoop.Post
        member __.finishTransmitting d = FinishTransmitting d |> messageLoop.Post
        member private __.tryPeekReceivedMessage() = messageLoop.PostAndAsyncReply (fun reply -> TryPeekReceivedMessage reply)
        member private __.tryRemoveReceivedMessage m = messageLoop.PostAndAsyncReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member this.tryProcessMessage s f = onTryProcessMessage this s f

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
    let maxNumberOfMessages = 1_000

    let maxNumberOfSmallMessages = 1_000
    let maxNumberOfMediumMessages = 20
    let maxNumberOfLargeMessages = 2

    type MessageCount =
        {
            smallMessages : int
            mediumMessages : int
            largeMessages : int
        }

        with
        static member defaultValue =
            {
                smallMessages = 0
                mediumMessages = 0
                largeMessages = 0
            }

        static member maxAllowed =
            {
                smallMessages = maxNumberOfSmallMessages
                mediumMessages = maxNumberOfMediumMessages
                largeMessages = maxNumberOfMediumMessages
            }

        member t.canProcess =
            let m = MessageCount.maxAllowed

            if t.smallMessages < m.smallMessages && t.mediumMessages < m.mediumMessages && t.largeMessages < m.largeMessages then true
            else false

        member t.onSmallMessage() = { t with smallMessages  = t.smallMessages + 1 }
        member t.onMediumMessage() = { t with mediumMessages = t.mediumMessages + 1 }
        member t.onLargeMessage() = { t with largeMessages = t.largeMessages + 1 }


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
            expirationTime : TimeSpan
        }

        member s.msgClientId = s.messageClientData.msgAccessInfo.msgClientId
        member s.tryGetService() = s.messageClientData.msgResponseHandler.tryGetMessagingService()
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
                expirationTime = TimeSpan(6, 0, 0)
            }


    type MessagingClientMessage =
        | Start of MessagingClient
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        | StartTransmitting of MessagingClient
        | FinishTransmitting of TransmissionData
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>
        | RemoveExpiredMessages
        //| RunTestMethod of (string * AsyncReplyChannel<string>)


    and MessagingClient(d : MessagingClientData) =
        let className = "MessagingClient"
        let getMethodName n = className + "." + n
        let onSendMessageName = getMethodName "onSendMessage"
        let onGetVersionName = getMethodName "onGetVersion"
        let sendMessageImplName = getMethodName "sendMessageImpl"
        let tryReceiveSingleMessageName = getMethodName "tryReceiveSingleMessage"
        let receiveMessagesImplName = getMethodName "receiveMessagesImpl"
        let onFinishTransmittingName = getMethodName "onFinishTransmitting"
        let onConfigureClientName = getMethodName "onConfigureClient"
        let onTryPeekReceivedMessageName = getMethodName "onTryPeekReceivedMessage"
        let onTryRemoveReceivedMessageName = getMethodName "onTryRemoveReceivedMessage"
        let onStartName = getMethodName "onStart"
        let onTransmittingName = getMethodName "onTransmitting"
        let onStartTransmittingName = getMethodName "onStartTransmitting"
        let onTryProcessMessageName = getMethodName "onTryProcessMessage"
        let onRemoveExpiredMessagesName = getMethodName "onRemoveExpiredMessages"


        /// Outgoing messages are stored with the newest at the head.
        let sortOutgoing (m : List<Message>) = m |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn)


        /// Incoming messages are stored with the oldest at the head.
        let sortIncoming (m : List<Message>) = m |> List.sortBy (fun e -> e.messageDataInfo.createdOn)


        let onSendMessage (s : MessagingClientStateData) (m : MessageInfo) =
            printfn "%s..." onSendMessageName
            let message =
                {
                    messageDataInfo =
                        {
                            messageId = MessageId.create()
                            dataVersion = messagingDataVersion
                            sender = s.messageClientData.msgAccessInfo.msgClientId
                            recipientInfo = m.recipientInfo
                            createdOn = DateTime.Now
                        }

                    messageData = m.messageData
                }

            match m.recipientInfo.deliveryType with
            | GuaranteedDelivery -> s.proxy.saveMessage { messageType = OutgoingMessage; message = message }
            | NonGuaranteedDelivery -> ignore()

            { s with outgoingMessages = (message :: s.outgoingMessages) |> sortOutgoing }


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "%s" onGetVersionName
            r.Reply messagingDataVersion
            s


        let sendMessageImpl (s : MessagingClientStateData) (m : Message) =
            printfn "%s: messageId = %A, createdOn = %A" sendMessageImplName m.messageDataInfo.messageId.value m.messageDataInfo.createdOn
            try
                match s.tryGetService() with
                | Some service ->
                    match service.sendMessage m with
                    | Ok _ ->
                        match m.messageDataInfo.recipientInfo.deliveryType with
                        | GuaranteedDelivery -> s.proxy.deleteMessage m.messageDataInfo.messageId
                        | NonGuaranteedDelivery -> ignore()
                        Some m
                    | Error (DataVersionMismatch v) ->
                        s.logErr (sprintf "%s: messageId = %A, data version mismatch server has: %A but client has: %A." sendMessageImplName m.messageDataInfo.messageId.value v messagingDataVersion)
                        None
                    | Error ServerIsShuttingDown ->
                        s.logInfo (sprintf "%s: messageId = %A - server is shutting down." sendMessageImplName m.messageDataInfo.messageId.value)
                        None
                    | Error (MsgWcfError e) ->
                        s.logErr (sprintf "%s: messageId = %A, error: %A" sendMessageImplName m.messageDataInfo.messageId.value e)
                        None
                | None ->
                    s.logErr (sprintf "%s: Unable to create connection, messageId = %A." sendMessageImplName m.messageDataInfo.messageId.value)
                    None
            with
            | e ->
                s.logExn (sprintf "%s: Failed to send message: %A" sendMessageImplName m.messageDataInfo.messageId) e
                None


        let tryReceiveSingleMessage (s : MessagingClientStateData) : Async<MessageResult> =
            async {
                printfn "%s..." tryReceiveSingleMessageName

                try
                    match s.tryGetService() with
                    | Some service ->
                        match service.tryPeekMessage s.msgClientId with
                        | Some m ->
                            printfn "%s: Received message with id: %A" tryReceiveSingleMessageName m.messageDataInfo.messageId
                            match m.messageDataInfo.recipientInfo.deliveryType with
                            | GuaranteedDelivery ->
                                {
                                    message = m
                                    messageType = IncomingMessage
                                }
                                |> s.proxy.saveMessage
                            | NonGuaranteedDelivery -> ignore()

                            match service.tryDeleteFromServer s.msgClientId m.messageDataInfo.messageId with
                            //match service.tryDeleteFromServer (s.msgClientId, m.messageDataInfo.messageId) with
                            | true ->
                                printfn "%s: Deleted message from server. Message id: %A" tryReceiveSingleMessageName m.messageDataInfo.messageId
                                ignore()
                            | false ->
                                s.logErr (sprintf "%s: Unable to delete a message from server for client: %A, message id: %A." tryReceiveSingleMessageName s.msgClientId m.messageDataInfo.messageId)

                            match m.messageData.getMessageSize() with
                            | SmallSize -> return SmallMessage m
                            | MediumSize -> return MediumMessage m
                            | LargeSize -> return LargeMessage m
                        | None ->
                            printfn "%s: Did not receive a message." tryReceiveSingleMessageName
                            return NoMessage
                    | None ->
                        s.logErr (sprintf "%s: Unable to create connection." tryReceiveSingleMessageName)
                        return NoMessage
                with
                | e ->
                    printfn "%s: exception occurred: %A" tryReceiveSingleMessageName e
                    s.logExn tryReceiveSingleMessageName e
                    return NoMessage
            }


        let receiveMessagesImpl (s : MessagingClientStateData) =
            let mapper (c : MessageCount) =
                async {
                    match c.canProcess with
                    | true ->
                        match! tryReceiveSingleMessage s with
                        | NoMessage -> return None
                        | SmallMessage m -> return Some (c.onSmallMessage(), m)
                        | MediumMessage m -> return Some (c.onMediumMessage(), m)
                        | LargeMessage m -> return Some (c.onLargeMessage(), m)
                    | false -> return None
                    }

            let tryReceiveMessages() =
                async {
                    let rec doTryReceive x w c =
                        async {
                            match x with
                            | [] -> return w
                            | _ :: t ->
                                match! mapper c with
                                | Some (c1, m) -> return! doTryReceive t (m :: w) c1
                                | None -> return w
                            }

                    let! y = doTryReceive MessagingClientStateData.maxMessages [] MessageCount.defaultValue
                    return y
                    }

            async {
                printfn "%s..." receiveMessagesImplName

                try
                    match s.tryGetService() with
                    | Some service ->
                        let serverVersion = service.getVersion()

                        match serverVersion = messagingDataVersion with
                        | true -> return! tryReceiveMessages()
                        | false ->
                            s.logErr (sprintf "%s: Different data versions - client: %A, server: %A." receiveMessagesImplName messagingDataVersion.value serverVersion.value)
                            return []
                    | None ->
                        s.logErr (sprintf "%s: Unable to create connection." receiveMessagesImplName)
                        return []
                with
                | e ->
                    s.logExn receiveMessagesImplName e
                    return []
                }


        let onFinishTransmitting (s : MessagingClientStateData) (t : TransmissionData) =
            printfn "%s..." onFinishTransmittingName
            let received = t.receivedMessages
            let sent = t.sentMessages |> List.map (fun e -> e.messageDataInfo.messageId)

            let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageDataInfo.messageId) |> Set.ofList
            let notSent = Set.difference outgoing (sent |> Set.ofList)
            let remaining = s.outgoingMessages |> List.filter (fun e -> notSent.Contains e.messageDataInfo.messageId) |> sortOutgoing
            let x = { s with messagingClientState = MsgCliIdle; outgoingMessages = remaining; incomingMessages = (s.incomingMessages @ received) |> sortIncoming }
            printfn "%s: incomingMessages.Length = %A, outgoingMessages.Length = %A" onFinishTransmittingName x.incomingMessages.Length x.outgoingMessages.Length
            x


        let onConfigureClient s x =
            printfn "%s..." onConfigureClientName
            s


        let onTryPeekReceivedMessage (s : MessagingClientStateData) (r : AsyncReplyChannel<Message option>) =
            printfn "%s..." onTryPeekReceivedMessageName
            s.incomingMessages |> List.tryHead |> r.Reply
            s


        let onTryRemoveReceivedMessage (s : MessagingClientStateData) m (r : AsyncReplyChannel<bool>) =
            printfn "%s..." onTryRemoveReceivedMessageName
            s.proxy.deleteMessage m
            r.Reply true
            { s with incomingMessages = s.incomingMessages |> List.filter (fun e -> e.messageDataInfo.messageId <> m) |> sortIncoming }


        let onStart (s : MessagingClientStateData) (w : MessagingClient) =
            match s.messagingClientState with
            | MsgCliNotStarted ->
                let messages = s.proxy.loadMessages()
                let incoming = messages |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
                let outgoing = messages |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)

                let eventHandler _ =
                    printfn "%s: Transmitting messages..." onTransmittingName
                    w.startTransmitting()

                let h = new EventHandler(EventHandlerInfo.defaultValue (s.logExn onTransmittingName) eventHandler)
                do h.start()

                let eventHandler1 _ =
                    w.removeExpiredMessages()

                let h1 = new EventHandler(EventHandlerInfo.oneHourValue (d.logger.logExn onStartName) eventHandler1)
                do h1.start()

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
                    printfn "%s..." onStartTransmittingName
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
                printfn "%s: Starting..." onTryProcessMessageName
                match! w.tryPeekReceivedMessage() with
                | Some m ->
                    try
                        printfn "    %s: calling f m, messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn
                        let r = f x m
                        printfn "    %s: calling tryRemoveReceivedMessage, messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn

                        match! w.tryRemoveReceivedMessage m.messageDataInfo.messageId with
                        | true -> printfn "    %s: Successfully removed messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn
                        | false -> printfn "    %s: !!! ERROR !!! removing messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn

                        printfn "    %s - completed." onTryProcessMessageName
                        return Some r
                    with
                    | ex ->
                        logger.logExn onTryProcessMessageName ex
                        return None
                | None -> return None
            }


        let onRemoveExpiredMessages (s : MessagingClientStateData) =
            let removeExpired (r : List<Message>) =
                let expired, notExpired = r |> List.partition (fun e -> e.isExpired s.expirationTime)
                expired |> List.map (fun e -> s.proxy.deleteMessage e.messageDataInfo.messageId) |> ignore
                notExpired

            {
                s with
                    outgoingMessages = s.outgoingMessages |> removeExpired
                    incomingMessages = s.incomingMessages |> removeExpired
            }


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! timed onStartName onStart s w |> loop
                            | GetVersion r -> return! timed onGetVersionName onGetVersion s r |> loop
                            | SendMessage m -> return! timed onSendMessageName onSendMessage s m |> loop
                            | StartTransmitting w -> return! timed onStartTransmittingName onStartTransmitting s w |> loop
                            | FinishTransmitting d -> return! onFinishTransmitting s d |> loop
                            | ConfigureClient x -> return! timed onConfigureClientName onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! timed onTryPeekReceivedMessageName onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! timed onTryRemoveReceivedMessageName onTryRemoveReceivedMessage s m r |> loop
                            | RemoveExpiredMessages -> return! timed onRemoveExpiredMessagesName onRemoveExpiredMessages s |> loop
                            //| RunTestMethod (name, reply) ->
                            //    match s.tryGetService() with
                            //    | Some service ->
                            //        printfn "RunTestMethod: Successfully obtained WCF service."
                            //        let x = service.testMethod name
                            //        printfn "RunTestMethod: x = '%A'." x
                            //        reply.Reply x
                            //    | None ->
                            //        printfn "RunTestMethod: Unable to get WCF service."
                            //        reply.Reply "!Error!"
                            //    return! s|> loop
                        }

                MessagingClientStateData.defaultValue d |> loop
                )


        member this.start() = Start this |> messageLoop.Post
        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage (m : MessageInfo) = SendMessage m |> messageLoop.Post
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member this.startTransmitting() = StartTransmitting this |> messageLoop.Post
        member __.finishTransmitting d = FinishTransmitting d |> messageLoop.Post
        member private __.tryPeekReceivedMessage() : Async<Message option> = messageLoop.PostAndAsyncReply (fun reply -> TryPeekReceivedMessage reply)
        member private __.tryRemoveReceivedMessage m = messageLoop.PostAndAsyncReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member this.tryProcessMessage s f = onTryProcessMessage this s f
        member private __.removeExpiredMessages() = RemoveExpiredMessages |> messageLoop.Post

        //member __.testMethod (m : string) : string =
        //    messageLoop.PostAndReply (fun reply -> RunTestMethod (m, reply))

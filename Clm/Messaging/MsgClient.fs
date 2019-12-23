namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.TimerEvents
open System.Threading

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
            messagingService : IMessagingService
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }


    type MessagingClientState =
        | MsgCliNotStarted
        | MsgCliIdle


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
        member s.messagingService = s.messageClientData.messagingService
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


    let private className = "MessagingClient"
    let private getMethodName n = className + "." + n
    let private onSendMessageName = getMethodName "onSendMessage"
    let private onGetVersionName = getMethodName "onGetVersion"
    let private sendMessageImplName = getMethodName "sendMessageImpl"
    let private tryReceiveSingleMessageName = getMethodName "tryReceiveSingleMessage"
    let private receiveMessagesImplName = getMethodName "receiveMessagesImpl"
    let private onFinishTransmittingName = getMethodName "onFinishTransmitting"
    let private onConfigureClientName = getMethodName "onConfigureClient"
    let private onTryPeekReceivedMessageName = getMethodName "onTryPeekReceivedMessage"
    let private onTryRemoveReceivedMessageName = getMethodName "onTryRemoveReceivedMessage"
    let private onStartName = getMethodName "onStart"
    let private onTransmittingName = getMethodName "onTransmitting"
    let private onStartTransmittingName = getMethodName "onStartTransmitting"
    let private onTryProcessMessageName = getMethodName "onTryProcessMessage"
    let private onRemoveExpiredMessagesName = getMethodName "onRemoveExpiredMessages"


    /// Outgoing messages are stored with the newest at the head.
    let sortOutgoing (m : List<Message>) = m |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn)


    /// Incoming messages are stored with the oldest at the head.
    let sortIncoming (m : List<Message>) = m |> List.sortBy (fun e -> e.messageDataInfo.createdOn)


    let tryReceiveSingleMessage (s : MessagingClientStateData) : MessageResult =
        printfn "%s..." tryReceiveSingleMessageName

        match s.messagingService.tryPeekMessage s.msgClientId with
        | Ok (Some m) ->
            printfn "%s: Received message with id: %A" tryReceiveSingleMessageName m.messageDataInfo.messageId
            match m.messageDataInfo.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                {
                    message = m
                    messageType = IncomingMessage
                }
                |> s.proxy.saveMessage
            | NonGuaranteedDelivery -> ignore()

            match s.messagingService.tryDeleteFromServer (s.msgClientId, m.messageDataInfo.messageId) with
            | Ok true ->
                printfn "%s: Deleted message from server. Message id: %A" tryReceiveSingleMessageName m.messageDataInfo.messageId
                ignore()
            | Ok false ->
                s.logErr (sprintf "%s: Unable to delete a message from server for client: %A, message id: %A." tryReceiveSingleMessageName s.msgClientId m.messageDataInfo.messageId)
            | Error e ->
                s.logErr (sprintf "%s: Unable to delete a message from server for client: %A, message id: %A due to error: %A." tryReceiveSingleMessageName s.msgClientId m.messageDataInfo.messageId e)
            match m.messageData.getMessageSize() with
            | SmallSize -> SmallMessage m
            | MediumSize -> MediumMessage m
            | LargeSize -> LargeMessage m
        | Ok None ->
            printfn "%s: Did not receive a message." tryReceiveSingleMessageName
            NoMessage
        | Error e ->
            printfn "%s: Exception occurred: %A." tryReceiveSingleMessageName e
            NoMessage


    let receiveMessagesImpl (s : MessagingClientStateData) =
        let mapper (c : MessageCount) =
            match c.canProcess with
            | true ->
                match tryReceiveSingleMessage s with
                | NoMessage -> None
                | SmallMessage m -> Some (c.onSmallMessage(), m)
                | MediumMessage m -> Some (c.onMediumMessage(), m)
                | LargeMessage m -> Some (c.onLargeMessage(), m)
            | false -> None

        let tryReceiveMessages() =
            let rec doTryReceive x w c =
                match x with
                | [] -> w
                | _ :: t ->
                    match mapper c with
                    | Some (c1, m) -> doTryReceive t (m :: w) c1
                    | None -> w

            let y = doTryReceive MessagingClientStateData.maxMessages [] MessageCount.defaultValue
            y

        printfn "%s..." receiveMessagesImplName

        match s.messagingService.getVersion() with
        | Ok serverVersion ->
            match serverVersion = messagingDataVersion with
            | true -> tryReceiveMessages()
            | false ->
                s.logErr (sprintf "%s: Different data versions - client: %A, server: %A." receiveMessagesImplName messagingDataVersion.value serverVersion.value)
                []
        | Error e ->
            s.logErr (sprintf "%s: Exception occurred: %A." receiveMessagesImplName e)
            []


    type MessagingClientMessage =
        | Start of MessagingClient
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        | StartTransmitting
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>
        | RemoveExpiredMessages


    and MessagingClient(d : MessagingClientData) =

        let onSendMessage (s : MessagingClientStateData) (m : MessageInfo) =
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

            printfn "%s: messageId = %A" onSendMessageName message.messageDataInfo.messageId

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

            match s.messagingService.sendMessage m with
            | Ok _ ->
                match m.messageDataInfo.recipientInfo.deliveryType with
                | GuaranteedDelivery -> s.proxy.deleteMessage m.messageDataInfo.messageId
                | NonGuaranteedDelivery -> ignore()
                Some m
            | Error (DataVersionMismatch v) ->
                s.logErr (sprintf "%s: messageId = %A, data version mismatch server has: %A but client message has: %A." sendMessageImplName m.messageDataInfo.messageId.value v m.messageDataInfo.dataVersion)
                None
            | Error ServerIsShuttingDown ->
                s.logInfo (sprintf "%s: messageId = %A - server is shutting down." sendMessageImplName m.messageDataInfo.messageId.value)
                None
            | Error (MsgWcfError e) ->
                s.logErr (sprintf "%s: messageId = %A, error: %A" sendMessageImplName m.messageDataInfo.messageId.value e)
                None


        let onFinishTransmitting (s : MessagingClientStateData) (t : TransmissionData) =
            printfn "%s..." onFinishTransmittingName
            let received = t.receivedMessages
            let sent = t.sentMessages |> List.map (fun e -> e.messageDataInfo.messageId)

            let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageDataInfo.messageId) |> Set.ofList
            let notSent = Set.difference outgoing (sent |> Set.ofList)
            let remaining = s.outgoingMessages |> List.filter (fun e -> notSent.Contains e.messageDataInfo.messageId) |> sortOutgoing
            let x = { s with outgoingMessages = remaining; incomingMessages = (s.incomingMessages @ received) |> sortIncoming }
            printfn "%s: incomingMessages.Length = %A, outgoingMessages.Length = %A" onFinishTransmittingName x.incomingMessages.Length x.outgoingMessages.Length
            x


        let onConfigureClient s x =
            printfn "%s..." onConfigureClientName
            s


        let onTryPeekReceivedMessage (s : MessagingClientStateData) (r : AsyncReplyChannel<Message option>) =
            let x = s.incomingMessages |> List.tryHead
            printfn "%s: messageId = %A" onTryPeekReceivedMessageName (x |> Option.bind (fun e -> Some e.messageDataInfo.messageId))
            r.Reply x
            s


        let onTryRemoveReceivedMessage (s : MessagingClientStateData) m (r : AsyncReplyChannel<bool>) =
            printfn "%s: messageId: %A" onTryRemoveReceivedMessageName m
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

                { s with
                    messagingClientState = MsgCliIdle
                    outgoingMessages = (s.outgoingMessages @ outgoing) |> sortOutgoing
                    incomingMessages = (s.incomingMessages @ incoming) |> sortIncoming
                }
            | MsgCliIdle -> s


        let onStartTransmitting s =
            match s.messagingClientState with
            | MsgCliNotStarted -> s
            | MsgCliIdle ->
                printfn "%s..." onStartTransmittingName
                let sentMessages =
                    // Note that we need to apply List.rev to get to the first (the oldest) message in the outgoing queue.
                    s.outgoingMessages
                    |> List.rev
                    |> List.map (sendMessageImpl s)
                    |> List.choose id

                let received = receiveMessagesImpl s

                {
                    receivedMessages = received
                    sentMessages = sentMessages
                }
                |> onFinishTransmitting s


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
                            | StartTransmitting -> return! timed onStartTransmittingName onStartTransmitting s |> loop
                            | ConfigureClient x -> return! timed onConfigureClientName onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! timed onTryPeekReceivedMessageName onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! timed onTryRemoveReceivedMessageName onTryRemoveReceivedMessage s m r |> loop
                            | RemoveExpiredMessages -> return! timed onRemoveExpiredMessagesName onRemoveExpiredMessages s |> loop
                        }

                MessagingClientStateData.defaultValue d |> loop
                )


        member this.start() = Start this |> messageLoop.Post
        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage (m : MessageInfo) = SendMessage m |> messageLoop.Post
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member __.startTransmitting() = StartTransmitting |> messageLoop.Post
        member __.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member __.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member __.removeExpiredMessages() = RemoveExpiredMessages |> messageLoop.Post


    let mutable private callCount = -1


    let onTryProcessMessage (w : MessagingClient) x f =
        printfn "%s: Starting..." onTryProcessMessageName

        let retVal =
            if Interlocked.Increment(&callCount) = 0
            then
                match w.tryPeekReceivedMessage() with
                | Some m ->
                    try
                        printfn "    %s: calling f m, messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn
                        let r = f x m
                        printfn "    %s: calling tryRemoveReceivedMessage, messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn

                        match w.tryRemoveReceivedMessage m.messageDataInfo.messageId with
                        | true -> printfn "    %s: Successfully removed messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn
                        | false -> printfn "    %s: !!! ERROR !!! removing messageId: %A, createdOn: %A" onTryProcessMessageName m.messageDataInfo.messageId m.messageDataInfo.createdOn

                        printfn "    %s - completed." onTryProcessMessageName
                        Some r
                    with
                    | ex ->
                        logger.logExn onTryProcessMessageName ex
                        None
                | None -> None
            else
                printfn "%s: Not processing message at %A because callCount = %A." onTryProcessMessageName DateTime.Now callCount
                None

        Interlocked.Decrement(&callCount) |> ignore
        retVal


    type MessagingClient
        with
        member w.tryProcessMessage s f = onTryProcessMessage w s f

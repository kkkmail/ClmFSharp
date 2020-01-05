namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.GeneralData
open ClmSys.GeneralErrors
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.TimerEvents
open System.Threading
open ServiceProxy.MsgProcessorProxy
open ClmSys

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


    type MessagingClientOnStartProxy =
        {
            startTransmitting : unit -> unit
            removeExpiredMessages : unit -> unit
        }


    type TryReceiveSingleMessageProxy =
        {
            saveMessage : MessageWithType -> UnitResult
            deleteMessage : MessageId -> UnitResult
            getVersion : unit -> GetVersionResult
            tryPeekMessage : unit -> TryPeekMessageResult
            tryDeleteFromServer : MessageId -> TryDeleteFromServerResult
        }


    type MessagingClientMessage =
        | Start of MessagingClientOnStartProxy
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        | StartTransmitting
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>
        | RemoveExpiredMessages


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


    let trySaveMessage (proxy : TryReceiveSingleMessageProxy) (m : Message) =
        match m.messageDataInfo.recipientInfo.deliveryType with
        | GuaranteedDelivery ->
            {
                message = m
                messageType = IncomingMessage
            }
            |> proxy.saveMessage
        | NonGuaranteedDelivery -> Ok()


    let tryRemoveMessage (proxy : TryReceiveSingleMessageProxy) (m : Message) e =
        let e1 = e |> TryDeleteFromServerErr |> MessagingServiceErr

        match proxy.deleteMessage m.messageDataInfo.messageId with
        | Ok _ -> Error e1
        | Error e2 -> Error (e1 + e2)


    let toMessageWithSize m =
        match m.messageData.getMessageSize() with
        | SmallSize -> SmallMessage m |> Ok
        | MediumSize -> MediumMessage m |> Ok
        | LargeSize -> LargeMessage m |> Ok


    let tryReceiveSingleMessage (proxy : TryReceiveSingleMessageProxy) : MessageResult =
        printfn "%s..." tryReceiveSingleMessageName

        match proxy.tryPeekMessage () with
        | Ok (Some m) ->
            match trySaveMessage proxy m with
            | Ok _ ->
                match proxy.tryDeleteFromServer m.messageDataInfo.messageId with
                | Ok _ -> toMessageWithSize m
                | Error e -> tryRemoveMessage proxy m e
            | Error e -> Error e
        | Ok None -> Ok NoMessage
        | Error e -> e |> TryPeekMessageErr |> MessagingServiceErr |> Error


    let mapper (proxy : TryReceiveSingleMessageProxy) (c : MessageCount) =
        match c.canProcess with
        | true ->
            match tryReceiveSingleMessage proxy with
            | Ok NoMessage -> None |> Ok
            | Ok (SmallMessage m) -> (c.onSmallMessage(), m) |> Some |> Ok
            | Ok (MediumMessage m) -> (c.onMediumMessage(), m) |> Some |> Ok
            | Ok (LargeMessage m) -> (c.onLargeMessage(), m) |> Some |> Ok
            | Error e -> Error e
        | false -> None |> Ok


    let tryReceiveMessages (proxy : TryReceiveSingleMessageProxy) : ListResult<Message> =
        let rec doTryReceive x w c =
            let toOk z = z |> List.map Ok |> Ok
            let toOkWithLastErr e z = ((Error e) :: (z |> List.map Ok)) |> Ok

            match x with
            | [] -> toOk w
            | _ :: t ->
                match mapper proxy c with
                | Ok (Some (c1, m)) -> doTryReceive t (m :: w) c1
                | Ok None -> toOk w
                | Error e -> toOkWithLastErr e w

        let y = doTryReceive MessagingClientStateData.maxMessages [] MessageCount.defaultValue
        y


    let receiveMessagesImpl (proxy : TryReceiveSingleMessageProxy) =
        printfn "%s..." receiveMessagesImplName

        match proxy.getVersion() with
        | Ok serverVersion ->
            match serverVersion = messagingDataVersion with
            | true -> tryReceiveMessages proxy
            | false ->
                {
                    localVersion = messagingDataVersion.value
                    remoteVersion = serverVersion.value
                }
                |> VersionMismatchError |> GetVersionErr |> MessagingServiceErr |> Error
        | Error e -> e |> GetVersionErr |> MessagingServiceErr |> Error


    let createMessage msgClientId (m : MessageInfo) =
        {
            messageDataInfo =
                {
                    messageId = MessageId.create()
                    dataVersion = messagingDataVersion
                    sender = msgClientId
                    recipientInfo = m.recipientInfo
                    createdOn = DateTime.Now
                }

            messageData = m.messageData
        }


    let onSendMessage (s : MessagingClientStateData) (m : MessageInfo) =
        let message = createMessage s.messageClientData.msgAccessInfo.msgClientId m
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


    let onStart (s : MessagingClientStateData) (w : MessagingClientOnStartProxy) =
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


    type MessagingClient(d : MessagingClientData) =
        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! onStart s w |> loop
                            | GetVersion r -> return! onGetVersion s r |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | StartTransmitting -> return! onStartTransmitting s |> loop
                            | ConfigureClient x -> return! onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! onTryRemoveReceivedMessage s m r |> loop
                            | RemoveExpiredMessages -> return! onRemoveExpiredMessages s |> loop
                        }

                MessagingClientStateData.defaultValue d |> loop
                )


        member m.start() = Start m.messagingClientOnStartProxy |> messageLoop.Post
        member _.getVersion() = GetVersion |> messageLoop.PostAndReply
        member _.sendMessage (m : MessageInfo) = SendMessage m |> messageLoop.Post
        member _.configureClient x = ConfigureClient x |> messageLoop.Post
        member _.startTransmitting() = StartTransmitting |> messageLoop.Post
        member _.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member _.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member _.removeExpiredMessages() = RemoveExpiredMessages |> messageLoop.Post

        member m.messageProcessorProxy : MessageProcessorProxy =
            {
                sendMessage = m.sendMessage
                tryPeekReceivedMessage = m.tryPeekReceivedMessage
                tryRemoveReceivedMessage = m.tryRemoveReceivedMessage
            }


        member m.messagingClientOnStartProxy : MessagingClientOnStartProxy =
            {
                startTransmitting = m.startTransmitting
                removeExpiredMessages = m.removeExpiredMessages
            }


        member _.tryReceiveSingleMessageProxy : TryReceiveSingleMessageProxy =
            {
                saveMessage = d.msgClientProxy.saveMessage
                deleteMessage = d.msgClientProxy.deleteMessage
                getVersion = d.messagingService.getVersion
                tryPeekMessage = fun () -> d.messagingService.tryPeekMessage d.msgAccessInfo.msgClientId
                tryDeleteFromServer = fun x -> d.messagingService.tryDeleteFromServer (d.msgAccessInfo.msgClientId, x)
            }


    let mutable private callCount = -1


    let onTryProcessMessage (w : MessageProcessorProxy) x f =
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
                        w.logger.logExn onTryProcessMessageName ex
                        None
                | None -> None
            else
                printfn "%s: Not processing message at %A because callCount = %A." onTryProcessMessageName DateTime.Now callCount
                None

        Interlocked.Decrement(&callCount) |> ignore
        retVal

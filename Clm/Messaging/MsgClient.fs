namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open System.Threading
open ServiceProxy.MsgProcessorProxy
open ClmSys
open ClmSys.GeneralData
open ClmSys.TimerEvents
open ClmSys.ClmErrors
open ClmSys.MessagingClientErrors
open ClmSys.MessagingPrimitives
open ClmSys.MessagingCommonErrors

module Client =

    /// Maximum number of messages to process in one go.
    let maxNumberOfMessages = 1_000

    let maxNumberOfSmallMessages = 1_000
    let maxNumberOfMediumMessages = 20
    let maxNumberOfLargeMessages = 2

    let private toError e = e |> MessagingClientErr |> Error
    let private addError g f e = ((f |> g |> MessagingClientErr) + e) |> Error

    type MessageCount =
        {
            smallMessages : int
            mediumMessages : int
            largeMessages : int
        }

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
            incomingMessages : List<Message>
            outgoingMessages : List<Message>
            expirationTime : TimeSpan
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                messagingClientState = MsgCliNotStarted
                incomingMessages = []
                outgoingMessages = []
                expirationTime = TimeSpan(6, 0, 0)
            }


    type TryReceiveSingleMessageProxy =
        {
            saveMessage : MessageWithType -> UnitResult
            tryDeleteMessage : MessageId -> UnitResult
            getVersion : unit -> ClmResult<MessagingDataVersion>
            sendMessage : Message -> UnitResult
            tryPeekMessage : unit -> ClmResult<Message option>
            tryDeleteFromServer : MessageId -> UnitResult
        }


    type MessagingClientMessage =
        | Start of AsyncReplyChannel<UnitResult>
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of AsyncReplyChannel<UnitResult> * MessageInfo
        | TransmitMessages of AsyncReplyChannel<UnitResult> * TryReceiveSingleMessageProxy
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of AsyncReplyChannel<TryRemoveReceivedMessageResult> * MessageId
        | RemoveExpiredMessages of AsyncReplyChannel<UnitResult>


    /// Outgoing messages are stored with the newest at the head.
    let sortOutgoing (m : List<Message>) = m |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn)


    /// Incoming messages are stored with the oldest at the head.
    let sortIncoming (m : List<Message>) = m |> List.sortBy (fun e -> e.messageDataInfo.createdOn)


    let toMessageWithSize m =
        match m.messageData.getMessageSize() with
        | SmallSize -> SmallMessage m
        | MediumSize -> MediumMessage m
        | LargeSize -> LargeMessage m
        |> Ok


    let tryReceiveSingleMessage (proxy : TryReceiveSingleMessageProxy) : MessageResult =
        printfn "tryReceiveSingleMessage: Starting..."
        let addError = addError TryReceiveSingleMessageErr

        let result =
            match proxy.tryPeekMessage () with
            | Ok (Some m) ->
                let r =
                    match m.messageDataInfo.recipientInfo.deliveryType with
                    | GuaranteedDelivery -> proxy.saveMessage { message = m; messageType = IncomingMessage }
                    | NonGuaranteedDelivery -> Ok()

                match r with
                | Ok() ->
                    match proxy.tryDeleteFromServer m.messageDataInfo.messageId with
                    | Ok() -> toMessageWithSize m
                    | Error e ->
                        match proxy.tryDeleteMessage m.messageDataInfo.messageId with
                        | Ok() -> addError TryDeleteFromServerErr e
                        | Error e1 -> addError TryDeleteFromServerErr (e1 + e)
                | Error e -> addError SaveMessageErr e
            | Ok None -> Ok NoMessage
            | Error e -> addError TryPeekMessageErr e

        printfn "tryReceiveSingleMessage: Comleted."
        result


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
        let addError = addError GetVersionErr

        match proxy.getVersion() with
        | Ok serverVersion ->
            match serverVersion = messagingDataVersion with
            | true -> tryReceiveMessages proxy
            | false ->
                {
                    localVersion = messagingDataVersion
                    remoteVersion = serverVersion
                }
                |> VersionMismatchErr |> GetVersionErr |> toError
        | Error e -> addError GetVersionWcfErr e


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


    let onSendMessage saveMessage msgClientId s m =
        let message = createMessage msgClientId m

        let result =
            match m.recipientInfo.deliveryType with
            | GuaranteedDelivery -> saveMessage { messageType = OutgoingMessage; message = message }
            | NonGuaranteedDelivery -> Ok()

        { s with outgoingMessages = (message :: s.outgoingMessages) |> sortOutgoing }, result


    let onGetVersion s =
        s, messagingDataVersion


    let sendMessageImpl sendMessage deleteMessage (m : Message) =
        let addError = addError SendMessageErr

        match sendMessage m with
        | Ok () ->
            match m.messageDataInfo.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                match deleteMessage m.messageDataInfo.messageId with
                | Ok () -> Ok m
                | Error e -> Error e
            | NonGuaranteedDelivery -> Ok m
        | Error e -> addError SendMessageFaileErr e


    let onFinishTransmitting (s : MessagingClientStateData) (t : TransmissionData) =
        let received = t.receivedMessages
        let sent = t.sentMessages |> List.map (fun m -> m.messageDataInfo.messageId)
        let outgoing = s.outgoingMessages |> List.map (fun m -> m.messageDataInfo.messageId) |> Set.ofList
        let notSent = Set.difference outgoing (sent |> Set.ofList)
        let remaining = s.outgoingMessages |> List.filter (fun m -> notSent.Contains m.messageDataInfo.messageId) |> sortOutgoing
        let x = { s with outgoingMessages = remaining; incomingMessages = (s.incomingMessages @ received) |> sortIncoming }
        x


    let onConfigureClient s _ =
        s


    let onTryPeekReceivedMessage (s : MessagingClientStateData) =
        printfn "onTryPeekReceivedMessage: Starting..."
        let x = s.incomingMessages |> List.tryHead
        s, x


    let onTryRemoveReceivedMessage deleteMessage (s : MessagingClientStateData) m =
        printfn "onTryRemoveReceivedMessage: Starting..."
        let removedMessage e =
            let result =
                match e with
                | None -> RemovedSucessfully
                | Some e -> RemovedWithError e

            { s with incomingMessages = s.incomingMessages |> List.filter (fun e -> e.messageDataInfo.messageId <> m) |> sortIncoming }, result

        let failedToRemove e = s, FailedToRemove e

        match s.incomingMessages |> List.tryFind (fun e -> e.messageDataInfo.messageId = m) with
        | Some msg ->
            match msg.messageDataInfo.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                match deleteMessage m with
                | Ok() -> removedMessage None
                | Error e -> removedMessage (Some e)
            | NonGuaranteedDelivery -> removedMessage None
        | None -> m |> MessageNotFoundErr |> OnTryRemoveReceivedMessageErr |> MessagingClientErr |> failedToRemove


    let onStart (s : MessagingClientStateData) loadMessages =
        let w, result =
            match s.messagingClientState with
            | MsgCliNotStarted ->
                match loadMessages() with
                | Ok messages ->
                    let m, e = messages |> Rop.unzip
                    let incoming = m |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
                    let outgoing = m |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)
                    let error = foldToUnitResult e

                    { s with
                        messagingClientState = MsgCliIdle
                        outgoingMessages = (s.outgoingMessages @ outgoing) |> sortOutgoing
                        incomingMessages = (s.incomingMessages @ incoming) |> sortIncoming
                    }, error
                | Error e -> s, Error e
            | MsgCliIdle -> s, Ok()

        w, result


    let onTransmitMessages proxy s =
        printfn "onTransmitMessages: Starting..."
        let (w, result) =
            match s.messagingClientState with
            | MsgCliNotStarted -> s, Ok()
            | MsgCliIdle ->
                printfn "onTransmitMessages: Sending..."
                // Note that we need to apply List.rev to get to the first (the oldest) message in the outgoing queue.
                let sent, sentErrors =
                    s.outgoingMessages
                    |> List.rev
                    |> List.map (sendMessageImpl proxy.sendMessage proxy.tryDeleteMessage)
                    |> Rop.unzip

                printfn "onTransmitMessages: Receiving..."
                let received, receivedErrors =
                    match receiveMessagesImpl proxy with
                    | Ok r -> r |> Rop.unzip
                    | Error e -> [], [ e ]

                let e = sentErrors @ receivedErrors |> foldToUnitResult
                { receivedMessages = received; sentMessages = sent } |> (onFinishTransmitting s), e

        printfn "onTransmitMessages: result = %A" result
        w, result


    let removeExpired deleteMessage expirationTime (r : List<Message>) =
        let expired, notExpired = r |> List.partition (fun e -> e.isExpired expirationTime)

        let err =
            expired
            |> List.map (fun e -> deleteMessage e.messageDataInfo.messageId)
            |> List.map (fun (e : UnitResult) -> match e with | Ok() -> None | Error e -> Some e)
            |> List.choose id

        notExpired, err


    let onRemoveExpiredMessages deleteMessage (s : MessagingClientStateData) =
        let removeExpired = removeExpired deleteMessage s.expirationTime
        let o = s.outgoingMessages |> removeExpired
        let i = s.incomingMessages |> removeExpired
        let result = (snd o) @ (snd i) |> foldToUnitResult
        { s with outgoingMessages = fst o; incomingMessages = fst i }, result


    type MessagingClient(d : MessagingClientData) =
        let loadMsg = d.msgClientProxy.loadMessages
        let saveMsg = d.msgClientProxy.saveMessage
        let deleteMsg = d.msgClientProxy.tryDeleteMessage
        let msgClietnId = d.msgAccessInfo.msgClientId

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start r -> return! onStart s loadMsg |> (withReply r) |> loop
                            | GetVersion r -> return! onGetVersion s |> (withReply r) |> loop
                            | SendMessage (r, m) -> return! onSendMessage saveMsg msgClietnId s m |> (withReply r) |> loop
                            | TransmitMessages (r, p) -> return! onTransmitMessages p s |> (withReply r) |> loop
                            | ConfigureClient x -> return! onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! onTryPeekReceivedMessage s |> (withReply r) |> loop
                            | TryRemoveReceivedMessage (r, m) -> return! onTryRemoveReceivedMessage deleteMsg s m |> (withReply r) |> loop
                            | RemoveExpiredMessages r -> return! onRemoveExpiredMessages deleteMsg s |> (withReply r) |> loop
                        }

                MessagingClientStateData.defaultValue |> loop
                )


        member _.start() = messageLoop.PostAndReply Start
        member _.getVersion() = messageLoop.PostAndReply GetVersion
        member _.sendMessage (m : MessageInfo) = messageLoop.PostAndReply (fun reply -> SendMessage (reply, m))
        member _.configureClient x = ConfigureClient x |> messageLoop.Post
        member m.transmitMessages() = messageLoop.PostAndReply (fun reply -> TransmitMessages (reply, m.tryReceiveSingleMessageProxy))
        member _.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member _.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (reply, m))
        member _.removeExpiredMessages() = messageLoop.PostAndReply RemoveExpiredMessages


        member m.messageProcessorProxy : MessageProcessorProxy =
            {
                tryPeekReceivedMessage = m.tryPeekReceivedMessage
                tryRemoveReceivedMessage = m.tryRemoveReceivedMessage
                sendMessage = m.sendMessage
            }


        member _.tryReceiveSingleMessageProxy : TryReceiveSingleMessageProxy =
            {
                saveMessage = d.msgClientProxy.saveMessage
                tryDeleteMessage = d.msgClientProxy.tryDeleteMessage
                getVersion = d.messagingService.getVersion
                sendMessage = d.messagingService.sendMessage
                tryPeekMessage = fun () -> d.messagingService.tryPeekMessage d.msgAccessInfo.msgClientId
                tryDeleteFromServer = fun x -> d.messagingService.tryDeleteFromServer (d.msgAccessInfo.msgClientId, x)
            }


    let mutable private callCount = -1


    let onTryProcessMessage (w : MessageProcessorProxy) x f =
        let retVal =
            if Interlocked.Increment(&callCount) = 0
            then
                match w.tryPeekReceivedMessage() with
                | Some m ->
                    try
                        let r = f x m
    
                        match w.tryRemoveReceivedMessage m.messageDataInfo.messageId with
                        | RemovedSucessfully -> ProcessedSucessfully r
                        | RemovedWithError e -> ProcessedWithError (r, e)
                        | FailedToRemove e -> ProcessedWithFailedToRemove (r, e)
                    with
                    | e -> e |> OnTryProcessMessageExn |> OnTryProcessMessageErr |> MessagingClientErr |> FailedToProcess
                | None -> NothingToDo
            else BusyProcessing
    
        Interlocked.Decrement(&callCount) |> ignore
        retVal


    /// Call this function to create timer events necessary for automatic MessagingClient operation.
    /// If you don't call it, then you have to operate MessagingClient by hands.
    let createMessagingClientEventHandlers logger (w : MessagingClient) =
        let eventHandler _ = w.transmitMessages()
        let h = ClmEventHandlerInfo.defaultValue logger eventHandler "MessagingClient - transmitMessages" |> ClmEventHandler
        do h.start()

        let eventHandler1 _ = w.removeExpiredMessages()
        let h1 = ClmEventHandlerInfo.oneHourValue logger eventHandler1 "MessagingClient - removeExpiredMessages" |> ClmEventHandler
        do h1.start()



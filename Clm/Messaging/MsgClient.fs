﻿namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.GeneralErrors
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
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
            //messageClientData : MessagingClientData
            incomingMessages : List<Message>
            outgoingMessages : List<Message>
            expirationTime : TimeSpan
        }

        //member s.msgClientId = s.messageClientData.msgAccessInfo.msgClientId
        //member s.messagingService = s.messageClientData.messagingService
        //member s.proxy = s.messageClientData.msgClientProxy

        //member s.logErr = s.messageClientData.logger.logErr
        //member s.logExn = s.messageClientData.logger.logExn
        //member s.logInfo = s.messageClientData.logger.logInfo

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                messagingClientState = MsgCliNotStarted
                //messageClientData = d
                incomingMessages = []
                outgoingMessages = []
                expirationTime = TimeSpan(6, 0, 0)
            }


    type TrySaveMessageProxy =
        {
            saveMessage : MessageWithType -> UnitResult
        }


    type TryRemoveMessageProxy =
        {
            deleteMessage : MessageId -> UnitResult
        }


    type TryReceiveSingleMessageProxy =
        {
            saveMessage : MessageWithType -> UnitResult
            deleteMessage : MessageId -> UnitResult
            getVersion : unit -> GetVersionResult
            sendMessage : Message -> MessageDeliveryResult
            tryPeekMessage : unit -> TryPeekMessageResult
            tryDeleteFromServer : MessageId -> TryDeleteFromServerResult
        }

        member p.trySaveMessageProxy =
            {
                saveMessage = p.saveMessage
            }

        member p.tryRemoveMessageProxy =
            {
                deleteMessage = p.deleteMessage
            }


    type MessagingClientMessage =
        | Start of AsyncReplyChannel<ClmError option>
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo * AsyncReplyChannel<ClmError option>
        | TransmitMessages of TryReceiveSingleMessageProxy * AsyncReplyChannel<ClmError option>
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<TryRemoveReceivedMessageResult>
        | RemoveExpiredMessages of AsyncReplyChannel<ClmError option>


    /// Outgoing messages are stored with the newest at the head.
    let sortOutgoing (m : List<Message>) = m |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn)


    /// Incoming messages are stored with the oldest at the head.
    let sortIncoming (m : List<Message>) = m |> List.sortBy (fun e -> e.messageDataInfo.createdOn)


    let trySaveMessage (proxy : TrySaveMessageProxy) (m : Message) =
        match m.messageDataInfo.recipientInfo.deliveryType with
        | GuaranteedDelivery ->
            {
                message = m
                messageType = IncomingMessage
            }
            |> proxy.saveMessage
        | NonGuaranteedDelivery -> Ok()


    let tryRemoveMessage (proxy : TryRemoveMessageProxy) (m : Message) e =
        let e1 = e |> TryDeleteFromServerErr |> MessagingServiceErr

        match proxy.deleteMessage m.messageDataInfo.messageId with
        | Ok _ -> Error e1
        | Error e2 -> Error (e1 + e2)


    let toMessageWithSize m =
        match m.messageData.getMessageSize() with
        | SmallSize -> SmallMessage m
        | MediumSize -> MediumMessage m
        | LargeSize -> LargeMessage m
        |> Ok


    let tryReceiveSingleMessage (proxy : TryReceiveSingleMessageProxy) : MessageResult =
        match proxy.tryPeekMessage () with
        | Ok (Some m) ->
            match trySaveMessage proxy.trySaveMessageProxy m with
            | Ok _ ->
                match proxy.tryDeleteFromServer m.messageDataInfo.messageId with
                | Ok _ -> toMessageWithSize m
                | Error e -> tryRemoveMessage proxy.tryRemoveMessageProxy m e
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


    let onSendMessage saveMessage msgClientId s m (r : AsyncReplyChannel<ClmError option>) =
        let message = createMessage msgClientId m

        let err =
            match m.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                match saveMessage { messageType = OutgoingMessage; message = message } with
                | Ok () -> None
                | Error e -> Some e
            | NonGuaranteedDelivery -> None

        r.Reply err
        { s with outgoingMessages = (message :: s.outgoingMessages) |> sortOutgoing }


    let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
        r.Reply messagingDataVersion
        s


    let sendMessageImpl sendMessage deleteMessage (m : Message) =
        match sendMessage m with
        | Ok () ->
            match m.messageDataInfo.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                match deleteMessage m.messageDataInfo.messageId with
                | Ok () -> Ok m
                | Error e -> Error e
            | NonGuaranteedDelivery -> Ok m
        | Error e -> e |> MessageDeliveryErr |> MessagingServiceErr |> Error


    let onFinishTransmitting (s : MessagingClientStateData) e (t : TransmissionData) =
        let received = t.receivedMessages
        let sent = t.sentMessages |> List.map (fun e -> e.messageDataInfo.messageId)
        let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageDataInfo.messageId) |> Set.ofList
        let notSent = Set.difference outgoing (sent |> Set.ofList)
        let remaining = s.outgoingMessages |> List.filter (fun e -> notSent.Contains e.messageDataInfo.messageId) |> sortOutgoing
        let x = { s with outgoingMessages = remaining; incomingMessages = (s.incomingMessages @ received) |> sortIncoming }
        x, e


    let onConfigureClient s _ =
        s


    let onTryPeekReceivedMessage (s : MessagingClientStateData) (r : AsyncReplyChannel<Message option>) =
        let x = s.incomingMessages |> List.tryHead
        r.Reply x
        s


    let onTryRemoveReceivedMessage deleteMessage (s : MessagingClientStateData) m (r : AsyncReplyChannel<TryRemoveReceivedMessageResult>) =
        let removedMessage e =
            match e with
            | None -> RemovedSucessfully
            | Some e -> RemovedWithError e
            |> r.Reply

            { s with incomingMessages = s.incomingMessages |> List.filter (fun e -> e.messageDataInfo.messageId <> m) |> sortIncoming }

        let failedToRemove e =
            r.Reply (FailedToRemove e)
            s

        match s.incomingMessages |> List.tryFind (fun e -> e.messageDataInfo.messageId = m) with
        | Some msg ->
            match msg.messageDataInfo.recipientInfo.deliveryType with
            | GuaranteedDelivery ->
                match deleteMessage m with
                | Ok() -> removedMessage None
                | Error e -> removedMessage (Some e)
            | NonGuaranteedDelivery -> removedMessage None
        | None -> m.value |> MessageNotFoundError |> MessageNotFoundErr |> MessagingClientErr |> failedToRemove


    let onStart (s : MessagingClientStateData) loadMessages (r : AsyncReplyChannel<ClmError option>) =
        let w, f =
            match s.messagingClientState with
            | MsgCliNotStarted ->
                match loadMessages() with
                | Ok messages ->
                    let m, e = messages |> Rop.unzip
                    let incoming = m |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
                    let outgoing = m |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)
                    let error = foldErrors e

                    //let eventHandler _ =
                    //    printfn "%s: Transmitting messages..." onTransmittingName
                    //    w.startTransmitting()

                    //let h = new EventHandler(EventHandlerInfo.defaultValue (s.logExn onTransmittingName) eventHandler)
                    //do h.start()

                    //let eventHandler1 _ =
                    //    w.removeExpiredMessages()

                    //let h1 = new EventHandler(EventHandlerInfo.oneHourValue (d.logger.logExn onStartName) eventHandler1)
                    //do h1.start()

                    { s with
                        messagingClientState = MsgCliIdle
                        outgoingMessages = (s.outgoingMessages @ outgoing) |> sortOutgoing
                        incomingMessages = (s.incomingMessages @ incoming) |> sortIncoming
                    }, error
                | Error e -> s, e |> Some
            | MsgCliIdle -> s, None

        r.Reply f
        w


    let onTransmitMessages proxy s (r : AsyncReplyChannel<ClmError option>) =
        let (w, f) =
            match s.messagingClientState with
            | MsgCliNotStarted -> s, None
            | MsgCliIdle ->
                // Note that we need to apply List.rev to get to the first (the oldest) message in the outgoing queue.
                let sent, sentErrors =
                    s.outgoingMessages
                    |> List.rev
                    |> List.map (sendMessageImpl proxy.sendMessage proxy.deleteMessage)
                    |> Rop.unzip

                let received, receivedErrors =
                    match receiveMessagesImpl proxy with
                    | Ok r -> r |> Rop.unzip
                    | Error e -> [], [ e ]

                let e = sentErrors @ receivedErrors |> foldErrors
                { receivedMessages = received; sentMessages = sent } |> (onFinishTransmitting s e)

        r.Reply f
        w


    let removeExpired deleteMessage expirationTime (r : List<Message>) =
        let expired, notExpired = r |> List.partition (fun e -> e.isExpired expirationTime)

        let err =
            expired
            |> List.map (fun e -> deleteMessage e.messageDataInfo.messageId)
            |> List.map (fun (e : UnitResult) -> match e with | Ok() -> None | Error e -> Some e)
            |> List.choose id

        notExpired, err


    let onRemoveExpiredMessages deleteMessage (s : MessagingClientStateData) (r :  AsyncReplyChannel<ClmError option>) =
        let removeExpired = removeExpired deleteMessage s.expirationTime
        let o = s.outgoingMessages |> removeExpired
        let i = s.incomingMessages |> removeExpired
        let e = (snd o) @ (snd i) |> foldErrors
        r.Reply e
        { s with outgoingMessages = fst o; incomingMessages = fst i }


    type MessagingClient(d : MessagingClientData) =
        let loadMsg = d.msgClientProxy.loadMessages
        let saveMsg = d.msgClientProxy.saveMessage
        let deleteMsg = d.msgClientProxy.deleteMessage
        let msgClietnId = d.msgAccessInfo.msgClientId

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start r -> return! onStart s loadMsg r |> loop
                            | GetVersion r -> return! onGetVersion s r |> loop
                            | SendMessage (m, r) -> return! onSendMessage saveMsg msgClietnId s m r |> loop
                            | TransmitMessages (p, r) -> return! onTransmitMessages p s r |> loop
                            | ConfigureClient x -> return! onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! onTryRemoveReceivedMessage deleteMsg s m r |> loop
                            | RemoveExpiredMessages r -> return! onRemoveExpiredMessages deleteMsg s r |> loop
                        }

                MessagingClientStateData.defaultValue |> loop
                )


        member _.start() = messageLoop.PostAndReply Start
        member _.getVersion() = messageLoop.PostAndReply GetVersion
        member _.sendMessage (m : MessageInfo) = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))
        member _.configureClient x = ConfigureClient x |> messageLoop.Post
        member m.transmitMessages() = messageLoop.PostAndReply (fun reply -> TransmitMessages (m.tryReceiveSingleMessageProxy, reply))
        member _.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member _.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member _.removeExpiredMessages() = messageLoop.PostAndReply RemoveExpiredMessages


        member m.messageProcessorProxy : MessageProcessorProxy =
            {
                tryPeekReceivedMessage = m.tryPeekReceivedMessage
                tryRemoveReceivedMessage = m.tryRemoveReceivedMessage
            }


        member _.tryReceiveSingleMessageProxy : TryReceiveSingleMessageProxy =
            {
                saveMessage = d.msgClientProxy.saveMessage
                deleteMessage = d.msgClientProxy.deleteMessage
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
                    | e -> e |> TryProcessMessageErr |> MessagingClientErr |> FailedToProcess
                | None -> NothingToDo
            else BusyProcessing
    
        Interlocked.Decrement(&callCount) |> ignore
        retVal

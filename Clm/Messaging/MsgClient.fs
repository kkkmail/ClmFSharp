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
    let maxNumberOfMediumMessages = 100
    let maxNumberOfLargeMessages = 10

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


    type MessagingClientStateData =
        {
            expirationTime : TimeSpan
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                expirationTime = TimeSpan(6, 0, 0)
            }


    type MessagingClientMessage =
        | SendMessage of AsyncReplyChannel<UnitResult> * MessageInfo
        | ScheduleMessage of MessageInfo
        | TryPeekReceivedMessage of AsyncReplyChannel<ClmResult<Message option>>
        | TryRemoveReceivedMessage of AsyncReplyChannel<UnitResult> * MessageId


    type TryReceiveSingleMessageProxy =
        {
            saveMessage : Message -> UnitResult
            tryDeleteMessage : MessageId -> UnitResult
            tryPeekMessage : unit -> ClmResult<Message option>
            tryDeleteFromServer : MessageId -> UnitResult
        }


    let tryReceiveSingleMessage (proxy : TryReceiveSingleMessageProxy) =
        //printfn "tryReceiveSingleMessage: Starting..."
        let addError = addError TryReceiveSingleMessageErr

        let result =
            match proxy.tryPeekMessage () with
            | Ok (Some m) ->
                //printfn "tryReceiveSingleMessage: Received message with messageId = %A, sent by %A to %A." m.messageDataInfo.messageId m.messageDataInfo.sender m.messageDataInfo.recipientInfo.recipient

                match proxy.saveMessage m with
                | Ok() ->
                    match proxy.tryDeleteFromServer m.messageDataInfo.messageId with
                    | Ok() -> m.messageData.getMessageSize() |> Some |> Ok
                    | Error e ->
                        match proxy.tryDeleteMessage m.messageDataInfo.messageId with
                        | Ok() -> addError TryDeleteFromServerErr e
                        | Error e1 -> addError TryDeleteFromServerErr (e1 + e)
                | Error e -> addError SaveMessageErr e
            | Ok None -> Ok None
            | Error e -> addError TryPeekMessageErr e

        //printfn "tryReceiveSingleMessage: Completed."
        result


    let mapper transmitter (c : MessageCount) =
        match c.canProcess with
        | true ->
            match transmitter() with
            | Ok None -> None |> Ok
            | Ok (Some SmallSize) -> c.onSmallMessage() |> Some |> Ok
            | Ok (Some MediumSize) -> c.onMediumMessage() |> Some |> Ok
            | Ok (Some LargeSize) -> c.onLargeMessage() |> Some |> Ok
            | Error e -> Error e
        | false -> None |> Ok


    let tryTransmitMessages transmitter =
        let rec doTryTransmit x c =
            match x with
            | [] -> Ok()
            | _ :: t ->
                match mapper transmitter c with
                | Ok (Some c1) -> doTryTransmit t c1
                | Ok None -> Ok()
                | Error e -> Error e

        let y = doTryTransmit MessagingClientStateData.maxMessages MessageCount.defaultValue
        y


    let tryReceiveMessages proxy = tryTransmitMessages (fun () -> tryReceiveSingleMessage proxy)


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


    let onSendMessage saveMessage msgClientId m = createMessage msgClientId m |> saveMessage


    type TrySendSingleMessageProxy =
        {
            tryPickOutgoingMessage : unit -> ClmResult<Message option>
            tryDeleteMessage : MessageId -> UnitResult
            sendMessage : Message -> UnitResult
        }


    let trySendSingleMessage (proxy : TrySendSingleMessageProxy) =
        match proxy.tryPickOutgoingMessage() with
        | Ok None -> Ok None
        | Ok (Some m) ->
            match proxy.sendMessage m with
            | Ok() ->
                match proxy.tryDeleteMessage m.messageDataInfo.messageId with
                | Ok() -> m.messageData.getMessageSize() |> Some |> Ok
                | Error e -> Error e
            | Error e -> Error e
        | Error e -> Error e


    let trySendMessages proxy = tryTransmitMessages (fun () -> trySendSingleMessage proxy)


    type MessagingClient(d : MessagingClientData) =
        let proxy = d.msgClientProxy
        let saveMsg = proxy.saveMessage
        let msgClientId = d.msgAccessInfo.msgClientId

        let receiveProxy =
            {
                saveMessage = proxy.saveMessage
                tryDeleteMessage = proxy.tryDeleteMessage
                tryPeekMessage = fun () -> d.messagingService.tryPeekMessage msgClientId
                tryDeleteFromServer = fun m -> d.messagingService.tryDeleteFromServer (msgClientId, m)
            }

        let sendProxy =
            {
                tryPickOutgoingMessage = proxy.tryPickOutgoingMessage
                tryDeleteMessage = proxy.tryDeleteMessage
                sendMessage = d.messagingService.sendMessage
            }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : MessagingClientStateData) =
                    async
                        {
                            match! u.Receive() with
                            | SendMessage (r, m) -> return! (s, onSendMessage saveMsg msgClientId m) |> (withReply r) |> loop
                            | ScheduleMessage m -> return! (s, onSendMessage saveMsg msgClientId m) |> withoutReply |> loop
                            | TryPeekReceivedMessage r -> return! (s, proxy.tryPickIncomingMessage()) |> (withReply r) |> loop
                            | TryRemoveReceivedMessage (r, m) -> return! (s, proxy.tryDeleteMessage m) |> (withReply r) |> loop
                        }

                MessagingClientStateData.defaultValue |> loop
                )

        /// Verifies that we have access to the relevant database and removes all expired messages.
        member m.start() = m.removeExpiredMessages()

        /// Sends a message and waits for confirmation that it was sent.
        /// If failed then the error in Result will contain the error.
        member _.sendMessage (m : MessageInfo) = messageLoop.PostAndReply (fun reply -> SendMessage (reply, m))

        /// Schedules a message to be sent and returns immediately.
        /// If something fails at a later stage, then no error will be recorded.
        member _.scheduleMessage (m : MessageInfo) = ScheduleMessage m |> messageLoop.Post

        member _.tryPeekReceivedMessage() = messageLoop.PostAndReply (fun reply -> TryPeekReceivedMessage reply)
        member _.tryRemoveReceivedMessage m = messageLoop.PostAndReply (fun reply -> TryRemoveReceivedMessage (reply, m))
        member _.transmitMessages() : UnitResult = [ tryReceiveMessages receiveProxy; trySendMessages sendProxy ] |> foldUnitResults
        member m.removeExpiredMessages() : UnitResult = proxy.deleteExpiredMessages MessagingClientStateData.defaultValue.expirationTime


        member m.messageProcessorProxy : MessageProcessorProxy =
            {
                start = m.start
                tryPeekReceivedMessage = m.tryPeekReceivedMessage
                tryRemoveReceivedMessage = m.tryRemoveReceivedMessage
                sendMessage = m.sendMessage
                scheduleMessage = m.scheduleMessage
                transmitMessages = m.transmitMessages
                removeExpiredMessages = m.removeExpiredMessages
            }


        member _.tryReceiveSingleMessageProxy : TryReceiveSingleMessageProxy =
            {
                saveMessage = d.msgClientProxy.saveMessage
                tryDeleteMessage = d.msgClientProxy.tryDeleteMessage
                tryPeekMessage = fun () -> d.messagingService.tryPeekMessage d.msgAccessInfo.msgClientId
                tryDeleteFromServer = fun x -> d.messagingService.tryDeleteFromServer (d.msgAccessInfo.msgClientId, x)
            }


    let mutable private callCount = -1


    let onTryProcessMessage (w : MessageProcessorProxy) x f =
        let retVal =
            if Interlocked.Increment(&callCount) = 0
            then
                match w.tryPeekReceivedMessage() with
                | Ok (Some m) ->
                    try
                        let r = f x m

                        match w.tryRemoveReceivedMessage m.messageDataInfo.messageId with
                        | Ok() -> ProcessedSuccessfully r
                        | Error e -> ProcessedWithFailedToRemove (r, e)
                    with
                    | e -> e |> OnTryProcessMessageExn |> OnTryProcessMessageErr |> MessagingClientErr |> FailedToProcess
                | Ok None -> NothingToDo
                | Error e -> FailedToProcess e
            else BusyProcessing

        Interlocked.Decrement(&callCount) |> ignore
        retVal


    /// Call this function to create timer events necessary for automatic MessagingClient operation.
    /// If you don't call it, then you have to operate MessagingClient by hands.
    let createMessagingClientEventHandlers logger (w : MessageProcessorProxy) =
        let eventHandler _ = w.transmitMessages()
        let h = ClmEventHandlerInfo.defaultValue logger eventHandler "MessagingClient - transmitMessages" |> ClmEventHandler
        do h.start()

        let eventHandler1 _ = w.removeExpiredMessages()
        let h1 = ClmEventHandlerInfo.oneHourValue logger eventHandler1 "MessagingClient - removeExpiredMessages" |> ClmEventHandler
        do h1.start()

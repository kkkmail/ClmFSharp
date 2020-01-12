namespace Messaging

open System
open ClmSys
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.TimerEvents
open ClmSys.Logging
open ClmSys.GeneralErrors

module Service =

    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
            logger : Logger
        }


    type MessagingServiceState =
        {
            workState : MessagingWorkState
            messageServiceData : MessagingServiceData
            messages : Map<MessagingClientId, List<MessageWithOptionalData>>
            expirationTime : TimeSpan
        }

        member s.proxy = s.messageServiceData.messagingServiceProxy

        member s.getState() =
            {
                msgVersion = messagingDataVersion
                msgWorkState = s.workState
                msgInfo = s.messages |> Map.toList |> List.map (fun (k, v) -> k, v |> List.map (fun e -> e.messageDataInfo.messageId))
            }

        static member defaultValue d =
            {
                workState = MsgSvcNotStarted
                messageServiceData = d
                messages = Map.empty
                expirationTime = TimeSpan(6, 0, 0)
            }


    let private className = "MessagingService"
    let private getMethodName n = className + "." + n
    let private updateMessagesName = getMethodName "updateMessages"
    let private onStartName = getMethodName "onStart"
    let private onGetVersionName = getMethodName "onGetVersion"
    let private onSendMessageName = getMethodName "onSendMessage"
    let private onTryPeekMessageName = getMethodName "onTryPeekMessage"
    let private onTryDeleteFromServerName = getMethodName "onTryDeleteFromServer"
    let private onConfigureName = getMethodName "onConfigure"
    let private onGetStateName = getMethodName "onGetState"
    let private onRemoveExpiredMessagesName = getMethodName "onRemoveExpiredMessages"


    type MessagingServiceMessage =
        | Start of AsyncReplyChannel<ClmError option>
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<MessageDeliveryResult>
        | ConfigureService of MessagingConfigParam
        | GetState of AsyncReplyChannel<MsgServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<ClmResult<Message option>>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<UnitResult>
        | RemoveExpiredMessages


    let updateMessages s (m : Message) =
        let x =
            match s.messages.TryFind m.messageDataInfo.recipientInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, m.toMessageWithOptionalData() :: r) }
            | None -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, [ m.toMessageWithOptionalData() ]) }
        x


    let onStart (loadMessages : unit -> ListResult<Message>) (s : MessagingServiceState) (r : AsyncReplyChannel<ClmError option>) =
        let w, f =
            match s.workState with
            | MsgSvcNotStarted ->
                match loadMessages() with
                | Ok q ->
                    let v, e = q |> Rop.unzip

                    let x =
                        v
                        |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn) // The newest message WILL BE at the head after we add them to the list starting from the oldest first.
                        |> List.fold (fun acc e -> updateMessages acc e) s

                //let eventHandler _ = w.removeExpiredMessages()
                //let h = new EventHandler(EventHandlerInfo.oneHourValue (d.logger.logExn onStartName) eventHandler)
                //do h.start()

                    { x with workState = CanTransmitMessages }, foldErrors e
                | Error e -> s, Some e
            | CanTransmitMessages | ShuttingDown -> s, None

        r.Reply f
        w


    let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
        printfn "%s" onGetVersionName
        r.Reply messagingDataVersion
        s


    let onSendMessage saveMessage (s : MessagingServiceState) (m : Message) (r : AsyncReplyChannel<MessageDeliveryResult>) =
        let w, f =
            match m.messageDataInfo.dataVersion.value = messagingDataVersion.value with
            | true ->
                match s.workState with
                | MsgSvcNotStarted -> s, ServiceNotStarted |> Error
                | ShuttingDown -> s, ServerIsShuttingDown |> Error
                | CanTransmitMessages ->
                    let err =
                        match m.messageDataInfo.recipientInfo.deliveryType, m.messageData.keepInMemory() with
                        | GuaranteedDelivery, _ | NonGuaranteedDelivery, false -> saveMessage m
                        | NonGuaranteedDelivery, true -> Ok()

                    updateMessages s m, err
            | false -> s, messagingDataVersion |> DataVersionMismatch |> Error

        r.Reply f
        w

    let onConfigure s x =
        match x with
        | MsgWorkState w ->
            match w with
            | MsgSvcNotStarted -> s // Cannot change the state to not started.
            | CanTransmitMessages | ShuttingDown -> { s with workState = w }


    let onGetState (s : MessagingServiceState) (r : AsyncReplyChannel<MsgServiceState>) =
        s.getState() |> r.Reply
        s


    let onTryPeekMessage tryLoadMessage s n (r : AsyncReplyChannel<ClmResult<Message option>>) =
        let reply, w =
            match s.messages.TryFind n with
            | Some v ->
                // Note that we need to apply List.rev to get to the first message.
                match List.rev v with
                | [] -> Ok None, s
                | h :: t ->
                    match h.toMessasge() with
                    | Some m -> Ok(Some m), s
                    | None ->
                        match tryLoadMessage h.messageDataInfo.messageId with
                        | Ok(Some m) -> Ok(Some m), s
                        | Ok None ->
                            let err = (n.value, h.messageDataInfo.messageId.value) |> UnableToLoadMessageError |> TryPeekMessageErr |> MessagingServiceErr |> Error
                            err, { s with messages = s.messages.Add(n, t |> List.rev) }
                        | Error e -> Error e, s
            | None -> Ok None, s

        r.Reply reply
        w


    let onTryDeleteFromServer deleteMessage s n m (r : AsyncReplyChannel<bool>) =
        match s.messages.TryFind n with
        | Some v ->
            let x = removeFirst (fun e -> e.messageDataInfo.messageId = m) v
            r.Reply (x.Length <> v.Length)
            deleteMessage m
            { s with messages = s.messages.Add(n, x) }
        | None ->
            printfn "    %s: Cannot find client for ClientId %A." onTryDeleteFromServerName n
            r.Reply false
            s


    let onRemoveExpiredMessages (s : MessagingServiceState) =
        let removeExpired (r : List<MessageWithOptionalData>) =
            let expired, notExpired = r |> List.partition (fun e -> e.isExpired s.expirationTime)
            expired |> List.map (fun e -> s.proxy.deleteMessage e.messageDataInfo.messageId) |> ignore
            notExpired

        { s with messages = s.messages |> Map.toList |> List.map (fun (n, r) -> (n, removeExpired r)) |> Map.ofList}




    type MessagingService(d : MessagingServiceData) =
        let loadMsg = d.messagingServiceProxy.loadMessages

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! (timed onStartName onStart s w |> loop)
                            | GetVersion r -> return! (timed onGetVersionName onGetVersion s r |> loop)
                            | SendMessage (m, r) -> return! (timed onSendMessageName onSendMessage s m r |> loop)
                            | ConfigureService x -> return! (timed onConfigureName onConfigure s x |> loop)
                            | GetState r -> return! (timed onGetStateName onGetState s r |> loop)
                            | TryPeekMessage (n, r) -> return! (timed onTryPeekMessageName onTryPeekMessage s n r |> loop)
                            | TryDeleteFromServer (n, m, r) -> return! (timed onTryDeleteFromServerName onTryDeleteFromServer s n m r |> loop)
                            | RemoveExpiredMessages -> return! (timed onRemoveExpiredMessagesName onRemoveExpiredMessages s |> loop)
                        }

                (MessagingServiceState.defaultValue d) |> loop
                )

        member this.start() = Start this |> messageLoop.Post
        member __.getVersion() = GetVersion |> messageLoop.PostAndReply |> Ok
        member __.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))

        member __.configureService x =
            ConfigureService x |> messageLoop.Post
            Ok ServiceConfigured

        member __.getState() = GetState |> messageLoop.PostAndReply |> Ok
        member __.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply)) |> Ok
        member __.tryDeleteFromServer (n, m) = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply)) |> Ok
        member private __.removeExpiredMessages() = RemoveExpiredMessages |> messageLoop.Post

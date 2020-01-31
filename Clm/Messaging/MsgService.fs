namespace Messaging

open System
open ClmSys
open ClmSys.VersionInfo
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives
open ClmSys.MessagingServiceErrors

module Service =

    let private toError f = f |> MessagingServiceErr |> Error
    let private addError g f e = ((f |> g |> MessagingServiceErr) + e) |> Error


    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
        }


    type MessagingServiceState =
        {
            workState : MessagingWorkState
            messages : Map<MessagingClientId, List<MessageWithOptionalData>>
            expirationTime : TimeSpan
        }

        member s.getState() =
            {
                msgVersion = messagingDataVersion
                msgWorkState = s.workState
                msgInfo = s.messages |> Map.toList |> List.map (fun (k, v) -> k, v |> List.map (fun e -> e.messageDataInfo.messageId))
            }

        static member defaultValue =
            {
                workState = MsgSvcNotStarted
                messages = Map.empty
                expirationTime = TimeSpan(6, 0, 0)
            }


    type OnStartProxy =
        {
            loadMessages : unit -> ListResult<Message>
            updateMessages : MessagingServiceState -> Message -> MessagingServiceState
        }


    type MessagingServiceMessage =
        | Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<UnitResult>
        | ConfigureService of MessagingConfigParam
        | GetState of AsyncReplyChannel<MsgServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<ClmResult<Message option>>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<UnitResult>
        | RemoveExpiredMessages of AsyncReplyChannel<UnitResult>


    let updateMessages s (m : Message) =
        let x =
            match s.messages.TryFind m.messageDataInfo.recipientInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, m.toMessageWithOptionalData() :: r) }
            | None -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, [ m.toMessageWithOptionalData() ]) }
        x


    let onStart (proxy : OnStartProxy) (s : MessagingServiceState) (r : AsyncReplyChannel<UnitResult>) =
        let w, f =
            match s.workState with
            | MsgSvcNotStarted ->
                match proxy.loadMessages() with
                | Ok q ->
                    let v, e = q |> Rop.unzip

                    let x =
                        v
                        |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn) // The newest message WILL BE at the head after we add them to the list starting from the oldest first.
                        |> List.fold (fun acc e -> proxy.updateMessages acc e) s

                //let eventHandler _ = w.removeExpiredMessages()
                //let h = new EventHandler(EventHandlerInfo.oneHourValue (d.logger.logExn onStartName) eventHandler)
                //do h.start()

                    { x with workState = CanTransmitMessages }, foldToUnitResult e
                | Error e -> s, Error e
            | CanTransmitMessages | ShuttingDown -> s, Ok()

        r.Reply f
        w


    let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
        r.Reply messagingDataVersion
        s


    /// kk:20200120 - If it gets to the point that it is possible to handle some specific errors, then
    /// create a proxy instead of saveMessage and then add an "error handler" to the proxy.
    /// The "error handler" should be "responsible" for whatever it can when handling lower level errors.
    let onSendMessage saveMessage (s : MessagingServiceState) (m : Message) (r : AsyncReplyChannel<UnitResult>) =
        let w, f =
            match m.messageDataInfo.dataVersion.value = messagingDataVersion.value with
            | true ->
                match s.workState with
                | MsgSvcNotStarted -> s, ServiceNotStartedErr |> MessageDeliveryErr |> toError
                | ShuttingDown -> s, ServerIsShuttingDownErr |> MessageDeliveryErr |> toError
                | CanTransmitMessages ->
                    let err =
                        match m.messageDataInfo.recipientInfo.deliveryType, m.messageData.keepInMemory() with
                        | GuaranteedDelivery, _ | NonGuaranteedDelivery, false -> saveMessage m
                        | NonGuaranteedDelivery, true -> Ok()

                    updateMessages s m, err
            | false -> s, messagingDataVersion |> DataVersionMismatchErr |> MessageDeliveryErr |> toError

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
        let w, f =
            match s.messages |> Map.tryFind n with
            | Some v ->
                // Note that we need to apply List.rev to get to the first message.
                match List.rev v with
                | [] -> s, Ok None
                | h :: t ->
                    match h.toMessasge() with
                    | Some m -> s, Ok(Some m)
                    | None ->
                        match tryLoadMessage h.messageDataInfo.messageId with
                        | Ok m -> s, Ok(Some m)
                        | Error e ->
                            let err = (n.value, h.messageDataInfo.messageId.value) |> UnableToLoadMessageError |> TryPeekMessageErr |> MessagingServiceErr
                            // Remove the message as we cannot load it.
                            { s with messages = s.messages.Add(n, t |> List.rev) }, Error (err + e)
            | None -> s, Ok None

        r.Reply f
        w


    let onTryDeleteFromServer deleteMessage s n m (r : AsyncReplyChannel<UnitResult>) =
        let w, f =
            match s.messages.TryFind n with
            | Some v ->
                let x = removeFirst (fun e -> e.messageDataInfo.messageId = m) v
                let z() = { s with messages = s.messages.Add(n, x) }
                let f() = (n.value, m.value) |> UnableToDeleteMessageErr |> TryDeleteFromServerErr |> MessagingServiceErr

                match x.Length <> v.Length, deleteMessage m with
                | true, Ok() -> z(), Ok()
                | false, Ok() -> z(), f() |> Error
                | true, Error e -> z(), Error e
                | false, Error e -> z(), e + f() |> Error
            | None -> s, n.value |> CannotFindClientErr |> TryDeleteFromServerErr |> toError

        r.Reply f
        w


    let removeExpired deleteMessage expirationTime (r : List<MessageWithOptionalData>) =
        let expired, notExpired = r |> List.partition (fun e -> e.isExpired expirationTime)

        let (_, f) =
            expired
            |> List.map (fun e -> deleteMessage e.messageDataInfo.messageId)
            |> Rop.unzip

        notExpired, f |> foldErrors


    let onRemoveExpiredMessages deleteMessage (s : MessagingServiceState) (r : AsyncReplyChannel<UnitResult>) =
        let m, fo =
            s.messages
            |> Map.toList
            |> List.map (fun (n, r) -> (n, removeExpired deleteMessage s.expirationTime r))
            |> List.map (fun (n, (m, f)) -> (n, m), f)
            |> List.unzip

        let f = fo |> List.choose id |> foldErrors |> Option.bind (fun e -> e |> Error |> Some) |> (Option.defaultValue (Ok()))
        r.Reply f
        { s with messages = m |> Map.ofList }


    type MessagingService(d : MessagingServiceData) =
        let deleteMsg = d.messagingServiceProxy.deleteMessage
        let tryLoadMsg = d.messagingServiceProxy.tryLoadMessage
        let saveMsg = d.messagingServiceProxy.saveMessage

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (p, r) -> return! onStart p s r |> loop
                            | GetVersion r -> return! onGetVersion s r |> loop
                            | SendMessage (m, r) -> return! onSendMessage saveMsg s m r |> loop
                            | ConfigureService x -> return! onConfigure s x |> loop
                            | GetState r -> return! onGetState s r |> loop
                            | TryPeekMessage (n, r) -> return! onTryPeekMessage tryLoadMsg s n r |> loop
                            | TryDeleteFromServer (n, m, r) -> return! onTryDeleteFromServer deleteMsg s n m r |> loop
                            | RemoveExpiredMessages r -> return! onRemoveExpiredMessages deleteMsg s r |> loop
                        }

                MessagingServiceState.defaultValue |> loop
                )

        member m.start() = messageLoop.PostAndReply (fun reply -> Start (m.onStartProxy, reply))
        member _.getVersion() = GetVersion |> messageLoop.PostAndReply |> Ok
        member _.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))

        member _.configureService x =
            ConfigureService x |> messageLoop.Post
            Ok()

        member _.getState() = GetState |> messageLoop.PostAndReply |> Ok
        member _.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply)) |> Ok
        member _.tryDeleteFromServer (n, m) = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply)) |> Ok
        member _.removeExpiredMessages() = messageLoop.PostAndReply RemoveExpiredMessages

        member _.onStartProxy =
            {
                loadMessages = d.messagingServiceProxy.loadMessages
                updateMessages = updateMessages
            }

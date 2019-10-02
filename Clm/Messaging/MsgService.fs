namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.TimerEvents
open ClmSys.Logging

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
                workState = CanTransmitMessages
                messageServiceData = d
                messages = Map.empty
                expirationTime = TimeSpan(6, 0, 0)
            }


    type MessagingServiceMessage =
        | Start of MessagingService
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<MessageDeliveryResult>
        | ConfigureService of MessagingConfigParam
        | GetState of AsyncReplyChannel<MsgServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<Message option>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<bool>
        | RemoveExpiredMessages


    and MessagingService(d : MessagingServiceData) =
        let className = "MessagingService"
        let getMethodName n = className + "." + n
        let updateMessagesName = getMethodName "updateMessages"
        let onStartName = getMethodName "onStart"
        let onGetVersionName = getMethodName "onGetVersion"
        let onSendMessageName = getMethodName "onSendMessage"
        let onTryPeekMessageName = getMethodName "onTryPeekMessage"
        let onTryTryDeleteFromServerName = getMethodName "onTryTryDeleteFromServer"
        let onConfigureName = getMethodName "onConfigure"
        let onGetStateName = getMethodName "onGetState"
        let onRemoveExpiredMessagesName = getMethodName "onRemoveExpiredMessages"


        let updateMessages s (m : Message) =
            printfn "%s: Updating with mesageid: %A ..." updateMessagesName m.messageDataInfo.messageId

            let x =
                match s.messages.TryFind m.messageDataInfo.recipientInfo.recipient with
                | Some r -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, m.toMessageWithOptionalData() :: r) }
                | None -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, [ m.toMessageWithOptionalData() ]) }

            printfn "%s: Updated with mesageid: %A." updateMessagesName m.messageDataInfo.messageId
            x

        let onStart (s : MessagingServiceState) (w : MessagingService) =
            match s.workState with
            | MsgSvcNotStarted ->
                printfn "%s" onStartName

                let x =
                    s.proxy.loadMessages()
                    |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn) // The newest message WILL BE at the head after we add them to the list starting from the oldest first.
                    |> List.fold (fun acc e -> updateMessages acc e) s

                let eventHandler _ =
                    w.removeExpiredMessages()

                let h = new EventHandler(EventHandlerInfo.oneHourValue (d.logger.logExn onStartName) eventHandler)
                do h.start()

                x
            | CanTransmitMessages -> s
            | ShuttingDown -> s


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "%s" onGetVersionName
            r.Reply messagingDataVersion
            s


        let onSendMessage (s : MessagingServiceState) (m : Message) (r : AsyncReplyChannel<MessageDeliveryResult>) =
            printfn "%s: Sending messageId = %A ..." onSendMessageName m.messageDataInfo.messageId

            match m.messageDataInfo.dataVersion = messagingDataVersion with
            | true ->
                match s.workState with
                | MsgSvcNotStarted ->
                    d.logger.logErr (sprintf "%s: Service must be started to send messages." onSendMessageName)
                    s
                | CanTransmitMessages ->
                    match m.messageDataInfo.recipientInfo.deliveryType, m.messageData.keepInMemory with
                    | GuaranteedDelivery, _ | NonGuaranteedDelivery, true -> s.proxy.saveMessage m
                    | NonGuaranteedDelivery, false -> ignore()

                    r.Reply DeliveredSuccessfully
                    printfn "%s: Sent messageId = %A." onSendMessageName m.messageDataInfo.messageId
                    updateMessages s m
                | ShuttingDown ->
                    r.Reply ServerIsShuttingDown
                    s
            | false ->
                r.Reply (DataVersionMismatch messagingDataVersion)
                s


        let onConfigure s x =
            match x with
            | MsgWorkState w -> { s with workState = w }


        let onGetState (s : MessagingServiceState) (r : AsyncReplyChannel<MsgServiceState>) =
            s.getState() |> r.Reply
            s


        let onTryPeekMessage s n (r : AsyncReplyChannel<Message option>) =
            printfn "%s: ClientId: %A" onTryPeekMessageName n

            let reply, w =
                match s.messages.TryFind n with
                | Some v ->
                    // Note that we need to apply List.rev to get to the first message.
                    printfn "    %s: v: %A" onTryPeekMessageName v
                    match List.rev v with
                    | [] ->
                        printfn "%s: No messages." onTryPeekMessageName
                        None, s
                    | h :: t ->
                        printfn "%s: Found message with id %A." onTryPeekMessageName h.messageDataInfo.messageId
                        match h.toMessasge() with
                        | Some m -> Some m, s
                        | None ->
                            match s.proxy.tryLoadMessage h.messageDataInfo.messageId with
                            | Some m -> Some m, s
                            | None ->
                                d.logger.logErr (sprintf "%s: Cannot find message data for id: %A" onTryPeekMessageName h.messageDataInfo.messageId)
                                None, { s with messages = s.messages.Add(n, t |> List.rev) }
                | None ->
                    printfn "%s: No client for ClientId %A." onTryPeekMessageName n
                    None, s

            printfn "%s: Replying with message id: %A ..." onTryPeekMessageName (reply |> Option.bind (fun e -> Some e.messageDataInfo.messageId))
            r.Reply reply
            printfn "%s: Replied with message id: %A." onTryPeekMessageName (reply |> Option.bind (fun e -> Some e.messageDataInfo.messageId))
            w


        let onTryTryDeleteFromServer s n m (r : AsyncReplyChannel<bool>) =
            printfn "%s: ClientId: %A, MessageId: %A" onTryTryDeleteFromServerName n m

            match s.messages.TryFind n with
            | Some v ->
                printfn "    %s: v.Length: %A" onTryTryDeleteFromServerName v.Length
                let x = removeFirst (fun e -> e.messageDataInfo.messageId = m) v
                printfn "    %s: x.Length: %A" onTryTryDeleteFromServerName x.Length
                r.Reply (x.Length <> v.Length)
                s.proxy.deleteMessage m
                { s with messages = s.messages.Add(n, x) }
            | None ->
                printfn "    %s: Cannot find client for ClientId %A." onTryTryDeleteFromServerName n
                r.Reply false
                s


        let onRemoveExpiredMessages (s : MessagingServiceState) =
            let removeExpired (r : List<MessageWithOptionalData>) =
                let expired, notExpired = r |> List.partition (fun e -> e.isExpired s.expirationTime)
                expired |> List.map (fun e -> s.proxy.deleteMessage e.messageDataInfo.messageId) |> ignore
                notExpired

            { s with messages = s.messages |> Map.toList |> List.map (fun (n, r) -> (n, removeExpired r)) |> Map.ofList}


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
                            | TryDeleteFromServer (n, m, r) -> return! (timed onTryTryDeleteFromServerName onTryTryDeleteFromServer s n m r |> loop)
                            | RemoveExpiredMessages -> return! (timed onRemoveExpiredMessagesName onRemoveExpiredMessages s |> loop)
                        }

                (MessagingServiceState.defaultValue d) |> loop
                )

        member this.start() = Start this |> messageLoop.Post
        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))
        member __.configureService x = ConfigureService x |> messageLoop.Post
        member __.getState() = GetState |> messageLoop.PostAndReply
        member __.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply))
        member __.tryDeleteFromServer n m = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply))
        member private __.removeExpiredMessages() = RemoveExpiredMessages |> messageLoop.Post

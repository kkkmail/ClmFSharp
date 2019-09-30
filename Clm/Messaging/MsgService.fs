namespace Messaging

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
            messages : Map<MessagingClientId, List<Message>>
        }

        member s.proxy = s.messageServiceData.messagingServiceProxy

        member s.state =
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
        let onStartName = getMethodName "onStart"
        let onStartNameName = getMethodName "onStartName"


        let updateMessages s (m : Message) =
            match s.messages.TryFind m.messageDataInfo.recipientInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, [ m ]) }


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

                let h = new EventHandler(EventHandlerInfo.defaultValue (d.logger.logExn onStartName) eventHandler)
                do h.start()

                x
            | CanTransmitMessages -> s
            | ShuttingDown -> s


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "MessagingService.onGetVersion"
            r.Reply messagingDataVersion
            s


        let onSendMessage (s : MessagingServiceState) (m : Message) (r : AsyncReplyChannel<MessageDeliveryResult>) =
            printfn "MessagingService.onSendMessage: messageId = %A." m.messageDataInfo.messageId

            match m.messageDataInfo.dataVersion = messagingDataVersion with
            | true ->
                match s.workState with
                | MsgSvcNotStarted ->
                    d.logger.logErr (sprintf "%s: Service must be started to send messages." onStartNameName)
                    s
                | CanTransmitMessages ->
                    match m.messageDataInfo.recipientInfo.deliveryType with
                    | GuaranteedDelivery -> s.proxy.saveMessage m
                    | NonGuaranteedDelivery -> ignore()

                    r.Reply DeliveredSuccessfully
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
            r.Reply s.state
            s


        let onTryPeekMessage s n (r : AsyncReplyChannel<Message option>) =
            printfn "MessagingService.onTryPeekMessage: ClientId: %A" n

            let reply =
                match s.messages.TryFind n with
                | Some v ->
                    // Note that we need to apply List.rev to get to the first message.
                    printfn "    MessagingService.onTryPeekMessage: v: %A" v
                    match List.rev v with
                    | [] ->
                        printfn "MessagingService.onTryPeekMessage: No messages."
                        None
                    | h :: _ ->
                        printfn "MessagingService.onTryPeekMessage: Found message with id %A." h.messageDataInfo.messageId
                        Some h
                | None ->
                    printfn "MessagingService.onTryPeekMessage: No client for ClientId %A." n
                    None

            r.Reply reply
            s


        let onTryTryDeleteFromServer s n m (r : AsyncReplyChannel<bool>) =
            printfn "MessagingService.onTryTryDeleteFromServer: ClientId: %A, MessageId: %A" n m

            match s.messages.TryFind n with
            | Some v ->
                printfn "    MessagingService.onTryTryDeleteFromServer: v.Length: %A" v.Length
                let x = removeFirst (fun e -> e.messageDataInfo.messageId = m) v
                printfn "    MessagingService.onTryTryDeleteFromServer: x.Length: %A" x.Length
                r.Reply (x.Length <> v.Length)
                s.proxy.deleteMessage m
                { s with messages = s.messages.Add(n, x) }
            | None ->
                printfn "    MessagingService.onTryTryDeleteFromServer: Cannot find client for ClientId %A." n
                r.Reply false
                s


        let onRemoveExpiredMessages s =
            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! timed "MessagingService.onStart" onStart s w |> loop
                            | GetVersion r -> return! timed "MessagingService.onGetVersion" onGetVersion s r |> loop
                            | SendMessage (m, r) -> return! timed "MessagingService.onSendMessage" onSendMessage s m r |> loop
                            | ConfigureService x -> return! timed "MessagingService.onConfigure" onConfigure s x |> loop
                            | GetState r -> return! timed "MessagingService.onGetState" onGetState s r |> loop
                            | TryPeekMessage (n, r) -> return! timed "MessagingService.onTryPeekMessage" onTryPeekMessage s n r |> loop
                            | TryDeleteFromServer (n, m, r) -> return! timed "MessagingService.onTryTryDeleteFromServer" onTryTryDeleteFromServer s n m r |> loop
                            | RemoveExpiredMessages -> return! timed "MessagingService.onRemoveExpiredMessages" onRemoveExpiredMessages s |> loop

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

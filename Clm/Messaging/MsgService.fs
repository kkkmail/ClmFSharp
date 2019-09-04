namespace Messaging

open ClmSys.VersionInfo
open ClmSys.Rop
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy

module Service =

    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
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
                msgInfo = s.messages |> Map.toList |> List.map (fun (k, v) -> k, v |> List.map (fun e -> e.messageId))
            }

        static member defaultValue d =
            {
                workState = CanTransmitMessages
                messageServiceData = d
                messages = Map.empty
            }


    type MessagingServiceMessage =
        | Start
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<MessageDeliveryResult>
        //| GetMessages of MessagingClientId * AsyncReplyChannel<List<Message>>
        | ConfigureService of MessagingConfigParam
        | GetState of AsyncReplyChannel<MsgServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<Message option>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<bool>


    type MessagingService(d : MessagingServiceData) =
        let updateMessages s m =
            match s.messages.TryFind m.messageInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageInfo.recipient, [ m ]) }


        let onStart (s : MessagingServiceState) =
            printfn "MessagingService.onStart"
            s.proxy.loadMessages()
            |> List.fold (fun acc e -> updateMessages acc e) s


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "MessagingService.onGetVersion"
            r.Reply messagingDataVersion
            s


        let onSendMessage (s : MessagingServiceState) m (r : AsyncReplyChannel<MessageDeliveryResult>) =
            printfn "MessagingService.onSendMessage: messageId = %A." m.messageId

            match m.dataVersion = messagingDataVersion with
            | true ->
                match s.workState with
                | CanTransmitMessages ->
                    match m.messageInfo.deliveryType with
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

        //let onGetMessages (s : MessagingServiceState) n (r : AsyncReplyChannel<List<Message>>) =
        //    printfn "MessagingService.onGetMessages: ClientId: %A" n
        //    match s.messages.TryFind n with
        //    | Some v ->
        //        r.Reply (List.rev v)

        //        v
        //        |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
        //        |> List.map (fun e -> s.proxy.deleteMessage e.messageId)
        //        |> ignore

        //        { s with messages = s.messages.Add(n, []) }
        //    | None ->
        //        r.Reply []
        //        s


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
                    printfn "    MessagingService.onTryPeekMessage: v: %A" v
                    match List.rev v with
                    | [] ->
                        printfn "MessagingService.onTryPeekMessage: No messages."
                        None
                    | h :: _ ->
                        printfn "MessagingService.onTryPeekMessage: Found message with id %A." h.messageId
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
                let x = removeFirst (fun e -> e.messageId = m) v
                printfn "    MessagingService.onTryTryDeleteFromServer: x.Length: %A" x.Length
                r.Reply (x.Length <> v.Length)
                s.proxy.deleteMessage m
                { s with messages = s.messages.Add(n, x) }
            | None ->
                printfn "    MessagingService.onTryTryDeleteFromServer: Cannot find client for ClientId %A." n
                r.Reply false
                s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! timed "MessagingService.onStart" onStart s |> loop
                            | GetVersion r -> return! timed "MessagingService.onGetVersion" onGetVersion s r |> loop
                            | SendMessage (m, r) -> return! timed "MessagingService.onSendMessage" onSendMessage s m r |> loop
                            //| GetMessages (n, r) -> return! timed "MessagingService.onGetMessages" onGetMessages s n r |> loop
                            | ConfigureService x -> return! timed "MessagingService.onConfigure" onConfigure s x |> loop
                            | GetState r -> return! timed "MessagingService.onGetState" onGetState s r |> loop
                            | TryPeekMessage (n, r) -> return! timed "MessagingService.onTryPeekMessage" onTryPeekMessage s n r |> loop
                            | TryDeleteFromServer (n, m, r) -> return! timed "MessagingService.onTryTryDeleteFromServer" onTryTryDeleteFromServer s n m r |> loop

                        }

                onStart (MessagingServiceState.defaultValue d) |> loop
                )

        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))
        //member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))
        member __.configureService x = ConfigureService x |> messageLoop.Post
        member __.getState() = GetState |> messageLoop.PostAndReply
        member __.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply))
        member __.tryDeleteFromServer n m = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply))

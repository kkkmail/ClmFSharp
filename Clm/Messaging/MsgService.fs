namespace Messaging

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
            messageServiceData : MessagingServiceData
            messages : Map<MessagingClientId, List<Message>>
        }

        member s.proxy = s.messageServiceData.messagingServiceProxy

        static member defaultValue d =
            {
                messageServiceData = d
                messages = Map.empty
            }


    type MessagingServiceMessage =
        | Start
        | SendMessage of Message
        //| GetMessages of MessagingClientId * AsyncReplyChannel<List<Message>>
        | ConfigureService of MessagingConfigParam
        | GetState of AsyncReplyChannel<MessagingServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<Message option>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<bool>



    type MessagingService(d : MessagingServiceData) =
        let updateMessages s m =
            match s.messages.TryFind m.messageInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageInfo.recipient, [ m ]) }
            //| Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, enqueue r m) }
            //| None -> { s with messages = s.messages.Add (m.messageInfo.recipient, enqueue emptyQueue m) }


        let onStart (s : MessagingServiceState) =
            printfn "MessagingService.onStart"
            s.proxy.loadMessages()
            |> List.fold (fun acc e -> updateMessages acc e) s


        let onSendMessage (s : MessagingServiceState) m =
            printfn "MessagingService.onSendMessage: m = %A." m
            match m.messageInfo.deliveryType with
            | GuaranteedDelivery -> s.proxy.saveMessage m
            | NonGuaranteedDelivery -> ignore()

            updateMessages s m


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


        /// TODO kk:20190820 - Implement.
        let onConfigure s x =
            s


        let onGetState s (r : AsyncReplyChannel<MessagingServiceState>) =
            r.Reply s
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
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            //| GetMessages (n, r) -> return! onGetMessages s n r |> loop
                            | ConfigureService x -> return! onConfigure s x |> loop
                            | GetState r -> return! onGetState s r |> loop
                            | TryPeekMessage (n, r) -> return! onTryPeekMessage s n r |> loop
                            | TryDeleteFromServer (n, m, r) -> return! onTryTryDeleteFromServer s n m r |> loop

                        }

                onStart (MessagingServiceState.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        //member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))
        member __.configureService x = ConfigureService x |> messageLoop.Post
        member __.getState() = GetState |> messageLoop.PostAndReply
        member __.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply))
        member __.tryDeleteFromServer n m = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply))

namespace Messaging

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingServiceInfo.ServiceProxy

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

        static member defaultValue d =
            {
                messageServiceData = d
                messages = Map.empty
            }


    type MessagingServiceMessage =
        | Start
        | SendMessage of Message
        | GetMessages of MessagingClientId * AsyncReplyChannel<List<Message>>
        | ConfigureService of MessagingConfigParam

        /// For debugging.
        | GetState of AsyncReplyChannel<MessagingServiceState>


    type MessagingService(d : MessagingServiceData) =
        let updateMessages s m =
            match s.messages.TryFind m.messageInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageInfo.recipient, [ m ]) }


        let onStart s =
            s.messageServiceData.messagingServiceProxy.loadMessages()
            |> List.fold (fun acc e -> updateMessages acc e) s


        let onSendMessage s m =
            match m.messageInfo.deliveryType with
            | GuaranteedDelivery -> s.messageServiceData.messagingServiceProxy.saveMessage m
            | NonGuaranteedDelivery -> ignore()

            updateMessages s m


        let onGetMessages s (n, r : AsyncReplyChannel<List<Message>>) =
            match s.messages.TryFind n with
            | Some v ->
                r.Reply v

                v
                |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
                |> List.map (fun e -> s.messageServiceData.messagingServiceProxy.deleteMessage e.messageId)
                |> ignore

                { s with messages = s.messages.Add(n, []) }
            | None ->
                r.Reply []
                s


        let onConfigure s x =
            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages (n, r) -> return! onGetMessages s (n, r) |> loop
                            | ConfigureService x -> return! onConfigure s x |> loop
                            | GetState _ -> failwith ""
                        }

                onStart (MessagingServiceState.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))
        member __.configureService x = ConfigureService x |> messageLoop.Post
        member __.getState() = GetState |> messageLoop.PostAndReply

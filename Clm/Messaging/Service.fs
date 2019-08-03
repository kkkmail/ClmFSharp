namespace Messaging

open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MessagingService

module Service =

    type MessagingServiceData<'T> =
        {
            messageServerProxy : MessagingServiceProxy<'T>
        }


    type MessagingServiceState<'T> =
        {
            messageServiceData : MessagingServiceData<'T>
            messages : Map<NodeId, List<Message<'T>>>
        }

        static member defaultValue d =
            {
                messageServiceData = d
                messages = Map.empty
            }


    type MessagingServiceMessage<'T> =
        | Start
        | SendMessage of Message<'T>
        | GetMessages of NodeId * AsyncReplyChannel<List<Message<'T>>>
        | ConfigureService of MessagingConfigParam


    type MessagingService<'T>(d : MessagingServiceData<'T>) =
        let updateMessages s m =
            match s.messages.TryFind m.messageInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageInfo.recipient, [ m ]) }


        let onStart s =
            s.messageServiceData.messageServerProxy.loadMessages()
            |> List.fold (fun acc e -> updateMessages acc e) s


        let onSendMessage s (m : Message<'T>) =
            match m.messageInfo.deliveryType with
            | GuaranteedDelivery -> s.messageServiceData.messageServerProxy.saveMessage m
            | NonGuaranteedDelivery -> ignore()

            updateMessages s m


        let onGetMessages s (n, r : AsyncReplyChannel<List<Message<'T>>>) =
            match s.messages.TryFind n with
            | Some v ->
                r.Reply v

                v
                |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
                |> List.map (fun e -> s.messageServiceData.messageServerProxy.deleteMessage e.messageId)
                |> ignore

                { s with messages = s.messages.Add(n, []) }
            | None ->
                r.Reply []
                s


        let onConfigure s x =
            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : MessagingServiceState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages (n, r) -> return! onGetMessages s (n, r) |> loop
                            | ConfigureService x -> return! onConfigure s x |> loop
                        }

                onStart (MessagingServiceState<'T>.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))
        member __.configureService x = ConfigureService x |> messageLoop.Post


    type ClmMessagingService = MessagingService<ClmMesage>

namespace Messaging

open System
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MessagingServer

module Server =

    type MessagingServerData<'T> =
        {
            messageServerProxy : MessagingServerProxy<'T>
        }


    type MessagingServerState<'T> =
        {
            messageServerData : MessagingServerData<'T>
            messages : Map<NodeId, List<Message<'T>>>
        }

        static member defaultValue d =
            {
                messageServerData = d
                messages = Map.empty
            }


    type MessagingServerMessage<'T> =
        | Start
        | SendMessage of Message<'T>
        | GetMessages of NodeId * AsyncReplyChannel<List<Message<'T>>>


    type MessagingServer<'T>(d : MessagingServerData<'T>) =
        let updateMessages s m =
            match s.messages.TryFind m.messageInfo.recipient with
            | Some r -> { s with messages = s.messages.Add (m.messageInfo.recipient, m :: r) }
            | None -> { s with messages = s.messages.Add (m.messageInfo.recipient, [ m ]) }


        let onStart s =
            s.messageServerData.messageServerProxy.loadMessages()
            |> List.fold (fun acc e -> updateMessages acc e) s


        let onSendMessage s (m : Message<'T>) =
            match m.messageInfo.deliveryType with
            | GuaranteedDelivery -> s.messageServerData.messageServerProxy.saveMessage m
            | NonGuaranteedDelivery -> ignore()

            updateMessages s m


        let onGetMessages s (n, r : AsyncReplyChannel<List<Message<'T>>>) =
            match s.messages.TryFind n with
            | Some v ->
                r.Reply v

                v
                |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
                |> List.map (fun e -> s.messageServerData.messageServerProxy.deleteMessage e.messageId)
                |> ignore

                { s with messages = s.messages.Add(n, []) }
            | None ->
                r.Reply []
                s

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : MessagingServerState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages (n, r) -> return! onGetMessages s (n, r) |> loop
                        }

                onStart (MessagingServerState<'T>.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))


    type ClmMessagingServer = MessagingServer<ClmMesage>

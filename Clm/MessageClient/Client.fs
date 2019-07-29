namespace Messaging

open System
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MessagingClient

module Client =

    type MessagingClientData<'T> =
        {
            messageClientProxy : MessagingClientProxy<'T>
        }


    type MessagingClientState<'T> =
        {
            messageClientData : MessagingClientData<'T>
            incomingMessages : List<Message<'T>>
            outgoingMessages : List<Message<'T>>
        }

        /// kk:20190726 - Removing d makes F# compiler fail on type MessagingClient<'T> with:
        /// "This code is not sufficiently generic. The type variable 'T could not be generalized because it would escape its scope.". WTF!!!
        static member defaultValue d =
            {
                messageClientData = d
                incomingMessages = []
                outgoingMessages = []
            }


    type MessagingClientMessage<'T> =
        | Start
        | SendMessage of MessageInfo<'T>
        | GetMessages of AsyncReplyChannel<List<Message<'T>>>
        | TransmitMessages


    type MessagingClient<'T>(d : MessagingClientData<'T>) =
        let onStart s =
            let messages = s.messageClientData.messageClientProxy.loadMessages()
            let (i, o) = messages |> List.partition (fun e -> match fst e with | IncomingMessage -> true | OutgoingMessage -> false)
            let incoming = i |> List.map snd
            let outgoing = o |> List.map snd
            { s with outgoingMessages = s.outgoingMessages @ outgoing; incomingMessages = s.incomingMessages @ incoming }


        let onSendMessage s m =
            let message =
                {
                    messageId = MessageId.create()
                    messageInfo = m
                    createdOn = DateTime.Now
                }

            match m.deliveryType with
            | GuaranteedDelivery -> s.messageClientData.messageClientProxy.saveMessage OutgoingMessage message
            | NonGuaranteedDelivery -> ignore()

            { s with outgoingMessages = message :: s.outgoingMessages }


        let onGetMessages s (r : AsyncReplyChannel<List<Message<'T>>>) =
            r.Reply s.incomingMessages

            s.incomingMessages
            |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            |> List.map (fun e -> s.messageClientData.messageClientProxy.deleteMessage e.messageId)
            |> ignore

            { s with incomingMessages = [] }


        let onTransmitMessages s =
            let result =
                s.outgoingMessages
                |> List.rev
                |> s.messageClientData.messageClientProxy.sendMessages

            let messages = s.messageClientData.messageClientProxy.receiveMessages()
            let transmitted = result |> List.map (fun e -> e.messageId) |> Set.ofList
            let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageId) |> Set.ofList
            let failed = Set.difference outgoing transmitted
            let remaining = s.outgoingMessages |> List.filter (fun e -> failed.Contains e.messageId)
            { s with outgoingMessages = remaining; incomingMessages = messages @ s.incomingMessages }


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : MessagingClientState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages r -> return! onGetMessages s r |> loop
                            | TransmitMessages -> return! onTransmitMessages s |> loop
                        }

                onStart (MessagingClientState<'T>.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages reply)


    type ClmMessagingClient = MessagingClient<ClmMesage>

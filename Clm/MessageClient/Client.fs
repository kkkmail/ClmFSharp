namespace MessageClient

open System

module Client =

    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type NodeId =
        | NodeId of Guid

        member this.value = let (NodeId v) = this in v


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type MessageInfo<'T> =
        {
            recipient : NodeId
            deliveryType : MessageDeliveryType
            messageData : 'T
        }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    type Message<'T> =
        {
            messageId : MessageId
            messageInfo : MessageInfo<'T>
            createdOn : DateTime
        }


    type MessageClientProxy<'T> =
        {
            loadMessages : unit -> List<MessageType * Message<'T>>
            saveMessage : MessageType -> Message<'T> -> unit
            deleteMessage : MessageId -> unit
            sendMessages : List<Message<'T>> -> List<Message<'T>>
            receiveMessages : unit -> List<Message<'T>>
        }


    type MessageClientData<'T> =
        {
            messageClientProxy : MessageClientProxy<'T>
        }


    type MessageClientState<'T> =
        {
            messageClientData : MessageClientData<'T>
            incomingMessages : List<Message<'T>>
            outgoingMessages : List<Message<'T>>
        }

        /// kk:20190726 - Removing d makes F# compiler fail on type MessageClient<'T> with:
        /// "This code is not sufficiently generic. The type variable 'T could not be generalized because it would escape its scope.". WTF!!!
        static member defaultValue d =
            {
                messageClientData = d
                incomingMessages = []
                outgoingMessages = []
            }


    type MessageClientMessage<'T> =
        | Start
        | SendMessage of MessageInfo<'T>
        | GetMessages of AsyncReplyChannel<List<Message<'T>>>
        | TransmitMessages


    type MessageClient<'T>(d : MessageClientData<'T>) =
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
                let rec loop (s : MessageClientState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages r -> return! onGetMessages s r |> loop
                            | TransmitMessages -> return! onTransmitMessages s |> loop
                        }

                onStart (MessageClientState<'T>.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages reply)


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


    type MessageServerProxy<'T> =
        {
            loadMessages : unit -> List<Message<'T>>
            saveMessage : Message<'T> -> unit
            deleteMessage : MessageId -> unit
            //sendMessages : List<Message<'T>> -> List<Message<'T>>
            //receiveMessages : unit -> List<Message<'T>>
        }


    type MessageServerData<'T> =
        {
            messageServerProxy : MessageServerProxy<'T>
        }


    type MessageServerState<'T> =
        {
            messageServerData : MessageServerData<'T>
            messages : Map<NodeId, List<Message<'T>>>
        }

        static member defaultValue d =
            {
                messageServerData = d
                messages = Map.empty
            }


    type MessageServerMessage<'T> =
        | Start
        | SendMessage of Message<'T>
        | GetMessages of NodeId * AsyncReplyChannel<List<Message<'T>>>


    type MessageServer<'T>(d : MessageServerData<'T>) =
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
                let rec loop (s : MessageServerState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | SendMessage m -> return! onSendMessage s m |> loop
                            | GetMessages (n, r) -> return! onGetMessages s (n, r) |> loop
                        }

                onStart (MessageServerState<'T>.defaultValue d) |> loop
                )

        member __.sendMessage m = SendMessage m |> messageLoop.Post
        member __.getMessages n = messageLoop.PostAndReply (fun reply -> GetMessages (n, reply))

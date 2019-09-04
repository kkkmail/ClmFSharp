namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.Rop
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.ServiceResponse
open ClmSys.TimerEvents

module Client =

    /// Maximum number of messages to process in one go.
    let maxNumberOfMessages = 1000

    type MessagingClientData =
        {
            msgAccessInfo : MessagingClientAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }


    type MessagingClientState =
        {
            messageClientData : MessagingClientData
            incomingMessages : List<Message>
            outgoingMessages : List<Message>
        }

        member s.msgClientId = s.messageClientData.msgAccessInfo.msgClientId
        member s.service = s.messageClientData.msgResponseHandler.messagingService
        member s.proxy = s.messageClientData.msgClientProxy
        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]


        /// kk:20190726 - Removing d makes F# compiler fail on type MessagingClient<'T> with:
        /// "This code is not sufficiently generic. The type variable 'T could not be generalized because it would escape its scope.". WTF!!!
        static member defaultValue d =
            {
                messageClientData = d
                incomingMessages = []
                outgoingMessages = []
            }


    type MessagingClientMessage =
        | Start
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of MessageInfo
        //| GetMessages of AsyncReplyChannel<List<Message>>
        | TransmitMessages
        | ConfigureClient of MessagingClientConfigParam
        | TryPeekReceivedMessage of AsyncReplyChannel<Message option>
        | TryRemoveReceivedMessage of MessageId * AsyncReplyChannel<bool>


    and MessagingClient(d : MessagingClientData) =
        let logger = d.logger
        let logErr = d.logger.logErr
        let logInfo = d.logger.logInfo
        let logExn = d.logger.logExn

        let onStart s =
            let messages = s.messageClientData.msgClientProxy.loadMessages()
            let incoming = messages |> List.choose (fun e -> match e.messageType with | IncomingMessage -> Some e.message | _ -> None)
            let outgoing = messages |> List.choose (fun e -> match e.messageType with | OutgoingMessage -> Some e.message | _ -> None)
            { s with outgoingMessages = s.outgoingMessages @ outgoing; incomingMessages = s.incomingMessages @ incoming }


        let onGetVersion s (r : AsyncReplyChannel<MessagingDataVersion>) =
            printfn "MessagingClient.onGetVersion"
            r.Reply messagingDataVersion
            s


        let onSendMessage (s : MessagingClientState) m =
            printfn "MessagingClient.onSendMessage..."
            let message =
                {
                    messageId = MessageId.create()
                    dataVersion = messagingDataVersion
                    sender = d.msgAccessInfo.msgClientId
                    messageInfo = m
                    createdOn = DateTime.Now
                }

            match m.deliveryType with
            | GuaranteedDelivery -> s.proxy.saveMessage { messageType = OutgoingMessage; message = message }
            | NonGuaranteedDelivery -> ignore()

            { s with outgoingMessages = message :: s.outgoingMessages }


        //let onGetMessages s (r : AsyncReplyChannel<List<Message>>) =
        //    printfn "MessagingClient.onGetMessages..."
        //    r.Reply s.incomingMessages
        //
        //    s.incomingMessages
        //    |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
        //    |> List.map (fun e -> s.proxy.deleteMessage e.messageId)
        //    |> ignore
        //
        //    { s with incomingMessages = [] }


        let sendMessageImpl (s : MessagingClientState) m =
            async {
                printfn "MessagingClient.sendMessageImpl, messageId = %A" m.messageId.value
                try
                    match s.service.sendMessage m with
                    | DeliveredSuccessfully _ ->
                        match m.messageInfo.deliveryType with
                        | GuaranteedDelivery -> s.proxy.deleteMessage m.messageId
                        | NonGuaranteedDelivery -> ignore()
                        return Some m
                    | DataVersionMismatch v ->
                        logErr (sprintf "MessagingClient.sendMessageImpl: messageId = %A, data version mismatch server has: %A but client has: %A." m.messageId.value v messagingDataVersion)
                        return None
                    | ServerIsShuttingDown ->
                        logInfo (sprintf "MessagingClient.sendMessageImpl: messageId = %A - server is shutting down." m.messageId.value)
                        return None
                    | ExceptionOccurred e ->
                        logExn (sprintf "MessagingClient.sendMessageImpl: messageId = %A - exception occurred." m.messageId.value) e
                        return None
                with
                    | e ->
                        s.messageClientData.logger.logExn (sprintf "MessagingClient.sendMessageImpl:Failed to send message: %A" m.messageId) e
                        return None
            }

        let tryReceiveSingleMessage (s : MessagingClientState) =
            async {
                printfn "MessagingClient.tryReceiveSingleMessage..."

                match s.service.tryPeekMessage s.msgClientId with
                | Some m ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Received message with id: %A" m.messageId
                    match m.messageInfo.deliveryType with
                    | GuaranteedDelivery ->
                        {
                            message = m
                            messageType = IncomingMessage
                        }
                        |> d.msgClientProxy.saveMessage
                    | NonGuaranteedDelivery -> ignore()

                    match s.service.tryDeleteFromServer s.msgClientId m.messageId with
                    | true ->
                        printfn "MessagingClient.tryReceiveSingleMessage: Deleted message from server. Message id: %A" m.messageId
                        ignore()
                    | false ->
                        printfn "MessagingClient.tryReceiveSingleMessage: Cannot delete message from server. Message id: %A" m.messageId
                        logErr (sprintf "tryReceiveSingleMessage: Unable to delete a message from server for client: %A, message id: %A." s.msgClientId m.messageId)
                    return Some m
                | None ->
                    printfn "MessagingClient.tryReceiveSingleMessage: Did not receive a message."
                    return None
            }


        let receiveMessagesImpl (s : MessagingClientState) =
            async {
                printfn "MessagingClient.receiveMessagesImpl..."
                try
                    let serverVersion = s.service.getVersion()

                    match serverVersion = messagingDataVersion with
                    | true ->
                        return! MessagingClientState.maxMessages |> List.mapWhileSomeAsync (fun _ -> tryReceiveSingleMessage s)
                    | false ->
                        s.messageClientData.logger.logErr (sprintf "MessagingClient.receiveMessagesImpl - different data versions - client: %A, server: %A" messagingDataVersion.value serverVersion.value)
                        return []
                with
                    | e ->
                        s.messageClientData.logger.logExn "MessagingClient.receiveMessagesImpl: Failed to receive messages: " e
                        return []
                }


        let onTransmitMessages s =
            async {
                printfn "MessagingClient.onTransmitMessages..."
                let! r =
                    s.outgoingMessages
                    |> List.rev
                    |> List.mapAsync (sendMessageImpl s)

                let result = r |> List.choose id
                let transmitted = result |> List.map (fun e -> e.messageId) |> Set.ofList
                let! received = receiveMessagesImpl s
                let outgoing = s.outgoingMessages |> List.map (fun e -> e.messageId) |> Set.ofList
                let failed = Set.difference outgoing transmitted
                let remaining = s.outgoingMessages |> List.filter (fun e -> failed.Contains e.messageId)
                let x = { s with outgoingMessages = remaining; incomingMessages = received @ s.incomingMessages }
                printfn "MessagingClient.onTransmitMessages: incomingMessages.Length = %A, outgoingMessages.Length = %A" x.incomingMessages.Length x.outgoingMessages.Length
                return x
            }


        let onConfigureClient s x =
            printfn "MessagingClient.onConfigureClient..."
            s


        let onTryPeekReceivedMessage (s : MessagingClientState) (r : AsyncReplyChannel<Message option>) =
            printfn "MessagingClient.onTryPeekReceivedMessage..."
            s.incomingMessages |> List.tryHead |> r.Reply
            s


        let onTryRemoveReceivedMessage (s : MessagingClientState) m (r : AsyncReplyChannel<bool>) =
            printfn "MessagingClient.onTryRemoveReceivedMessage..."
            s.proxy.deleteMessage m
            r.Reply true
            { s with incomingMessages = removeFirst (fun e -> e.messageId = m) s.incomingMessages }


        let onTryProcessMessage (w : MessagingClient) x f =
            async {
                printfn "MessagingClient.tryProcessMessageImpl - starting..."
                match! w.tryPeekReceivedMessage() with
                | Some m ->
                    try
                        printfn "    MessagingClient.tryProcessMessageImpl: calling f m, messageId: %A" m.messageId
                        let r = f x m
                        printfn "    MessagingClient.tryProcessMessageImpl: calling tryRemoveReceivedMessage, messageId: %A" m.messageId

                        match! w.tryRemoveReceivedMessage m.messageId with
                        | true -> printfn "    MessagingClient.tryProcessMessageImpl: Successfully removed message with id: %A" m.messageId
                        | false -> printfn "    MessagingClient.tryProcessMessageImpl: !!! ERROR !!! removing message with id: %A" m.messageId

                        printfn "    MessagingClient.tryProcessMessageImpl - completed."
                        return Some r
                    with
                    | ex ->
                        logger.logExn "MessagingClient.tryProcessMessageImpl" ex
                        return None
                | None -> return None
            }

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! timed "MessagingClient.onStart" onStart s |> loop
                            | GetVersion r -> return! timed "MessagingClient.onGetVersion" onGetVersion s r |> loop
                            | SendMessage m -> return! timed "MessagingClient.onSendMessage" onSendMessage s m |> loop
                            //| GetMessages r -> return! timed "MessagingClient.onGetMessages" onGetMessages s r |> loop
                            | TransmitMessages ->
                                let! ns = timed "MessagingClient.onTransmitMessages" onTransmitMessages s
                                return! ns |> loop
                            | ConfigureClient x -> return! timed "MessagingClient.onConfigureClient" onConfigureClient s x |> loop
                            | TryPeekReceivedMessage r -> return! timed "MessagingClient.onTryPeekReceivedMessage" onTryPeekReceivedMessage s r |> loop
                            | TryRemoveReceivedMessage (m, r) -> return! timed "MessagingClient.onTryRemoveReceivedMessage (using timed)" onTryRemoveReceivedMessage s m r |> loop
                        }

                onStart (MessagingClientState.defaultValue d) |> loop
                )


        let eventHandler _ =
            printfn "MessagingClient: Transmitting messages..."
            TransmitMessages |> messageLoop.Post


        let h = new EventHandler(EventHandlerInfo.defaultValue eventHandler)
        do h.start()


        member __.getVersion() = GetVersion |> messageLoop.PostAndReply
        member __.sendMessage m = SendMessage m |> messageLoop.Post
        //member __.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages reply)
        member __.configureClient x = ConfigureClient x |> messageLoop.Post
        member __.transmitMessages() = TransmitMessages |> messageLoop.Post
        member private __.tryPeekReceivedMessage() = messageLoop.PostAndAsyncReply (fun reply -> TryPeekReceivedMessage reply)
        member private __.tryRemoveReceivedMessage m = messageLoop.PostAndAsyncReply (fun reply -> TryRemoveReceivedMessage (m, reply))
        member this.tryProcessMessage s f = onTryProcessMessage this s f

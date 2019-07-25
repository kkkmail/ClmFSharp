namespace MessageClient

open System

module Client =

    type MessageId =
        | MessageId of Guid

        static member create() = Guid.NewGuid() |> MessageId


    type MessageRecipient =
        | MessageRecipient of Guid


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type MessageClientProxy<'T> =
        {
            dummy : int
        }

    type MessageClientData<'T> =
        {
            messageClientProxy : MessageClientProxy<'T>
        }


    type MessageInfo<'T> =
        {
            recipient : MessageRecipient
            deliveryType : MessageDeliveryType
            messageData : 'T
        }


    type Message<'T> =
        {
            messageId : MessageId
            messageInfo : MessageInfo<'T>
            createdOn : DateTime
        }

    type MessageClientState<'T> =
        {
            incomingMessages : List<Message<'T>>
            outgoingMessages : List<Message<'T>>
        }

        static member defaultValue : MessageClientState<'T> =
            {
                incomingMessages = []
                outgoingMessages = []
            }


    type MessageClientMessage<'T> =
        | SendMessage of Message<'T>
        | GetMessages of AsyncReplyChannel<MessageClientState<'T>>


    type MessageClient<'T>(d : MessageClientData<'T>) =

        let sendMessageImpl m (u : MailboxProcessor<MessageClientMessage<'T>>) =
            let message =
                {
                    messageId = MessageId.create()
                    messageInfo = m
                    createdOn = DateTime.Now
                }

            SendMessage message |> u.Post
            message.messageId


        let getMessagesImpl (u : MailboxProcessor<MessageClientMessage<'T>>) =
            let state = u.PostAndReply GetMessages
            state.incomingMessages


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop (s : MessageClientState<'T> ) =
                    async
                        {
                            match! u.Receive() with
                            | SendMessage e -> return! loop s
                            | GetMessages r ->
                                r.Reply s
                                return! loop s
                        }

                loop MessageClientState<'T>.defaultValue
                )

        member this.sendMessage m = sendMessageImpl m messageLoop
        member this.getMessages() = getMessagesImpl messageLoop

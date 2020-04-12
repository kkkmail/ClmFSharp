namespace Messaging

open System
open ClmSys.VersionInfo
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives

module Service =

    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
        }


    type MessagingServiceState =
        {
            expirationTime : TimeSpan
        }

        static member defaultValue =
            {
                expirationTime = TimeSpan.FromHours 6.0
            }


    type MessagingServiceMessage =
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<UnitResult>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<ClmResult<Message option>>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<UnitResult>
        | RemoveExpiredMessages of AsyncReplyChannel<UnitResult>


    let onGetVersion () = messagingDataVersion
    let onSendMessage saveMessage (m : Message) = saveMessage m


    let onTryPeekMessage tryPickMessage n =
        printfn "onTryPeekMessage: clientId = %A." n
        let result = tryPickMessage n
        printfn "onTryPeekMessage: result = %A for clientId = %A." (result.ToString().Substring(0, min 100 (result.ToString().Length - 1))) n
        result


    let onTryDeleteFromServer (deleteMessage : MessageId -> UnitResult) n m =
        printfn "onTryDeleteFromServer: clientId = %A, messageId = %A." n m
        deleteMessage m


    let onRemoveExpiredMessages deleteExpiredMessages (s : MessagingServiceState) =
        s, deleteExpiredMessages s.expirationTime


    type MessagingService(d : MessagingServiceData) =
        let tryPickMsg = d.messagingServiceProxy.tryPickMessage
        let saveMsg = d.messagingServiceProxy.saveMessage
        let deleteMsg = d.messagingServiceProxy.deleteMessage
        let deleteExpiredMsgs = d.messagingServiceProxy.deleteExpiredMessages

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | GetVersion r -> return! (s, onGetVersion()) |> (withReply r) |> loop
                            | SendMessage (m, r) -> return! (s, onSendMessage saveMsg m) |> (withReply r) |> loop
                            | TryPeekMessage (n, r) -> return! (s, onTryPeekMessage tryPickMsg n) |> (withReply r) |> loop
                            | TryDeleteFromServer (n, m, r) -> return! (s, onTryDeleteFromServer deleteMsg n m) |> (withReply r) |> loop
                            | RemoveExpiredMessages r -> return! onRemoveExpiredMessages deleteExpiredMsgs s |> (withReply r) |> loop
                        }

                MessagingServiceState.defaultValue |> loop
                )

        member _.getVersion() = GetVersion |> messageLoop.PostAndReply |> Ok
        member _.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))
        member _.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply))
        member _.tryDeleteFromServer (n, m) = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply))
        member _.removeExpiredMessages() = messageLoop.PostAndReply RemoveExpiredMessages

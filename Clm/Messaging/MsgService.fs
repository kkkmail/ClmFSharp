namespace Messaging

open System
open ClmSys
open ClmSys.VersionInfo
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives
open ClmSys.MessagingServiceErrors

module Service =

    let private toError f = f |> MessagingServiceErr |> Error
    let private addError g f e = ((f |> g |> MessagingServiceErr) + e) |> Error


    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
        }


    type MessagingServiceState =
        {
            //workState : MessagingWorkState
            //messages : Map<MessagingClientId, List<MessageWithOptionalData>>
            expirationTime : TimeSpan
        }

        //member s.getState() =
        //    {
        //        msgVersion = messagingDataVersion
        //        msgWorkState = s.workState
        //        msgInfo = s.messages |> Map.toList |> List.map (fun (k, v) -> k, v |> List.map (fun e -> e.messageDataInfo.messageId))
        //    }

        static member defaultValue =
            {
                //workState = MsgSvcNotStarted
                //messages = Map.empty
                expirationTime = TimeSpan.FromHours 6.0
            }


    //type OnStartProxy =
    //    {
    //        loadMessages : unit -> ListResult<Message>
    //        updateMessages : MessagingServiceState -> Message -> MessagingServiceState
    //    }


    type MessagingServiceMessage =
        //| Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | GetVersion of AsyncReplyChannel<MessagingDataVersion>
        | SendMessage of Message * AsyncReplyChannel<UnitResult>
        //| ConfigureService of MessagingConfigParam
        //| GetState of AsyncReplyChannel<MsgServiceState>
        | TryPeekMessage of MessagingClientId * AsyncReplyChannel<ClmResult<Message option>>
        | TryDeleteFromServer of MessagingClientId * MessageId * AsyncReplyChannel<UnitResult>
        | RemoveExpiredMessages of AsyncReplyChannel<UnitResult>


    //let updateMessages s (m : Message) =
    //    let x =
    //        match s.messages.TryFind m.messageDataInfo.recipientInfo.recipient with
    //        | Some r -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, m.toMessageWithOptionalData() :: r) }
    //        | None -> { s with messages = s.messages.Add (m.messageDataInfo.recipientInfo.recipient, [ m.toMessageWithOptionalData() ]) }
    //    x


    //let onStart (proxy : OnStartProxy) (s : MessagingServiceState) (r : AsyncReplyChannel<UnitResult>) =
    //    let w, f =
    //        match s.workState with
    //        | MsgSvcNotStarted ->
    //            match proxy.loadMessages() with
    //            | Ok q ->
    //                let v, e = q |> Rop.unzip

    //                let x =
    //                    v
    //                    |> List.sortByDescending (fun e -> e.messageDataInfo.createdOn) // The newest message WILL BE at the head after we add them to the list starting from the oldest first.
    //                    |> List.fold (fun acc e -> proxy.updateMessages acc e) s

    //                { x with workState = CanTransmitMessages }, foldToUnitResult e
    //            | Error e -> s, Error e
    //        | CanTransmitMessages | ShuttingDown -> s, Ok()

    //    r.Reply f
    //    w


    let onGetVersion () = messagingDataVersion
    let onSendMessage saveMessage (m : Message) = saveMessage m

    //let onConfigure s x =
    //    match x with
    //    | MsgWorkState w ->
    //        match w with
    //        | MsgSvcNotStarted -> s // Cannot change the state to not started.
    //        | CanTransmitMessages | ShuttingDown -> { s with workState = w }


    //let onGetState (s : MessagingServiceState) (r : AsyncReplyChannel<MsgServiceState>) =
    //    //s.getState() |> r.Reply
    //    s


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

        //member m.start() = ignore() // messageLoop.PostAndReply (fun reply -> Start (m.onStartProxy, reply))
        member _.getVersion() = GetVersion |> messageLoop.PostAndReply |> Ok
        member _.sendMessage m = messageLoop.PostAndReply (fun reply -> SendMessage (m, reply))

        //member _.configureService x =
        //    ConfigureService x |> messageLoop.Post
        //    Ok()

        //member _.getState() = GetState |> messageLoop.PostAndReply |> Ok
        member _.tryPeekMessage n = messageLoop.PostAndReply (fun reply -> TryPeekMessage (n, reply))
        member _.tryDeleteFromServer (n, m) = messageLoop.PostAndReply (fun reply -> TryDeleteFromServer (n, m, reply))
        member _.removeExpiredMessages() = messageLoop.PostAndReply RemoveExpiredMessages

        //member _.onStartProxy =
        //    {
        //        loadMessages = d.messagingServiceProxy.loadMessages
        //        updateMessages = updateMessages
        //    }

namespace ServiceProxy

open ClmSys.Retry
open MessagingServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ClmSys.MessagingData
open ClmSys.Registry

module MsgServiceProxy =

    let private logError e = printfn "Error: %A" e
    let private tryFun f = tryFun logError f

    let private saveMessage clientName m = tryFun (fun _ -> saveMessageFs clientName m) |> ignore
    let private deleteMessage clientName i = tryFun (fun _ -> tryDeleteMessageFs clientName i) |> ignore
    let private tryLoadMessage clientName i = tryFun (fun _ -> tryLoadMessageFs clientName i) |> Option.bind id


    let private loadMessages clientName () =
        match tryFun (getMessageIdsFs clientName) with
        | Some i ->
            i
            |> List.map (fun e -> tryFun (fun _ -> tryLoadMessageFs clientName e) |> Option.bind id)
            |> List.choose id
        | None -> []


    let private saveMessageWithType clientName m = tryFun (fun _ -> saveMessageWithTypeFs clientName m) |> ignore
    let private deleteMessageWithType clientName i = tryFun (fun _ -> tryDeleteMessageWithTypeFs clientName i) |> ignore


    let private loadMessageWithTypes clientName =
        match tryFun (getMessageWithTypeIdsFs clientName) with
        | Some i ->
            i
            |> List.map (fun e -> tryFun (fun _ -> tryLoadMessageWithTypeFs clientName e) |> Option.bind id)
            |> List.choose id
        | None -> []


    type MessagingClientProxyInfo =
        {
            messagingClientName : MessagingClientName
        }


    /// Provides IO proxy for messaging client.
    /// A messaging client may or may NOT have SQL server at its disposal.
    /// This proxy encapsulates that.
    type MessagingClientProxy =
        {
            loadMessages : unit -> list<MessageWithType>
            saveMessage : MessageWithType -> unit
            deleteMessage : MessageId -> unit
        }

        static member create (i : MessagingClientProxyInfo) =
            {
                loadMessages = fun () -> loadMessageWithTypes i.messagingClientName
                saveMessage = fun m -> saveMessageWithType i.messagingClientName m
                deleteMessage = fun m -> deleteMessageWithType i.messagingClientName m
            }


    type MessagingServiceProxy =
        {
            loadMessages : unit -> List<Message>
            saveMessage : Message -> unit
            deleteMessage : MessageId -> unit
            tryLoadMessage : MessageId -> Message option
        }

        static member defaultValue =
            {
                loadMessages = loadMessages messagingServiceName
                saveMessage = saveMessage messagingServiceName
                deleteMessage = deleteMessage messagingServiceName
                tryLoadMessage = tryLoadMessage messagingServiceName
            }

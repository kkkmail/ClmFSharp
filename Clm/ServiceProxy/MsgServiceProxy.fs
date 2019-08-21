namespace ServiceProxy

open ClmSys.Retry
open MessagingServiceInfo.ServiceInfo
open NoSql.FileSystemTypes

module MsgServiceProxy =

    let private logError e = printfn "Error: %A" e
    let private tryFun f = tryFun logError f

    let private saveMessage m = tryFun (fun _ -> saveMessageFs m) |> ignore
    let private deleteMessage i = tryFun (fun _ -> tryDeleteMessageFs i) |> ignore

    let private loadMessages () =
        match tryFun getMessageIdsFs with
        | Some i ->
            i
            |> List.map (fun e -> tryFun (fun _ -> tryLoadMessageFs e) |> Option.bind id)
            |> List.choose id
        | None -> []

    let private saveMessageWithType m = tryFun (fun _ -> saveMessageWithTypeFs m) |> ignore
    let private deleteMessageWithType i = tryFun (fun _ -> tryDeleteMessageWithTypeFs i) |> ignore


    let private loadMessageWithTypes () =
        match tryFun getMessageWithTypeIdsFs with
        | Some i ->
            i
            |> List.map (fun e -> tryFun (fun _ -> tryLoadMessageWithTypeFs e) |> Option.bind id)
            |> List.choose id
        | None -> []


    /// Provides IO proxy for messaging client.
    /// A messaging client may or may NOT have SQL server at its disposal.
    /// This proxy encapsulates that.
    type MessagingClientProxy =
        {
            loadMessages : unit -> List<MessageWithType>
            saveMessage : MessageWithType -> unit
            deleteMessage : MessageId -> unit
        }

        static member defaultValue : MessagingClientProxy =
            {
                loadMessages = loadMessageWithTypes
                saveMessage = saveMessageWithType
                deleteMessage = deleteMessageWithType
            }


    type MessagingServiceProxy =
        {
            loadMessages : unit -> List<Message>
            saveMessage : Message -> unit
            deleteMessage : MessageId -> unit
        }

        static member defaultValue =
            {
                loadMessages = loadMessages
                saveMessage = saveMessage
                deleteMessage = deleteMessage
            }

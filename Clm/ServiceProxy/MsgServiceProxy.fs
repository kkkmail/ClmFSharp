namespace ServiceProxy

open ClmSys.Retry
open MessagingServiceInfo.ServiceInfo
open NoSql.FileSystemTypes
open ClmSys.MessagingData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys
open ClmSys.GeneralErrors

module MsgServiceProxy =

    let private trySaveMessage clientName m = tryRopFun GeneralFileException (fun _ -> trySaveMessageFs clientName m)
    let private tryDeleteMessage clientName i = tryRopFun GeneralFileException (fun _ -> tryDeleteMessageFs clientName i)
    let private tryLoadMessage clientName i = tryRopFun GeneralFileException (fun _ -> tryLoadMessageFs clientName i)
    let private tryGetMessageIds clientName = tryRopFun GeneralFileException (fun _ -> tryGetMessageIdsFs clientName ())


    let private tryLoadMessages clientName () =
        match tryGetMessageIds clientName with
        | Ok i ->
            i
            |> List.map (tryLoadMessage clientName)
            |> Ok
        | Error e -> Error e


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
            //logger : Logger
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


    /// Provides IO proxy for messaging service.
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

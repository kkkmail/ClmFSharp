namespace MessagingServer

open Argu
open ClmSys.GeneralData
open ClmSys.Registry

module SvcCommandLine =

    let logger e = printfn "Error / Exception: %A" e


    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingServerRunArgs =
        | [<Unique>] [<AltCommandLine("-server")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgSaveSettings

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."
                | MsgSaveSettings -> "saves settings to the Registry."


    and
        [<CliPrefix(CliPrefix.None)>]
        SvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start of ParseResults<MessagingServerRunArgs>
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<MessagingServerRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install messaging service."
                | Uninstall -> "uninstall messaging service."
                | Start _ -> "start messaging service."
                | Stop -> "stop messaging service."
                | Run _ -> "run messaging service from command line without installing."


    let tryGetServerAddress p =
         p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort p =
        p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGetSaveSettings p =
        p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)


    let tryGetServiceAccessInfo p =
        let ao =
            match tryGetServerAddress p with
            | Some a -> Some a
            | None -> tryGetMessagingServerAddress logger

        let po =
            match tryGetServerPort p with
            | Some a -> Some a
            | None -> tryGetMessagingServerPort logger

        match ao, po with
        | Some address, Some port ->
            match tryGetSaveSettings p with
            | Some _ ->
                trySetMessagingServerAddress logger address |> ignore
                trySetMessagingServerPort logger port |> ignore
            | None -> ignore()

            {
                messagingServerAccessInfo =
                    {
                        serviceAddress = address
                        servicePort = port
                    }
            }
            |> Some
        | _ -> None

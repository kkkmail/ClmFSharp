namespace MessagingService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.ServiceInstaller

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] MsgVersion of string

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."
                | MsgSaveSettings -> "saves settings to the Registry."
                | MsgVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."


    type MsgSvcArguments = SvcArguments<MessagingServiceRunArgs>

    //and
    //    [<CliPrefix(CliPrefix.None)>]
    //    MsgSvcArguments =
    //    | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
    //    | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
    //    | [<Unique>] [<First>] Start of ParseResults<MessagingServiceRunArgs>
    //    | [<Unique>] [<First>] Stop
    //    | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<MessagingServiceRunArgs>
    //    | [<Unique>] [<First>] [<AltCommandLine("s")>] Save

    //with
    //    interface IArgParserTemplate with
    //        member s.Usage =
    //            match s with
    //            | Install -> "install messaging service."
    //            | Uninstall -> "uninstall messaging service."
    //            | Start _ -> "start messaging service."
    //            | Stop -> "stop messaging service."
    //            | Run _ -> "run messaging service from command line without installing."
    //            | Save -> "save parameters into the registry."


    let tryGetServerAddress p =
         p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort p =
        p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGetSaveSettings p =
        p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)


    let tryGetVersion p =
        p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getServiceAccessInfo p =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None ->
                match tryGetMessagingServerAddress logger version with
                | Some a -> a
                | None -> ServiceAddress.defaultMessagingServerValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None ->
                match tryGetMessagingServerPort logger version with
                | Some a -> a
                | None -> ServicePort.defaultMessagingServerValue

        match tryGetSaveSettings p with
        | Some _ ->
            trySetMessagingServerAddress logger versionNumberValue address |> ignore
            trySetMessagingServerPort logger versionNumberValue port |> ignore
        | None -> ignore()

        {
            messagingServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }
        }

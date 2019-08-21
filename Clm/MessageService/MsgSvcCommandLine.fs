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


    type MsgSvcArgs = SvcArguments<MessagingServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        MsgSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<MessagingServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<MessagingServiceRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install messaging service."
                | Uninstall -> "uninstall messaging service."
                | Start _ -> "start messaging service."
                | Stop -> "stop messaging service."
                | Run _ -> "run messaging service from command line without installing."
                | Save _ -> "save parameters into the registry."


    let convertArgs s =
        match s with
        | Install -> MsgSvcArgs.Install
        | Uninstall -> MsgSvcArgs.Uninstall
        | Start -> MsgSvcArgs.Start
        | Stop -> MsgSvcArgs.Stop
        | Run a -> MsgSvcArgs.Run a
        | Save a -> MsgSvcArgs.Save a


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort


    let getServiceAccessInfoImpl b p =
        let name = messagingServiceName

        let version = getVersion p
        let address = getMsgServerAddress logger version name p
        let port = getMsgServerPort logger version name p

        let saveSettings() =
            trySetMessagingClientAddress logger versionNumberValue name address |> ignore
            trySetMessagingClientPort logger versionNumberValue name port |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            messagingServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }
        }


    let getServiceAccessInfo = getServiceAccessInfoImpl false


    let saveSettings p =
        getServiceAccessInfoImpl true p |> ignore

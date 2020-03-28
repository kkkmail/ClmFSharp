namespace MessagingService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.ServiceInstaller
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives

//open ClmSys.MessagingPrimitives

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


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServiceAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServicePort = getMsgServicePortImpl tryGetMsgServicePort


    let getServiceAccessInfoImpl b p : MessagingServiceAccessInfo =
        let name = messagingServiceRegistryName

        let version = getVersion p
        let address = getMsgServiceAddress logger version name p
        let port = getMsgServicePort logger version name p
        printfn "address: %A, port: %A" address port

        let saveSettings() =
            trySetMessagingServiceAddress versionNumberValue name address |> ignore
            trySetMessagingServicePort versionNumberValue name port |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            messagingServiceAddress = address
            messagingServicePort = port
            messagingServiceName = messagingServiceName
        }


    let getServiceAccessInfo = getServiceAccessInfoImpl false


    let saveSettings p =
        getServiceAccessInfoImpl true p |> ignore

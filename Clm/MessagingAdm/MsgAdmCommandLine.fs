namespace MessagingAdm

open Argu
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives

module AdmCommandLine =

    [<Literal>]
    let MsgAdmAppName = "MessagingAdm.exe"

    [<CliPrefix(CliPrefix.None)>]
    type MsgAdmRunArgs =
        | [<Unique>] [<First>] [<AltCommandLine("m")>] MonitorMsgService
        | [<Unique>] [<First>] [<AltCommandLine("start")>] StartMsgService
        | [<Unique>] [<First>] [<AltCommandLine("stop")>] StopMsgService
        | [<Unique>] [<AltCommandLine("-address")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] MsgVersion of string

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MonitorMsgService _ -> "monitors messaging service."
                | StartMsgService _ -> "start messaging service."
                | StopMsgService _ -> "stop messaging service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."
                | MsgSaveSettings -> "saves settings to the Registry."
                | MsgVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServiceAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServicePort = getMsgServicePortImpl tryGetMsgServicePort


    let getServiceAccessInfoImpl b p =
        let name = messagingAdmRegistryName

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

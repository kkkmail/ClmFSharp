namespace MessagingAdm

open Argu
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.GeneralPrimitives

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


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort


    let getServiceAccessInfoImpl b p =
        let name = messagingAdmName

        let version = getVersion p
        let address = getMsgServerAddress logger version name p
        let port = getMsgServerPort logger version name p
        printfn "address: %A, port: %A" address port

        let saveSettings() =
            trySetMessagingClientAddress versionNumberValue name address |> ignore
            trySetMessagingClientPort versionNumberValue name port |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            messagingServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                    inputServiceName = MessagingServiceName
                }
        }

    let getServiceAccessInfo = getServiceAccessInfoImpl false

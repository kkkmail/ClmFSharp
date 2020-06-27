namespace MessagingAdm

open Argu
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.VersionInfo
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open MessagingServiceInfo.ServiceInfo

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
                | MsgVersion _ -> "tries to load data from specified version instead of current version. If -save is specified, then saves data into current version."


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServiceAddress (w: MsgSettings) p = tryGetMsgServiceAddress p |> Option.defaultValue w.msgSvcAddress
    let getMsgServicePort (w: MsgSettings) p = tryGetMsgServicePort p |> Option.defaultValue w.msgSvcPort

    
    let loadSettings p =
        MsgAppSettings.SelectExecutableFile(getFileName messagingProgramName)
        let w = loadMsgServiceSettings()
        
        let w1 =
            {
                msgSvcAddress = getMsgServiceAddress w p
                msgSvcPort = getMsgServicePort w p
            }
            
        w1
        

    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let tryGetSave() = tryGetSaveSettings p
        getMsgServiceAccessInfo (load, tryGetSave) b


    let getServiceAccessInfo = getServiceAccessInfoImpl false

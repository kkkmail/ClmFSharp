namespace MessagingAdm

open Argu
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.Logging

module AdmCommandLine =

    [<Literal>]
    let MsgAdmAppName = "MessagingAdm.exe"

    type
        [<CliPrefix(CliPrefix.Dash)>]
        ConfigureMsgServiceArgs =
            | [<Unique>] Start
            | [<Unique>] [<AltCommandLine("-s")>] ShutDown

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | Start -> "starts messaging server."
                    | ShutDown _ -> "start shut down process - the server will stop accepting new messages."

            member this.configParam =
                match this with
                | Start -> MsgWorkState CanTransmitMessages
                | ShutDown -> MsgWorkState ShuttingDown


    and
        [<CliPrefix(CliPrefix.Dash)>]
        MsgMonitorArgs =
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>] RefreshInterval of int

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | RefreshInterval _ -> "refresh inteval in seconds."

    and
        [<CliPrefix(CliPrefix.None)>]
        MsgAdmArguments =
            | [<Unique>] [<AltCommandLine("m")>]      MonitorMsgService of ParseResults<MsgMonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]      ConfigureMsgService of ParseResults<ConfigureMsgServiceArgs>
            | [<Unique>] [<AltCommandLine("server")>] MsgServerAddress of string
            | [<Unique>] [<AltCommandLine("port")>]   MsgServerPort of int
            | [<Unique>] [<AltCommandLine("-version")>] MsgVersion of string

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | MonitorMsgService _ -> "starts monitor."
                    | ConfigureMsgService _ -> "reconfigures service."
                    | MsgServerAddress _ -> "server address / name."
                    | MsgServerPort _ -> "server port."
                    | MsgVersion _ -> "tries to load data from specfied version instead of current version."


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | MsgServerAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | MsgServerPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | MsgVersion p -> p |> VersionNumber |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort


    let getServiceAccessInfo p =
        let name = messagingServiceName

        let version = getVersion p
        let address = getMsgServerAddress logger version name p
        let port = getMsgServerPort logger version name p
        printfn "address: %A, port: %A" address port

        {
            messagingServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                    serviceName = MessagingServiceName
                }
        }

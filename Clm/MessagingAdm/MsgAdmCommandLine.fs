namespace MessagingAdm

open Argu
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ClmSys.MessagingData

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

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | MonitorMsgService _ -> "starts monitor."
                    | ConfigureMsgService _ -> "reconfigures service."
                    | MsgServerAddress _ -> "server address / name."
                    | MsgServerPort _ -> "server port."


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | MsgServerAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | MsgServerPort p -> p |> ServicePort |> Some | _ -> None)


    let getServiceAccessInfo p =
        let address =
            match tryGetMsgServerAddress p with
            | Some a -> a
            | None -> ServiceAddress.defaultMessagingServerValue

        let port =
            match tryGetMsgServerPort p with
            | Some a -> a
            | None -> ServicePort.defaultMessagingServerValue


        {
            messagingServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                    serviceName = MessagingServiceName
                }
        }

namespace MessagingServer

open Argu
open ClmSys.GeneralData

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingServerRunArgs =
        | [<Unique>] [<AltCommandLine("-server")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgSvcAddress _ -> "service ip address / name."
                | MsgSvcPort _ -> "service port."


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

    let tryGetServerAddress (p :list<MessagingServerRunArgs>) =
         p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort (p :list<MessagingServerRunArgs>) =
        p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let getServiceAccessInfo (p :list<MessagingServerRunArgs>) =
        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None -> ServiceAddress.defaultValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None -> ServicePort.defaultValue

        {
            serviceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }

            minUsefulEe = ee
        }

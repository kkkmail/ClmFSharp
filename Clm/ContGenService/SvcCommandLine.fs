namespace ContGenService

open Argu
open ClmSys.GeneralData

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type RunArgs =
        | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
        | [<Unique>] [<AltCommandLine("-i")>] RunIdle
        | [<Unique>] [<AltCommandLine("-server")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | NumberOfCores _ -> "number of logical cores to use."
                | RunIdle -> "Start idle."
                | SvcAddress _ -> "service ip address / name."
                | SvcPort _ -> "service port."


    and
        [<CliPrefix(CliPrefix.None)>]
        SvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start of ParseResults<RunArgs>
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<RunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install service."
                | Uninstall -> "uninstall service."
                | Start _ -> "start service."
                | Stop -> "stop service."
                | Run _ -> "run service from command line without installing."

    let tryGetServerAddress (p :list<RunArgs>) =
         p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort (p :list<RunArgs>) =
        p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> Some | _ -> None)


    let getServiceAccessInfo (p :list<RunArgs>) =
        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None -> ServiceAddress.defaultValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None -> ServicePort.defaultValue

        {
            serviceAddress = address
            servicePort = port
        }


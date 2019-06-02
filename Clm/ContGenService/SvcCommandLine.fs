namespace ContGenService

open Argu
open ClmSys.GeneralData

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type RunArgs =
        | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
        | [<Unique>] [<AltCommandLine("-i")>] RunIdle
        | [<Unique>] [<AltCommandLine("-a")>] ServiceAddress of string
        | [<Unique>] [<AltCommandLine("-p")>] ServicePort of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | NumberOfCores _ -> "number of logical cores to use."
                | RunIdle -> "Start idle."
                | ServiceAddress _ -> "service ip address / name."
                | ServicePort _ -> "service port."


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
         p |> List.tryPick (fun e -> match e with | ServiceAddress s -> s |> ClmSys.GeneralData.ServiceAddress |> Some | _ -> None)


    let tryGetServerPort (p :list<RunArgs>) =
        p |> List.tryPick (fun e -> match e with | ServicePort p -> p |> ClmSys.GeneralData.ServicePort |> Some | _ -> None)


    let tryGetServiceAccessInfo (p :list<RunArgs>) =
        match tryGetServerAddress p, tryGetServerPort p with
        | Some a, Some p ->
            {
                serviceAddress = a
                servicePort = p
            }
            |> Some
        | _ -> None

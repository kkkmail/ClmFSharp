namespace ContGenService
open Argu
open ContGenServiceInfo.ServiceInfo

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type RunArgs =
        | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
        | [<Unique>] [<AltCommandLine("-i")>] RunIdle

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | NumberOfCores _ -> "number of logical cores to use."
                | RunIdle -> "Start idle."

        //member this.configParam =
        //    match this with
        //    | NumberOfCores n -> ContGenConfigParam.SetRunLimit n
        //    | Idle -> ContGenConfigParam.SetToIdle

    and
        [<CliPrefix(CliPrefix.Dash)>]
        SvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("-i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("-u")>] Uninstall
        | [<Unique>] [<First>] Start of ParseResults<RunArgs>
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("-r")>] Run of ParseResults<RunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install service."
                | Uninstall -> "uninstall service."
                | Start _ -> "start service."
                | Stop -> "stop service."
                | Run _ -> "run service from command line without installing."

namespace ContGenService
open Argu

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type SvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("-i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("-u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("-r")>] Run

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install service."
                | Uninstall -> "uninstall service."
                | Start -> "start service."
                | Stop -> "stop service."
                | Run -> "run service from command line without installing."

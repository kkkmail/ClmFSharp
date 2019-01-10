namespace ContGenService
open Argu

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type SvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("-i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("-u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install services."
                | Uninstall -> "uninstall services."
                | Start -> "start services."
                | Stop -> "stop services."

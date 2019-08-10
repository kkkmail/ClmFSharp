namespace ContGenService

open Argu
open ClmSys.GeneralData
open ClmSys.ServiceInstaller

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type ContGenRunArgs =
        | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
        | [<Unique>] [<AltCommandLine("-i")>] RunIdle
        | [<Unique>] [<AltCommandLine("-ee")>] MinimumUsefulEe of double

        | [<Unique>] [<AltCommandLine("-address")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | NumberOfCores _ -> "number of logical cores to use."
                | RunIdle -> "Start idle."
                | MinimumUsefulEe _ -> "minimum useful ee to generate charts. Set to 0.0 to generate all charts."

                | SvcAddress _ -> "service ip address / name."
                | SvcPort _ -> "service port."

    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<ContGenRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install ContGen service."
                | Uninstall -> "uninstall ContGen service."
                | Start -> "start ContGen service."
                | Stop -> "stop ContGen service."
                | Run _ -> "run ContGen service from command line without installing."
                | Save -> "save parameters into the registry."


    type ContGenSvcArgs = SvcArguments<ContGenRunArgs>


    let convertArgs s =
        match s with
        | Install -> ContGenSvcArgs.Install
        | Uninstall -> ContGenSvcArgs.Uninstall
        | Start -> ContGenSvcArgs.Start
        | Stop -> ContGenSvcArgs.Stop
        | Run a -> ContGenSvcArgs.Run a
        | Save -> ContGenSvcArgs.Save


    let tryGetServerAddress p =
         p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort p =
        p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGeMinUsefulEe p =
        p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)


    let getServiceAccessInfo p =
        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None -> ServiceAddress.defaultValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None -> ServicePort.defaultValue

        let ee =
            match tryGeMinUsefulEe p with
            | Some e -> e
            | None -> MinUsefulEe DefaultMinEe


        {
            contGenServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }

            minUsefulEe = ee
        }

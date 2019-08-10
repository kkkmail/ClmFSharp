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


    type ContGenSvcArguments = SvcArguments<ContGenRunArgs>


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

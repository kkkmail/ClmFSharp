﻿namespace Messaging

open System
open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.Registry
open ClmSys.MessagingData

module MsgCliCommandLine =

    let logger e = printfn "Error / Exception: %A" e


    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingClientRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] MsgCliSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgCliSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgCliSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] MsgCliVersion of string
        | [<Unique>] [<AltCommandLine("-id")>] MsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-name")>] MsgCliName of string

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgCliSvcAddress _ -> "messaging server ip address / name."
                | MsgCliSvcPort _ -> "messaging server port."
                | MsgCliSaveSettings -> "saves settings to the Registry."
                | MsgCliVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | MsgCliId _ -> "id of the client - it is like an \"email\" address of a client."
                | MsgCliName _ -> "optinal client name to distinguish clients when there is more than one on a machine."


    let tryGetServerAddress p =
         p |> List.tryPick (fun e -> match e with | MsgCliSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort p =
        p |> List.tryPick (fun e -> match e with | MsgCliSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGetSaveSettings p =
        p |> List.tryPick (fun e -> match e with | MsgCliSaveSettings -> Some () | _ -> None)


    let tryGetVersion p =
        p |> List.tryPick (fun e -> match e with | MsgCliVersion p -> p |> VersionNumber |> Some | _ -> None)


    let tryGetClientId p =
        p |> List.tryPick (fun e -> match e with | MsgCliId p -> p |> MessagingClientId |> Some | _ -> None)


    let getClientName p =
        match p |> List.tryPick (fun e -> match e with | MsgCliName p -> p |> MessagingClientName.create |> Some | _ -> None) with
        | Some e -> e
        | None -> MessagingClientName.Unnamed


    let tryGetClientServiceAccessInfo p =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let name = getClientName p

        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None ->
                match tryGetMessagingClientAddress logger version name with
                | Some a -> a
                | None -> ServiceAddress.defaultMessagingServerValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None ->
                match tryGetMessagingClientPort logger version name with
                | Some a -> a
                | None -> ServicePort.defaultMessagingServerValue

        let trySaveSettings c =
            match tryGetSaveSettings p with
            | Some _ ->
                trySetMessagingClientAddress logger versionNumberValue name address |> ignore
                trySetMessagingClientPort logger versionNumberValue name port |> ignore
                trySetMessagingClientId logger versionNumberValue name c |> ignore
            | None -> ignore()

        let co =
            match tryGetClientId p with
            | Some c -> Some c
            | None ->
                match tryGetMessagingClientId logger version name with
                | Some c -> Some c
                | None -> None

        match co with
        | Some c ->
            trySaveSettings c

            {
                msgClientId = c

                msgSvcAccessInfo =
                    {
                        serviceAddress = address
                        servicePort = port
                    }
            }
            |> Some
        | None -> None


    let tryCreateX p =
        match tryGetClientServiceAccessInfo p with
        | Some x -> failwith ""
        | None -> None

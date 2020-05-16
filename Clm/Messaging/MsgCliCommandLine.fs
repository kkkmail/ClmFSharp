namespace Messaging

open System
open Argu
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.MessagingData
open Messaging.ServiceResponse
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives

module MsgCliCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingClientRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] MsgCliSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgCliSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgCliSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] MsgCliVersion of string
        | [<Unique>] [<AltCommandLine("-id")>] MsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-name")>] MsgCliName of string

        /// For debugging.
        | [<Unique>] [<AltCommandLine("-rcp")>] MsgRcpId of Guid

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgCliSvcAddress _ -> "messaging server ip address / name."
                | MsgCliSvcPort _ -> "messaging server port."
                | MsgCliSaveSettings -> "saves settings to the Registry."
                | MsgCliVersion _ -> "tries to load data from specified version instead of current version. If -save is specified, then saves data into current version."
                | MsgCliId _ -> "id of the client - it is like an \"email\" address of a client."
                | MsgCliName _ -> "optional client name to distinguish clients when there is more than one on a machine."

                /// For debugging, comment when not needed.
                | MsgRcpId _ -> "id of message recipient."


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


    let tryGetClientName p =
        p |> List.tryPick (fun e -> match e with | MsgCliName p -> p |> MessagingClientName |> Some | _ -> None)


    let tryGetRecipientId p =
        p |> List.tryPick (fun e -> match e with | MsgRcpId p -> p |> MessagingClientId |> Some | _ -> None)


    let tryGetClientServiceAccessInfo p no =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let nameOpt =
            match tryGetClientName p with
            | Some c -> Some c
            | None -> no

        match nameOpt with
        | Some (MessagingClientName name) ->
            let registryName = RegistryKeyName name

            let address =
                match tryGetServerAddress p with
                | Some a -> MessagingServiceAddress a
                | None ->
                    match tryGetMessagingServiceAddress version registryName with
                    | Ok a -> a
                    | Error _ -> MessagingServiceAddress.defaultValue

            let port =
                match tryGetServerPort p with
                | Some a -> MessagingServicePort a
                | None ->
                    match tryGetMessagingServicePort version registryName with
                    | Ok a -> a
                    | Error _ -> MessagingServicePort.defaultValue

            let trySaveSettings c =
                match tryGetSaveSettings p with
                | Some _ ->
                    trySetMessagingServiceAddress versionNumberValue registryName address |> ignore
                    trySetMessagingServicePort versionNumberValue registryName port |> ignore
                    trySetMessagingClientId versionNumberValue registryName c |> ignore
                | None -> ignore()

            let co =
                match tryGetClientId p with
                | Some c -> Ok c
                | None -> tryGetMessagingClientId version registryName

            match co with
            | Ok c ->
                trySaveSettings c

                {
                    msgClientId = c

                    msgSvcAccessInfo =
                        {
                            messagingServiceAddress = address
                            messagingServicePort = port
                            messagingServiceName = messagingServiceName
                        }
                }
                |> Some
            | Error _ -> None
        | None -> None


    let tryCreateMsgResponseHandler p no =
        match tryGetClientServiceAccessInfo p no with
        | Some i -> i |> MsgResponseHandler |> Some
        | None -> None

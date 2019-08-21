namespace Messaging

open System
open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.Registry
open ClmSys.MessagingData
open ClmSys.Logging
open Messaging.ServiceResponse
open MessagingServiceInfo.ServiceInfo

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
                | MsgCliVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | MsgCliId _ -> "id of the client - it is like an \"email\" address of a client."
                | MsgCliName _ -> "optinal client name to distinguish clients when there is more than one on a machine."

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
        | Some name ->
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
                | None -> tryGetMessagingClientId logger version name

            match co with
            | Some c ->
                trySaveSettings c

                {
                    msgClientId = c

                    msgSvcAccessInfo =
                        {
                            serviceAddress = address
                            servicePort = port
                            serviceName = MessagingServiceName
                        }
                }
                |> Some
            | None -> None
        | None -> None


    let tryCreateMsgResponseHandler p no =
        match tryGetClientServiceAccessInfo p no with
        | Some i -> MsgResponseHandler.tryCreate i
        | None -> None

namespace PartitionerService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.PartitionerData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.ServiceInstaller
open System
//open ClmSys.WorkerNodeData

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type PartitionerServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] PartSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] PartSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] PartSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] PartVersion of string
        | [<Unique>] [<AltCommandLine("-id")>] PartMsgId of Guid

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | PartSvcAddress _ -> "messaging server ip address / name."
                | PartSvcPort _ -> "messaging server port."
                | PartSaveSettings -> "saves settings to the Registry."
                | PartVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | PartMsgId _ -> "messaging client id of a partitioner service."


    type PartitionerServiceArgs = SvcArguments<PartitionerServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        PartitionerServiceArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<PartitionerServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install partitioner service."
                | Uninstall -> "uninstall partitioner service."
                | Start _ -> "start partitioner service."
                | Stop -> "stop partitioner service."
                | Run _ -> "run partitioner service from command line without installing."
                | Save -> "save parameters into the registry."


    let convertArgs s =
        match s with
        | Install -> PartitionerServiceArgs.Install
        | Uninstall -> PartitionerServiceArgs.Uninstall
        | Start -> PartitionerServiceArgs.Start
        | Stop -> PartitionerServiceArgs.Stop
        | Run a -> PartitionerServiceArgs.Run a
        | Save -> PartitionerServiceArgs.Save


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | PartSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | PartSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | PartSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | PartVersion p -> p |> VersionNumber |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | PartMsgId p -> p |> MessagingClientId |> Some | _ -> None)


    let getServiceAccessInfo p =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let name = partitionerServiceName

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

        let c =
            match tryGetClientId p with
            | Some a -> a
            | None ->
                match tryGetMessagingClientId logger version name with
                | Some a -> a
                | None -> defaultPartitionerMessagingClientId

        trySaveSettings c


        {
            partitionerMsgClientId = c

            msgSvcAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }
        }

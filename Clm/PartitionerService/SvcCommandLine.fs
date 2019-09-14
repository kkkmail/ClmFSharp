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

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type PartitionerServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-msgAddress")>] PartMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] PartMsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] PartSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] PartVersion of string
        | [<Unique>] [<AltCommandLine("-id")>] PartPartitioner of Guid

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | PartMsgSvcAddress _ -> "messaging server ip address / name."
                | PartMsgSvcPort _ -> "messaging server port."
                | PartSaveSettings -> "saves settings to the Registry."
                | PartVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | PartPartitioner _ -> "messaging client id of a partitioner service (own id)."


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


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | PartMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | PartMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | PartSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | PartVersion p -> p |> VersionNumber |> Some | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | PartPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort
    let getPartitioner = getPartitionerImpl tryGetPartitioner


    let getServiceAccessInfo p =
        let name = partitionerServiceName

        let version = getVersion p
        let msgAddress = getMsgServerAddress logger version name p
        let msgPort = getMsgServerPort logger version name p
        let partitioner = getPartitioner logger version name p

        match tryGetSaveSettings p with
        | Some _ ->
            trySetPartitionerMessagingClientId logger versionNumberValue name partitioner |> ignore

            trySetMessagingClientAddress logger versionNumberValue name msgAddress |> ignore
            trySetMessagingClientPort logger versionNumberValue name msgPort |> ignore
        | None -> ignore()

        {
            partitionerId  = partitioner

            msgSvcAccessInfo =
                {
                    serviceAddress = msgAddress
                    servicePort = msgPort
                }
        }

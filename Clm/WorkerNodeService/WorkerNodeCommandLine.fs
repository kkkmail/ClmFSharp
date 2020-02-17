﻿namespace WorkerNodeService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.PartitionerData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.ServiceInstaller
open System
open ClmSys.WorkerNodeData
open MessagingServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int

        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] WrkVersion of string

        | [<Unique>] [<AltCommandLine("-msgAddress")>] WrkMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] WrkMsgSvcPort of int

        | [<Unique>] [<AltCommandLine("-id")>] WrkMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-p")>] WrkPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-i")>] WrkInactive of bool

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | WrkSvcAddress _ -> "worker node service ip address / name."
                | WrkSvcPort _ -> "worker node service port."
                | WrkName _ -> "worker node name."
                | WrkNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."

                | WrkSaveSettings -> "saves settings to the Registry."
                | WrkVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."

                | WrkMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkMsgSvcPort _ -> "messaging server port."

                | WrkMsgCliId _ -> "messaging client id of current worker node service."
                | WrkPartitioner _ -> "messaging client id of a partitioner service."
                | WrkInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    type WorkerNodeServiceArgs = SvcArguments<WorkerNodeServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        WorkerNodeServiceArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<WorkerNodeServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<WorkerNodeServiceRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install worker node service."
                | Uninstall -> "uninstall worker node service."
                | Start _ -> "start worker node service."
                | Stop -> "stop worker node service."
                | Run _ -> "run worker node service from command line without installing."
                | Save _ -> "save parameters into the registry."


    let convertArgs s =
        match s with
        | Install -> WorkerNodeServiceArgs.Install
        | Uninstall -> WorkerNodeServiceArgs.Uninstall
        | Start -> WorkerNodeServiceArgs.Start
        | Stop -> WorkerNodeServiceArgs.Stop
        | Run a -> WorkerNodeServiceArgs.Run a
        | Save a -> WorkerNodeServiceArgs.Save a


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkName p -> Some p | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort
    let getPartitioner = getPartitionerImpl tryGetPartitioner


    let getNoOfCores logger version name p =
        let n =
            match tryGetNoOfCores p with
            | Some n -> n
            | None ->
                match tryGetNumberOfCores version name with
                | Ok x -> x
                | Error _ -> Environment.ProcessorCount / 2
        max 0 (min n Environment.ProcessorCount)


    let getServerAddress logger version name p =
        match tryGetServerAddress p with
        | Some a -> a
        | None ->
            match tryGetContGenServiceAddress version name with
            | Ok a -> a
            | Error _ -> ServiceAddress.defaultWorkerNodeServiceValue


    let getServerPort logger version name p =
        match tryGetServerPort p with
        | Some a -> a
        | None ->
            match tryGetContGenServicePort version name with
            | Ok a -> a
            | Error _ -> ServicePort.defaultWorkerNodeServiceValue


    let getClientId logger version name p =
        match tryGetClientId p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientId version name with
            | Ok a -> a |> WorkerNodeId
            | Error _ -> Guid.NewGuid() |> MessagingClientId |> WorkerNodeId


    let tryGetNodeNameImpl logger version name p =
        match tryGetNodeName p with
        | Some a -> Some a
        | None ->
            match tryGetWorkerNodeName version name with
            | Ok a -> Some a
            | Error e -> None


    let getInactive logger version name p =
        match tryGetInactive p with
        | Some a -> a
        | None ->
            match tryGetWrkInactive version name with
            | Ok a -> a
            | Error _ -> false


    let getServiceAccessInfoImpl b p =
        let name = workerNodeServiceName

        let version = getVersion p
        let address = getServerAddress logger version name p
        let port = getServerPort logger version name p
        let noOfCores = getNoOfCores logger version name p

        let msgAddress = getMsgServerAddress logger version name p
        let msgPort = getMsgServerPort logger version name p
        let partitioner = getPartitioner logger version name p
        let clientId = getClientId logger version name p
        let inactive = getInactive logger version name p

        let nodeName = tryGetNodeNameImpl logger version name p |> Option.defaultValue (clientId.value.value.ToString("N"))

        let saveSettings() =
            trySetContGenServiceAddress versionNumberValue name address |> ignore
            trySetContGenServicePort versionNumberValue name port |> ignore
            trySetWorkerNodeName versionNumberValue name nodeName |> ignore
            trySetNumberOfCores versionNumberValue name noOfCores |> ignore

            trySetMessagingClientAddress versionNumberValue name msgAddress |> ignore
            trySetMessagingClientPort versionNumberValue name msgPort |> ignore
            trySetPartitionerMessagingClientId versionNumberValue name partitioner |> ignore
            trySetMessagingClientId versionNumberValue name clientId.messagingClientId |> ignore
            trySetWrkInactive versionNumberValue name inactive |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            workNodeMsgAccessInfo =
                {
                    workerNodeId = clientId

                    msgSvcAccessInfo =
                        {
                            serviceAddress = msgAddress
                            servicePort = msgPort
                            inputServiceName = MessagingServiceName
                        }
                }

            workerNodeName = WorkerNodeName nodeName
            noOfCores = noOfCores
            isInactive = inactive
            nodePriority = WorkerNodePriority.defaultValue
            partitionerId = partitioner

            workerNodeServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                    inputServiceName = WorkerNodeServiceName
                }
        }


    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

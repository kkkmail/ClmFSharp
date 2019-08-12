namespace WorkerNodeService

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

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkMsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] WrkVersion of string
        | [<Unique>] [<AltCommandLine("-p")>] WrkPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-id")>] WrkMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int
        | [<Unique>] [<AltCommandLine("-workerAddress")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-workerPort")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-s")>] WrkStorage of Guid

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | WrkMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkMsgSvcPort _ -> "messaging server port."
                | WrkSaveSettings -> "saves settings to the Registry."
                | WrkVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | WrkPartitioner _ -> "messaging client id of a partitioner service."
                | WrkMsgCliId _ -> "messaging client id of current worker node service."
                | WrkNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."
                | WrkSvcAddress _ -> "worker node service ip address / name."
                | WrkSvcPort _ -> "worker node service port."
                | WrkStorage _ -> "messaging client id of a storage service."


    type WorkerNodeServiceArgs = SvcArguments<WorkerNodeServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        WorkerNodeServiceArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<WorkerNodeServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install worker node service."
                | Uninstall -> "uninstall worker node service."
                | Start _ -> "start worker node service."
                | Stop -> "stop worker node service."
                | Run _ -> "run worker node service from command line without installing."
                | Save -> "save parameters into the registry."


    let convertArgs s =
        match s with
        | Install -> WorkerNodeServiceArgs.Install
        | Uninstall -> WorkerNodeServiceArgs.Uninstall
        | Start -> WorkerNodeServiceArgs.Start
        | Stop -> WorkerNodeServiceArgs.Stop
        | Run a -> WorkerNodeServiceArgs.Run a
        | Save -> WorkerNodeServiceArgs.Save


    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkVersion p -> p |> VersionNumber |> Some | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)
    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetStorage p = p |> List.tryPick (fun e -> match e with | WrkStorage p -> p |> MessagingClientId |> Some | _ -> None)


    let getMsgServerAddress logger version name p =
        match tryGetMsgServerAddress p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientAddress logger version name with
            | Some a -> a
            | None -> ServiceAddress.defaultMessagingServerValue


    let getMsgServerPort logger version name p =
        match tryGetMsgServerPort p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientPort logger version name with
            | Some a -> a
            | None -> ServicePort.defaultMessagingServerValue


    let getPartitioner logger version name p =
        match tryGetPartitioner p with
        | Some x -> x
        | None ->
            match tryGetPartitionerMessagingClientId logger version name with
            | Some x -> x
            | None -> defaultPartitionerMessagingClientId
        |> PartitionerId


    let getStorage logger version name p =
        match tryGetPartitioner p with
        | Some x -> x
        | None ->
            match tryGetStorageMessagingClientId logger version name with
            | Some x -> x
            | None -> defaultStorageMessagingClientId
        |> StorageId


    let getNoOfCores logger version name p =
        let n =
            match tryGetNoOfCores p with
            | Some n -> n
            | None ->
                match tryGetNumberOfCores logger version name with
                | Some x -> x
                | None -> Environment.ProcessorCount / 2
        max 1 (min n Environment.ProcessorCount)


    let getServerAddress logger version name p =
        match tryGetServerAddress p with
        | Some a -> a
        | None ->
            match tryGetContGenServiceAddress logger version name with
            | Some a -> a
            | None -> ServiceAddress.defaultWorkerNodeServiceValue


    let getServerPort logger version name p =
        match tryGetServerPort p with
        | Some a -> a
        | None ->
            match tryGetContGenServicePort logger version name with
            | Some a -> a
            | None -> ServicePort.defaultWorkerNodeServiceValue


    let getClientId logger version name p =
        match tryGetClientId p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientId logger version name with
            | Some a -> a
            | None -> Guid.NewGuid() |> MessagingClientId


    let getServiceAccessInfo p =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let name = workerNodeServiceName

        let address = getMsgServerAddress logger version name p
        let port = getMsgServerPort logger version name p
        let partitioner = getPartitioner logger version name p
        let noOfCores = getNoOfCores logger version name p
        let wrkAddress = getServerAddress logger version name p
        let wrkPort = getServerPort logger version name p
        let clientId = getClientId logger version name p
        let storage = getStorage logger version name p

        match tryGetSaveSettings p with
        | Some _ ->
            trySetMessagingClientAddress logger versionNumberValue name address |> ignore
            trySetMessagingClientPort logger versionNumberValue name port |> ignore
            trySetPartitionerMessagingClientId logger versionNumberValue name partitioner.messagingClientId |> ignore
            trySetNumberOfCores logger versionNumberValue name noOfCores |> ignore
            trySetContGenServiceAddress logger versionNumberValue name wrkAddress |> ignore
            trySetContGenServicePort logger versionNumberValue name wrkPort |> ignore
            trySetMessagingClientId logger versionNumberValue name clientId |> ignore
            trySetStorageMessagingClientId logger versionNumberValue name storage.messagingClientId |> ignore
        | None -> ignore()

        {
            msgCliAccessInfo =
                {
                    msgClientId = clientId

                    msgSvcAccessInfo =
                        {
                            serviceAddress = address
                            servicePort = port
                        }
                }

            noOfCores = noOfCores
            partitionerId = partitioner
            storageId = storage

            wrkSvcAccessInfo =
                {
                    serviceAddress = wrkAddress
                    servicePort = wrkPort
                }
        }

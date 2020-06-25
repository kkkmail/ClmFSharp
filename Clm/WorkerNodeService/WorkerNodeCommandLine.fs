namespace WorkerNodeService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.ServiceInstaller
open System
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open FSharp.Configuration

module SvcCommandLine =

    type Settings = AppSettings<"app.config">
    
    type WorkerNodeSettings =
        {
            svcAddress : WorkerNodeServiceAddress
            svcPort : WorkerNodeServicePort
            name : WorkerNodeName
            noOfCores : int
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
            msgCliId : MessagingClientId
            partitioner : PartitionerId
            isInactive : bool
        }
        
        member w.isValid =
            w.svcPort.value.value > 0
            && w.svcAddress.value.value <> EmptyString
            && w.name.value <> EmptyString
            && w.noOfCores >= 0
            && w.msgSvcAddress.value.value <> EmptyString
            && w.msgSvcPort.value.value > 0
            && w.msgCliId.value <> Guid.Empty
            && w.partitioner.value.value <> Guid.Empty
            
        member w.trySaveSettings() =
            match w.isValid with
            | true ->
                Settings.WrkSvcAddress <- w.svcAddress.value.value
                Settings.WrkSvcPort <- w.svcPort.value.value
                Settings.WrkName <- w.name.value
                Settings.WrkNoOfCores <- w.noOfCores
                Settings.WrkMsgSvcAddress <- w.msgSvcAddress.value.value
                Settings.WrkMsgSvcPort <- w.msgSvcPort.value.value
                Settings.WrkMsgCliId <- w.msgCliId.value
                Settings.WrkPartitioner <- w.partitioner.value.value
                Settings.WrkInactive <- w.isInactive
                
                Ok()
            | false ->
                Ok()
            
            
    
    let loadSettings() =
        {
            svcAddress = Settings.WrkSvcAddress |> ServiceAddress |> WorkerNodeServiceAddress
            svcPort =  Settings.WrkSvcPort |> ServicePort |> WorkerNodeServicePort
            name  = Settings.WrkName |> WorkerNodeName
            noOfCores  = Settings.WrkNoOfCores
            msgSvcAddress  = Settings.WrkMsgSvcAddress |> ServiceAddress |> MessagingServiceAddress
            msgSvcPort  = Settings.WrkMsgSvcPort |> ServicePort |> MessagingServicePort
            msgCliId  = Settings.WrkMsgCliId |> MessagingClientId
            partitioner  = Settings.WrkPartitioner |> MessagingClientId |> PartitionerId
            isInactive  = Settings.WrkInactive
        }
        

    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int

//        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings
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

//                | WrkSaveSettings -> "saves settings to the Registry."
                | WrkVersion _ -> "tries to load data from specified version instead of current version. If -save is specified, then saves data into current version."

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


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> WorkerNodeServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> WorkerNodeServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkName p -> Some p | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)

//    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServerPort = getMsgServicePortImpl tryGetMsgServicePort
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


    let getServiceAddress logger version name p =
        match tryGetServiceAddress p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServiceAddress version name with
            | Ok a -> a
            | Error _ -> WorkerNodeServiceAddress.defaultValue


    let getServicePort logger version name p =
        match tryGetServicePort p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServicePort version name with
            | Ok a -> a
            | Error _ -> WorkerNodeServicePort.defaultValue


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
        let name = workerNodeServiceRegistryName

        let version = getVersion p
        let noOfCores = getNoOfCores logger version name p
        let address = getServiceAddress logger version name p
        let port = getServicePort logger version name p

        let msgAddress = getMsgServerAddress logger version name p
        let msgPort = getMsgServerPort logger version name p
        let partitioner = getPartitioner logger version name p
        let clientId = getClientId logger version name p
        let inactive = getInactive logger version name p

        let nodeName =
            tryGetNodeNameImpl logger version name p |> Option.defaultValue (clientId.value.value.ToString("N"))
            |> WorkerNodeName

        let saveSettings() =
            trySetWorkerNodeServiceAddress versionNumberValue name address |> ignore
            trySetWorkerNodeServicePort versionNumberValue name port |> ignore
            trySetWorkerNodeName versionNumberValue name nodeName |> ignore
            trySetNumberOfCores versionNumberValue name noOfCores |> ignore
            trySetMessagingServiceAddress versionNumberValue name msgAddress |> ignore
            trySetMessagingServicePort versionNumberValue name msgPort |> ignore
            trySetPartitionerMessagingClientId versionNumberValue name partitioner |> ignore
            trySetMessagingClientId versionNumberValue name clientId.messagingClientId |> ignore
            trySetWrkInactive versionNumberValue name inactive |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            workerNodeInfo =
                {
                    workerNodeId = clientId
                    workerNodeName = nodeName
                    partitionerId = partitioner
                    noOfCores = noOfCores
                    nodePriority = WorkerNodePriority.defaultValue
                    isInactive = inactive
                    lastErrorDateOpt = None
                }

            workerNodeServiceAccessInfo =
                {
                    workerNodeServiceAddress = address
                    workerNodeServicePort = port
                    workerNodeServiceName = workerNodeServiceName
                }

            messagingServiceAccessInfo =
                {
                    messagingServiceAddress = msgAddress
                    messagingServicePort = msgPort
                    messagingServiceName = messagingServiceName
                }
        }


    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

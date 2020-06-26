namespace WorkerNodeService

open System
open FSharp.Configuration
open Argu

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.ClmErrors
open ClmSys.PartitionerData
open WorkerNodeServiceInfo.ServiceInfo

module SvcCommandLine =

//    type WorkerNodeAppSettings = AppSettings<"app.config">
    
    
    type WorkerNodeSettings
        with            
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    WorkerNodeAppSettings.WorkerNodeSvcAddress <- w.workerNodeSvcAddress.value.value
                    WorkerNodeAppSettings.WorkerNodeSvcPort <- w.workerNodeSvcPort.value.value
                    WorkerNodeAppSettings.WorkerNodeName <- w.workerNodeName.value
                    WorkerNodeAppSettings.WorkerNodeId <- w.workerNodeId.value.value
                    WorkerNodeAppSettings.NoOfCores <- w.noOfCores
                    WorkerNodeAppSettings.MsgSvcAddress <- w.msgSvcAddress.value.value
                    WorkerNodeAppSettings.MsgSvcPort <- w.msgSvcPort.value.value
                    WorkerNodeAppSettings.PartitionerId <- w.partitioner.value.value
                    WorkerNodeAppSettings.IsInactive <- w.isInactive
                    
                    Ok()
                with
                | e -> e |> WrkSettingExn |> WrkSettingsErr |> WorkerNodeErr |> Error
            | Error e -> Error e
            
    
    let loadSettings() =
        {
            workerNodeSvcAddress =
                match WorkerNodeAppSettings.WorkerNodeSvcAddress with
                | EmptyString -> WorkerNodeServiceAddress.defaultValue
                | s -> s |> ServiceAddress |> WorkerNodeServiceAddress
            workerNodeSvcPort =
                match WorkerNodeAppSettings.WorkerNodeSvcPort with
                | n when n > 0 -> n |> ServicePort |> WorkerNodeServicePort
                | _ -> WorkerNodeServicePort.defaultValue
            workerNodeName  = WorkerNodeAppSettings.WorkerNodeName |> WorkerNodeName
            noOfCores = WorkerNodeAppSettings.NoOfCores
            msgSvcAddress =
                match WorkerNodeAppSettings.MsgSvcAddress with
                | EmptyString -> MessagingServiceAddress.defaultValue
                | s -> s |> ServiceAddress |> MessagingServiceAddress
            msgSvcPort =
                match WorkerNodeAppSettings.MsgSvcPort with
                | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                | _ -> MessagingServicePort.defaultValue
            workerNodeId = WorkerNodeAppSettings.WorkerNodeId |> MessagingClientId |> WorkerNodeId
            partitioner =
                match WorkerNodeAppSettings.PartitionerId with
                | p when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                | _ -> defaultPartitionerId
            isInactive = WorkerNodeAppSettings.IsInactive
        }
        

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
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkName p -> p |> WorkerNodeName |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkVersion p -> p |> VersionNumber |> Some | _ -> None)
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)


    let getNoOfCores (w: WorkerNodeSettings) p =
        let n = tryGetNoOfCores p |> Option.defaultValue w.noOfCores
        max 0 (min n Environment.ProcessorCount)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress (w: WorkerNodeSettings) p = tryGetMsgServiceAddress p |> Option.defaultValue w.msgSvcAddress
    let getMsgServerPort (w: WorkerNodeSettings) p = tryGetMsgServicePort p |> Option.defaultValue w.msgSvcPort
    let getPartitioner (w: WorkerNodeSettings) p = tryGetPartitioner p |> Option.defaultValue w.partitioner
    let getServiceAddress (w: WorkerNodeSettings) p = tryGetServiceAddress p |> Option.defaultValue w.workerNodeSvcAddress
    let getServicePort (w: WorkerNodeSettings) p = tryGetServicePort p |> Option.defaultValue w.workerNodeSvcPort
    let getWorkerNodeId (w: WorkerNodeSettings) p = tryGetClientId p |> Option.defaultValue w.workerNodeId
    let getNodeName (w: WorkerNodeSettings) p = tryGetNodeName p |> Option.defaultValue w.workerNodeName
    let getInactive (w: WorkerNodeSettings) p = tryGetInactive p |> Option.defaultValue w.isInactive


    let getServiceAccessInfoImpl b p =
        let w = loadSettings()
        
        let w1 =
            {
                workerNodeSvcAddress = getServiceAddress w p
                workerNodeSvcPort = getServicePort w p
                workerNodeName = getNodeName w p
                noOfCores = getNoOfCores w p
                msgSvcAddress = getMsgServerAddress w p
                msgSvcPort = getMsgServerPort w p
                workerNodeId = getWorkerNodeId w p
                partitioner = getPartitioner w p
                isInactive = getInactive w p              
            }
            
        printfn "getServiceAccessInfoImpl: w1 = %A" w1    
            
        let g() =
            {
                workerNodeInfo =
                    {
                        workerNodeId = w1.workerNodeId
                        workerNodeName = w1.workerNodeName
                        partitionerId = w1.partitioner
                        noOfCores = w1.noOfCores
                        nodePriority = WorkerNodePriority.defaultValue
                        isInactive = w1.isInactive
                        lastErrorDateOpt = None
                    }

                workerNodeServiceAccessInfo =
                    {
                        workerNodeServiceAddress = w1.workerNodeSvcAddress
                        workerNodeServicePort = w1.workerNodeSvcPort
                        workerNodeServiceName = workerNodeServiceName
                    }

                messagingServiceAccessInfo =
                    {
                        messagingServiceAddress = w1.msgSvcAddress
                        messagingServicePort = w1.msgSvcPort
                        messagingServiceName = messagingServiceName
                    }
            }            

        let r =           
            match tryGetSaveSettings p, b with
            | Some _, _ -> w1.trySaveSettings()
            | _, true -> w1.trySaveSettings()
            | _ -> w1.isValid()
            
        printfn "getServiceAccessInfoImpl: r = %A" r    

        match r with
        | Ok() -> g() |> Ok
        | Error e -> Error e
        

    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

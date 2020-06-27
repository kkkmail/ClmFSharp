namespace WorkerNodeService

open System
open Argu

open ClmSys.ServiceInstaller
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open WorkerNodeServiceInfo.ServiceInfo

module SvcCommandLine =
    
    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int

        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings

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
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)


    let getNoOfCores (w: WorkerNodeSettings) p =
        let n = tryGetNoOfCores p |> Option.defaultValue w.noOfCores
        max 0 (min n Environment.ProcessorCount)


    let getMsgServerAddress (w: WorkerNodeSettings) p = tryGetMsgServiceAddress p |> Option.defaultValue w.msgSvcAddress
    let getMsgServerPort (w: WorkerNodeSettings) p = tryGetMsgServicePort p |> Option.defaultValue w.msgSvcPort
    let getPartitioner (w: WorkerNodeSettings) p = tryGetPartitioner p |> Option.defaultValue w.partitioner
    let getServiceAddress (w: WorkerNodeSettings) p = tryGetServiceAddress p |> Option.defaultValue w.workerNodeSvcAddress
    let getServicePort (w: WorkerNodeSettings) p = tryGetServicePort p |> Option.defaultValue w.workerNodeSvcPort
    let getWorkerNodeId (w: WorkerNodeSettings) p = tryGetClientId p |> Option.defaultValue w.workerNodeId
    let getNodeName (w: WorkerNodeSettings) p = tryGetNodeName p |> Option.defaultValue w.workerNodeName
    let getInactive (w: WorkerNodeSettings) p = tryGetInactive p |> Option.defaultValue w.isInactive

    let loadSettings p =
        let w = loadWorkerNodeSettings()
        
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
            
        printfn "loadSettings: w1 = %A" w1    
        w1

    
    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let trySave() = tryGetSaveSettings p
        getWorkerNodeServiceAccessInfo (load, trySave) b

    
    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

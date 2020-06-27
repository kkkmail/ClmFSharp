namespace WorkerNodeAdm

open Argu
open System
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.GeneralData
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open WorkerNodeServiceInfo.ServiceInfo

module AdmCommandLine =

    let WrkAdmAppName = "WorkerNodeAdm.exe"


    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeAdmArgs =
        //| [<Unique>] [<First>] [<AltCommandLine("c")>] ConfigureWrkService
        | [<Unique>] [<First>] [<AltCommandLine("m")>] MonitorWrkService

        | [<Unique>] [<AltCommandLine("-address")>] WrkAdmSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkAdmSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkAdmName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkAdmNoOfCores of int

        | [<Unique>] [<AltCommandLine("-save")>] WrkAdmSaveSettings

        | [<Unique>] [<AltCommandLine("-msgAddress")>] WrkAdmMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] WrkAdmMsgSvcPort of int

        | [<Unique>] [<AltCommandLine("-id")>] WrkAdmMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-p")>] WrkAdmPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-i")>] WrkAdmInactive of bool

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                //| ConfigureWrkService -> "configures running worker node service."
                | MonitorWrkService -> "monitors running worker node service."

                | WrkAdmSvcAddress _ -> "worker node service ip address / name."
                | WrkAdmSvcPort _ -> "worker node service port."
                | WrkAdmName _ -> "worker node name."
                | WrkAdmNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."

                | WrkAdmSaveSettings -> "saves settings to the Registry."

                | WrkAdmMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkAdmMsgSvcPort _ -> "messaging server port."

                | WrkAdmMsgCliId _ -> "messaging client id of current worker node service."
                | WrkAdmPartitioner _ -> "messaging client id of a partitioner service."
                | WrkAdmInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcAddress s -> s |> ServiceAddress |> WorkerNodeServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcPort p -> p |> ServicePort |> WorkerNodeServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkAdmName p -> p |> WorkerNodeName |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkAdmNoOfCores p -> Some p | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkAdmSaveSettings -> Some () | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkAdmPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkAdmInactive p -> Some p | _ -> None)


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
        WorkerNodeAppSettings.SelectExecutableFile(getFileName workerNodeServiceProgramName)
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
    

    let getServiceAccessInfo p = getServiceAccessInfoImpl false p
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

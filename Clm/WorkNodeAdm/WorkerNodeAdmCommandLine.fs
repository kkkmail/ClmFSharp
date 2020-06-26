namespace WorkerNodeAdm

open Argu
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.GeneralData
open ClmSys.Logging
open System
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives

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
        | [<Unique>] [<AltCommandLine("-version")>] WrkAdmVersion of string

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
                | WrkAdmVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."

                | WrkAdmMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkAdmMsgSvcPort _ -> "messaging server port."

                | WrkAdmMsgCliId _ -> "messaging client id of current worker node service."
                | WrkAdmPartitioner _ -> "messaging client id of a partitioner service."
                | WrkAdmInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcAddress s -> s |> ServiceAddress |> WorkerNodeServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcPort p -> p |> ServicePort |> WorkerNodeServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkAdmName p -> Some p | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkAdmNoOfCores p -> Some p | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkAdmSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkAdmVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkAdmPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkAdmInactive p -> Some p | _ -> None)


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServiceAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServicePort = getMsgServicePortImpl tryGetMsgServicePort
    let getPartitioner = getPartitionerImpl tryGetPartitioner


    let getNoOfCores logger version name p =
        let n =
            match tryGetNoOfCores p with
            | Some n -> n
            | None ->
                match tryGetNumberOfCores version name with
                | Ok x -> x
                | Error e -> Environment.ProcessorCount / 2
        max 0 (min n Environment.ProcessorCount)


    let getServiceAddress logger version name p =
        match tryGetServiceAddress p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServiceAddress version name with
            | Ok a -> a
            | Error e -> WorkerNodeServiceAddress.defaultValue


    let getServicePort logger version name p =
        match tryGetServicePort p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServicePort version name with
            | Ok a -> a
            | Error e -> WorkerNodeServicePort.defaultValue


    let getClientId logger version name p =
        match tryGetClientId p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientId version name with
            | Ok a -> a |> WorkerNodeId
            | Error e -> Guid.NewGuid() |> MessagingClientId |> WorkerNodeId


    let tryGetNodeNameImpl logger version name p =
        match tryGetNodeName p with
        | Some a -> Some a
        | None ->
            match tryGetWorkerNodeName version name with
            | Ok a -> Some a
            | Error e -> None
        |> Option.bind (fun e -> e |> WorkerNodeName |> Some)


    let getInactive logger version name p =
        match tryGetInactive p with
        | Some a -> a
        | None ->
            match tryGetWrkInactive version name with
            | Ok a -> a
            | Error e -> false


    let getServiceAccessInfoImpl b p =
        let name = workerNodeServiceRegistryName

        let version = getVersion p
        let address = getServiceAddress logger version name p
        let port = getServicePort logger version name p
        let noOfCores = getNoOfCores logger version name p

        let msgAddress = getMsgServiceAddress logger version name p
        let msgPort = getMsgServicePort logger version name p
        let partitioner = getPartitioner logger version name p
        let clientId = getClientId logger version name p
        let inactive = getInactive logger version name p

        match tryGetNodeNameImpl logger version name p with
        | Some nodeName ->
            let saveSettings() =
                trySetWorkerNodeServiceAddress versionNumberValue name address|> ignore
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
            |> Some
        | None -> None


    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

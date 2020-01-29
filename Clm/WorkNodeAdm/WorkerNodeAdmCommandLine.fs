namespace WorkerNodeAdm

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.Registry
open ClmSys.Logging
open System
open ClmSys.WorkerNodeData
open MessagingServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives

module AdmCommandLine =

    let WrkAdmAppName = "WorkerNodeAdm.exe"


    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeAdmArgs =
        | [<Unique>] [<First>] [<AltCommandLine("c")>] ConfigureWrkService
        | [<Unique>] [<First>] [<AltCommandLine("m")>] MonitorWrkService

        | [<Unique>] [<AltCommandLine("-address")>] WrkAdmSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkAdmSvcPort of int
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
                | ConfigureWrkService -> "configures running worker node service."
                | MonitorWrkService -> "monitors running worker node service."

                | WrkAdmSvcAddress _ -> "worker node service ip address / name."
                | WrkAdmSvcPort _ -> "worker node service port."
                | WrkAdmNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."

                | WrkAdmSaveSettings -> "saves settings to the Registry."
                | WrkAdmVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."

                | WrkAdmMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkAdmMsgSvcPort _ -> "messaging server port."

                | WrkAdmMsgCliId _ -> "messaging client id of current worker node service."
                | WrkAdmPartitioner _ -> "messaging client id of a partitioner service."
                | WrkAdmInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkAdmNoOfCores p -> Some p | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkAdmSaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | WrkAdmVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkAdmPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkAdmInactive p -> Some p | _ -> None)


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
                | Error e -> Environment.ProcessorCount / 2
        max 0 (min n Environment.ProcessorCount)


    let getServerAddress logger version name p =
        match tryGetServerAddress p with
        | Some a -> a
        | None ->
            match tryGetContGenServiceAddress version name with
            | Ok a -> a
            | Error e -> ServiceAddress.defaultWorkerNodeServiceValue


    let getServerPort logger version name p =
        match tryGetServerPort p with
        | Some a -> a
        | None ->
            match tryGetContGenServicePort version name with
            | Ok a -> a
            | Error e -> ServicePort.defaultWorkerNodeServiceValue


    let getClientId logger version name p =
        match tryGetClientId p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientId version name with
            | Ok a -> a |> WorkerNodeId
            | Error e -> Guid.NewGuid() |> MessagingClientId |> WorkerNodeId


    let getInactive logger version name p =
        match tryGetInactive p with
        | Some a -> a
        | None ->
            match tryGetWrkInactive version name with
            | Ok a -> a
            | Error e -> false


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

        let saveSettings() =
            trySetContGenServiceAddress versionNumberValue name address |> ignore
            trySetContGenServicePort versionNumberValue name port |> ignore
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

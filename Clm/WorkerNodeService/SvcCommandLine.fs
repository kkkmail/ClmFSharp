namespace WorkerNodeService

open Argu
open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.PartitionerData
open ClmSys.Registry
open System
open ClmSys.WorkerNodeData

module SvcCommandLine =

    let logger m e = printfn "Error / Exception for %A: %A" m e


    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] WrkVersion of string
        | [<Unique>] [<AltCommandLine("-p")>] WrkPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-id")>] WrkMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | WrkSvcAddress _ -> "messaging server ip address / name."
                | WrkSvcPort _ -> "messaging server port."
                | WrkSaveSettings -> "saves settings to the Registry."
                | WrkVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | WrkPartitioner _ -> "messaging client id of a partitioner service."
                | WrkMsgCliId _ -> "messaging client id of current worker node service."
                | WrkNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."


    and
        [<CliPrefix(CliPrefix.None)>]
        MsgSvcArguments =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start of ParseResults<WorkerNodeServiceRunArgs>
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
                | Save -> "save parameters into the registry without running."


    let tryGetServerAddress p =
         p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort p =
        p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGetSaveSettings p =
        p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)


    let tryGetVersion p =
        p |> List.tryPick (fun e -> match e with | WrkVersion p -> p |> VersionNumber |> Some | _ -> None)


    let tryGetPartitioner p =
        p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> Some | _ -> None)


    let tryGetClientId p =
        p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> Some | _ -> None)


    let tryGetNoOfCores p =
        p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)


    let getServiceAccessInfo p =
        let version =
            match tryGetVersion p with
            | Some x -> x
            | None -> versionNumberValue

        let name = workerNodeServiceName

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

        let partitioner =
            match tryGetPartitioner p with
            | Some x -> x
            | None ->
                match tryGetPartitionerMessagingClientId logger version name with
                | Some x -> x
                | None -> PartitionerData.defaultValue.partitionerMessangerClientId

        let noOfCores =
            let n =
                match tryGetNoOfCores p with
                | Some n -> n
                | None -> Environment.ProcessorCount / 2
            max 1 (min n Environment.ProcessorCount)

        let trySaveSettings c =
            match tryGetSaveSettings p with
            | Some _ ->
                trySetMessagingClientAddress logger versionNumberValue name address |> ignore
                trySetMessagingClientPort logger versionNumberValue name port |> ignore
                trySetMessagingClientId logger versionNumberValue name c |> ignore
                trySetPartitionerMessagingClientId logger versionNumberValue name partitioner |> ignore
                trySetNumberOfCores logger versionNumberValue name noOfCores |> ignore
            | None -> ignore()

        let c =
            match tryGetClientId p with
            | Some a -> a
            | None ->
                match tryGetMessagingClientId logger version name with
                | Some a -> a
                | None -> Guid.NewGuid() |> MessagingClientId

        trySaveSettings c

        {
            wrkMsgClientId  = c
            prtMsgClientId = partitioner
            noOfCores = noOfCores

            msgSvcAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                }
        }

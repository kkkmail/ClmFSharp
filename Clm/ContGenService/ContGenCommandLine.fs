namespace ContGenService

open Argu
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open System
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.Logging
open ClmSys.MessagingData
open ServiceProxy.Runner
open ContGen.Partitioner
open ServiceProxy.PartitionerProxy
open Messaging.ServiceResponse
open ServiceProxy.MsgServiceProxy
open ContGenServiceInfo.ServiceInfo
open MessagingServiceInfo.ServiceInfo

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type ContGenRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int

        | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
        | [<Unique>] [<AltCommandLine("-i")>] RunIdle
        | [<Unique>] [<AltCommandLine("-ee")>] MinimumUsefulEe of double

        | [<Unique>] [<AltCommandLine("-save")>] SaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] ContGenVersion of string

        | [<Unique>] [<AltCommandLine("-msgAddress")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] MsgSvcPort of int

        | [<Unique>] [<AltCommandLine("-p")>] Partitioner of Guid
        | [<Unique>] [<AltCommandLine("-u")>] UsePartitioner of bool

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | SvcAddress _ -> "cont gen service ip address / name."
                | SvcPort _ -> "cont gen service port."

                | NumberOfCores _ -> "number of logical cores to use."
                | RunIdle -> "Start idle."
                | MinimumUsefulEe _ -> "minimum useful ee to generate charts. Set to 0.0 to generate all charts."

                | SaveSettings -> "saves settings to the Registry."
                | ContGenVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."

                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."

                | Partitioner _ -> "messaging client id of a partitioner service."
                | UsePartitioner _ -> "if true, then use partitioner, otherwise don't use it (default)."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<ContGenRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<ContGenRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install ContGen service."
                | Uninstall -> "uninstall ContGen service."
                | Start -> "start ContGen service."
                | Stop -> "stop ContGen service."
                | Run _ -> "run ContGen service from command line without installing."
                | Save _ -> "save parameters into the registry."


    type ContGenSvcArgs = SvcArguments<ContGenRunArgs>


    let convertArgs s =
        match s with
        | Install -> ContGenSvcArgs.Install
        | Uninstall -> ContGenSvcArgs.Uninstall
        | Start -> ContGenSvcArgs.Start
        | Stop -> ContGenSvcArgs.Stop
        | Run a -> ContGenSvcArgs.Run a
        | Save a -> ContGenSvcArgs.Save a


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGeMinUsefulEe p = p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | SaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | ContGenVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetMsgServerAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServerPort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetUsePartitioner p = p |> List.tryPick (fun e -> match e with | UsePartitioner p -> Some p | _ -> None)


    let getServerAddress logger version name p =
        match tryGetServerAddress p with
        | Some a -> a
        | None ->
            match tryGetContGenServiceAddress logger version name with
            | Some a -> a
            | None -> ServiceAddress.defaultContGenServiceValue


    let getServerPort logger version name p =
        match tryGetServerPort p with
        | Some a -> a
        | None ->
            match tryGetContGenServicePort logger version name with
            | Some a -> a
            | None -> ServicePort.defaultContGenServiceValue


    let geMinUsefulEe logger version name p =
        match tryGeMinUsefulEe p with
        | Some e -> e
        | None ->
            match tryGetContGenMinUsefulEe logger version name with
            | Some e -> e
            | None -> MinUsefulEe DefaultMinEe


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress = getMsgServerAddressImpl tryGetMsgServerAddress
    let getMsgServerPort = getMsgServerPortImpl tryGetMsgServerPort
    let getPartitioner = getPartitionerImpl tryGetPartitioner
    let getUsePartitioner = getUsePartitionerImpl tryGetUsePartitioner


    /// TODO kk:20190816 - Refactor getServiceAccessInfo + getServiceProxy into one function.
    let getServiceAccessInfoImpl b p =
        let name = contGenServiceName

        let version = getVersion p
        let address = getServerAddress logger version name p
        let port = getServerPort logger version name p
        let ee = geMinUsefulEe logger version name p

        let msgAddress = getMsgServerAddress logger version name p
        let msgPort = getMsgServerPort logger version name p

        let partitioner = getPartitioner logger version name p
        let usePartitioner = getUsePartitioner logger version name p

        let saveSettings() =
            trySetContGenServiceAddress logger versionNumberValue name address |> ignore
            trySetContGenServicePort logger versionNumberValue name port |> ignore

            trySetMessagingClientAddress logger versionNumberValue name msgAddress |> ignore
            trySetMessagingClientPort logger versionNumberValue name msgPort |> ignore

            trySetPartitionerMessagingClientId logger versionNumberValue name partitioner |> ignore
            trySetUsePartitioner logger versionNumberValue name usePartitioner |> ignore

            trySetContGenMinUsefulEe logger versionNumberValue name ee |> ignore

        match tryGetSaveSettings p, b with
        | Some _, _ -> saveSettings()
        | _, true -> saveSettings()
        | _ -> ignore()

        {
            contGenServiceAccessInfo =
                {
                    serviceAddress = address
                    servicePort = port
                    inputServiceName = ContGenServiceName
                }

            minUsefulEe = ee
        }


    /// TODO kk:20190816 - Refactor getServiceAccessInfo + getServiceProxy into one function.
    let getServiceProxy p =
        let name = contGenServiceName
        let version = getVersion p

        let msgAddress = getMsgServerAddress logger version name p
        let msgPort = getMsgServerPort logger version name p
        let partitioner = getPartitioner logger version name p
        let usePartitioner = getUsePartitioner logger version name p

        let localRunner() = LocalRunnerConfig.defaultValue |> LocalRunnerProxy |> RunnerProxy.create, None

        match usePartitioner with
        | false -> localRunner()
        | true ->
            let w =
                {
                    partitionerId = partitioner
                    msgSvcAccessInfo =
                        {
                            serviceAddress = msgAddress
                            servicePort = msgPort
                            inputServiceName = MessagingServiceName
                        }
                }

            let m = MsgResponseHandler (w.messagingClientAccessInfo)

            let q =
                {
                    partitionerMsgAccessInfo = w
                    partitionerProxy = PartitionerProxy.create PartitionerProxyInfo.defaultValue
                    messagingService = m
                    msgClientProxy = MessagingClientProxy.create { messagingClientName = contGenServiceName }
                    logger = logger
                }

            let r = createServiceImpl q
            PartitionerRunnerConfig.defaultValue r.runModel |> PartitionerRunnerProxy |> RunnerProxy.create, Some r


    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore

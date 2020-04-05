namespace ContGenService

open Argu
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open System
open ClmSys.VersionInfo
open ClmSys.Registry
open ClmSys.Logging
open Messaging.ServiceResponse
open ServiceProxy.MsgServiceProxy
open Messaging.Client
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ContGen.ModelRunner
open DbData.Configuration
open Clm.ModelParams
open ClmSys.MessagingData

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type ContGenRunArgs =
        | [<Unique>] [<AltCommandLine("-ee")>] MinimumUsefulEe of double
        | [<Unique>] [<AltCommandLine("-save")>] SaveSettings
        | [<Unique>] [<AltCommandLine("-version")>] ContGenVersion of string
        | [<Unique>] [<AltCommandLine("-p")>] Partitioner of Guid
        | [<Unique>] [<AltCommandLine("-msgAddress")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] MsgSvcPort of int

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MinimumUsefulEe _ -> "minimum useful ee to generate charts. Set to 0.0 to generate all charts."
                | SaveSettings -> "saves settings to the Registry."
                | ContGenVersion _ -> "tries to load data from specfied version instead of current version. If -save is specified, then saves data into current version."
                | Partitioner _ -> "messaging client id of a partitioner service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."

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


    let tryGeMinUsefulEe p = p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | SaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | ContGenVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)


    let geMinUsefulEe logger version name p =
        match tryGeMinUsefulEe p with
        | Some e -> e
        | None ->
            match tryGetContGenMinUsefulEe version name with
            | Ok e -> e
            | Error _ -> MinUsefulEe DefaultMinEe


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServiceAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServicePort = getMsgServicePortImpl tryGetMsgServicePort
    let getPartitioner = getPartitionerImpl tryGetPartitioner


    let saveSettings p =
        let name = contGenServiceRegistryName
        let version = getVersion p

        let ee = geMinUsefulEe logger version name p
        let msgAddress = getMsgServiceAddress logger version name p
        let msgPort = getMsgServicePort logger version name p

        let partitioner = getPartitioner logger version name p
        let usePartitioner = true

        let saveSettings() =
            trySetMessagingServiceAddress versionNumberValue name msgAddress |> ignore
            trySetMessagingServicePort versionNumberValue name msgPort |> ignore
            trySetPartitionerMessagingClientId versionNumberValue name partitioner |> ignore
            trySetUsePartitioner versionNumberValue name usePartitioner |> ignore
            trySetContGenMinUsefulEe versionNumberValue name ee |> ignore

        match tryGetSaveSettings p with
        | Some() -> saveSettings()
        | None -> ignore()


    let createModelRunnerImpl (logger : Logger) (p : list<ContGenRunArgs>) : ModelRunner =
        let name = contGenServiceRegistryName
        let version = getVersion p

        let msgAddress = getMsgServiceAddress logger version name p
        let msgPort = getMsgServicePort logger version name p
        let partitioner = getPartitioner logger version name p

        let i =
            {
                msgClientId = partitioner.messagingClientId

                msgSvcAccessInfo =
                    {
                        messagingServiceAddress = msgAddress
                        messagingServicePort = msgPort
                        messagingServiceName = messagingServiceName
                    }
            }

        let m = MsgResponseHandler i

        let messagingClientData =
            {
                msgAccessInfo = i
                messagingService = m
                msgClientProxy = MessagingClientProxy.create { messagingClientName = contGenServiceName.value.messagingClientName }
            }

        let messagingClient = MessagingClient messagingClientData

        match messagingClient.start() with
        | Ok() -> createMessagingClientEventHandlers logger messagingClient
        | Error e -> logger.logError e

        let data =
            {
                connectionString = clmConnectionString
                minUsefulEe = MinUsefulEe.defaultValue
                resultLocation = DefaultResultLocationFolder
            }

        let modelRunner = ModelRunner.create logger data messagingClient.messageProcessorProxy
        modelRunner

namespace ContGenService

open Argu
open FSharp.Configuration
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open System
open ClmSys.VersionInfo
open ClmSys.Logging
open Messaging.ServiceResponse
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.Client
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ContGen.ModelRunner
open ClmSys.MessagingData
open ClmSys.ContGenData
open Clm.ModelParams
open DbData.Configuration
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.PartitionerData

module SvcCommandLine =

    type ContGenAppSettings = AppSettings<"app.config">
    
    
    type ContGenSettings
        with            
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    ContGenAppSettings.ContGenSvcAddress <- w.contGenSvcAddress.value.value
                    ContGenAppSettings.ContGenSvcPort <- w.contGenSvcPort.value.value
                    ContGenAppSettings.MinUsefulEe <- w.minUsefulEe.value
                    ContGenAppSettings.MsgSvcAddress <- w.msgSvcAddress.value.value
                    ContGenAppSettings.MsgSvcPort <- w.msgSvcPort.value.value
                    ContGenAppSettings.PartitionerId <- w.partitionerId.value.value
                    
                    Ok()
                with
                | e -> e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error
            | Error e -> Error e    

    
    type ContGenServiceData =
        {
            modelRunnerData : ModelRunnerDataWithProxy
            contGenServiceAccessInfo : ContGenServiceAccessInfo
        }


    [<CliPrefix(CliPrefix.Dash)>]
    type ContGenRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int
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
                | SvcAddress _ -> "cont gen service ip address / name."
                | SvcPort _ -> "cont gen service port."
                | MinimumUsefulEe _ -> "minimum useful ee to generate charts. Set to 0.0 to generate all charts."
                | SaveSettings -> "saves settings to the Registry."
                | ContGenVersion _ -> "tries to load data from specified version instead of current version. If -save is specified, then saves data into current version."
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


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> ContGenServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> ContGenServicePort |> Some | _ -> None)

    let tryGeMinUsefulEe p = p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | SaveSettings -> Some () | _ -> None)
    let tryGetVersion p = p |> List.tryPick (fun e -> match e with | ContGenVersion p -> p |> VersionNumber |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)


    let geMinUsefulEe (w: ContGenSettings) p = tryGeMinUsefulEe p |> Option.defaultValue w.minUsefulEe


    let getVersion = getVersionImpl tryGetVersion
    let getMsgServerAddress (w: ContGenSettings) p = tryGetMsgServiceAddress p |> Option.defaultValue w.msgSvcAddress
    let getMsgServerPort (w: ContGenSettings) p = tryGetMsgServicePort p |> Option.defaultValue w.msgSvcPort
    let getPartitionerId (w: ContGenSettings) p = tryGetPartitioner p |> Option.defaultValue w.partitionerId
    let getServiceAddress (w: ContGenSettings) p = tryGetServerAddress p |> Option.defaultValue w.contGenSvcAddress
    let getServicePort (w: ContGenSettings) p = tryGetServerPort p |> Option.defaultValue w.contGenSvcPort

    
    let loadSettings p =
        let w =
            {
                contGenSvcAddress =
                    match ContGenAppSettings.ContGenSvcAddress with
                    | EmptyString -> ContGenServiceAddress.defaultValue
                    | s -> s |> ServiceAddress |> ContGenServiceAddress
                contGenSvcPort =
                    match ContGenAppSettings.ContGenSvcPort with
                    | n when n > 0 -> n |> ServicePort |> ContGenServicePort
                    | _ -> ContGenServicePort.defaultValue
                minUsefulEe = ContGenAppSettings.MinUsefulEe |> MinUsefulEe
                msgSvcAddress =
                    match ContGenAppSettings.MsgSvcAddress with
                    | EmptyString -> MessagingServiceAddress.defaultValue
                    | s -> s |> ServiceAddress |> MessagingServiceAddress
                msgSvcPort =
                    match ContGenAppSettings.MsgSvcPort with
                    | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                    | _ -> MessagingServicePort.defaultValue
                partitionerId =
                    match ContGenAppSettings.PartitionerId with
                    | p when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                    | _ -> defaultPartitionerId
            }

        let w1 =
            {
                contGenSvcAddress = getServiceAddress w p
                contGenSvcPort = getServicePort w p
                minUsefulEe = geMinUsefulEe w p
                msgSvcAddress = getMsgServerAddress w p
                msgSvcPort = getMsgServerPort w p
                partitionerId = getPartitionerId w p
            }
            
        w1


    let saveSettings p =
        let w = loadSettings p
        
        let r =
            match tryGetSaveSettings p with
            | Some() -> w.trySaveSettings()
            | None -> Ok()
            
        match r with            
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn "Error occurred trying to save settings: %A." e
        

    /// TODO kk:20200517 - Propagate early exit info to command line parameters.
    let getContGenServiceData (logger : Logger) (p : list<ContGenRunArgs>) =
        let w = loadSettings p
        printfn "getContGenServiceData: w = %A" w         
        
        let i =
            {
                msgClientId = w.partitionerId.messagingClientId

                msgSvcAccessInfo =
                    {
                        messagingServiceAddress = w.msgSvcAddress
                        messagingServicePort = w.msgSvcPort
                        messagingServiceName = messagingServiceName
                    }
            }

        let getMessageProcessorProxy (d : MessagingClientAccessInfo) =
            let i =
                {
                    messagingClientName = contGenServiceName.value.messagingClientName
                    storageType = clmConnectionString |> MsSqlDatabase
                }

            let messagingClientData =
                {
                    msgAccessInfo = d
                    messagingService = MsgResponseHandler d
                    msgClientProxy = MessagingClientProxy.create i d.msgClientId
                    expirationTime = MessagingClientData.defaultExpirationTime
                }

            let messagingClient = MessagingClient messagingClientData
            messagingClient.messageProcessorProxy

        let data =
            {
                modelRunnerData =
                    {
                        runnerData =
                            {
                                connectionString = clmConnectionString
                                minUsefulEe = MinUsefulEe.defaultValue
                                resultLocation = DefaultResultLocationFolder
                                earlyExitInfoOpt = Some EarlyExitInfo.defaultValue
                            }

                        runnerProxy =
                            {
                                getMessageProcessorProxy = getMessageProcessorProxy
                                createMessagingEventHandlers = createMessagingClientEventHandlers
                            }

                        messagingClientAccessInfo = i
                        logger = logger
                    }

                contGenServiceAccessInfo =
                    {
                        contGenServiceAddress = w.contGenSvcAddress
                        contGenServicePort = w.contGenSvcPort
                        contGenServiceName = contGenServiceName
                    }
            }

        match w.isValid() with
        | Ok() -> Ok data
        | Error e -> Error e

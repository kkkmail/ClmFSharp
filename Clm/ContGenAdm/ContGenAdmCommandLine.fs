﻿namespace ContGenAdm
open Argu
open Clm.Substances
open Clm.ModelParams
open System
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.Logging
open ClmSys.Registry
open ClmSys.VersionInfo
open Messaging.ServiceResponse
open ClmSys.PartitionerPrimitives
open ClmSys.MessagingData
open ClmSys.ContGenAdmData
open Messaging.Client
open ServiceProxy.MsgServiceProxy
open ContGen.ModelRunner
open ServiceProxy.ModelRunnerProxy
open DbData.Configuration
open ClmSys.ClmErrors

module AdmCommandLine =

    [<Literal>]
    let ContGenAdmAppName = "ContGenAdm.exe"


    [<CliPrefix(CliPrefix.Dash)>]
    type AddClmTaskArgs =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-i")>] IndexOfDefault of int64
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-n")>] NumberOfAminoAcids of int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-m")>] MaxPeptideLength of int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-y")>] TaskY0 of list<decimal>
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-t")>] TaskTEnd of list<decimal>
        | [<Unique>] [<AltCommandLine("-r")>]               Repetitions of int
        | [<Unique>] [<AltCommandLine("-g")>]               GenerateModelCode


        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | IndexOfDefault _ -> "index of default value in a map of defaults."
                | NumberOfAminoAcids _ -> "number of amino acids."
                | MaxPeptideLength _ -> "max peptide length."
                | TaskY0 _ -> "value of total y0."
                | TaskTEnd _ -> "value of tEnd."
                | Repetitions _ -> "number of repetitions."
                | GenerateModelCode -> "add in order to generate and save model code."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        RunModelArgs =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-m")>] ModelId of Guid
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-y")>] Y0 of decimal
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-t")>] TEnd of decimal

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | ModelId _ -> "id of the modelData to run."
                | Y0 _ -> "value of total y0."
                | TEnd _ -> "value of tEnd."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        MonitorArgs =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>] RefreshInterval of int

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | RefreshInterval _ -> "refresh inteval in seconds."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        CancelRunQueueArgs =
        | [<Unique>] [<AltCommandLine("-q")>] RunQueueIdToCancel of Guid
        | [<Unique>] [<AltCommandLine("-p")>] Partitioner of Guid
        | [<Unique>] [<AltCommandLine("-msgAddress")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] MsgSvcPort of int

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | RunQueueIdToCancel _ -> "RunQueueId to cancel."
                | Partitioner _ -> "messaging client id of a partitioner service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenAdmArguments =
            | [<Unique>] [<AltCommandLine("add")>]    AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("run")>]    RunModel of ParseResults<RunModelArgs>
            | [<Unique>] [<AltCommandLine("m")>]      Monitor of ParseResults<MonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]      CancelRunQueue of ParseResults<CancelRunQueueArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | AddClmTask _ -> "adds task / generates a single model."
                    | RunModel _ -> "runs a given model."
                    | Monitor _ -> "starts monitor."
                    | CancelRunQueue _ -> "tries to cancel run queue."


    let tryGetCommandLineParams (p :list<AddClmTaskArgs>) =
        let t = p |> List.tryPick (fun e -> match e with | TaskTEnd i -> Some i | _ -> None)
        let y = p |> List.tryPick (fun e -> match e with | TaskY0 i -> Some i | _ -> None)

        match t, y with
        | Some tl, Some yl ->
            match tl.Length = yl.Length with
            | true ->
                List.zip tl yl
                |> List.map (fun (tEnd, y0) ->
                                {
                                    tEnd = tEnd
                                    y0 = y0
                                    useAbundant = false
                                }
                            )
                |> Some
            | false ->
                printfn "Lists of t and y must have the same length!"
                None
        | _ -> None


    let tryGetNumberOfAminoAcids (p :list<AddClmTaskArgs>) =
        match p |> List.tryPick (fun e -> match e with | NumberOfAminoAcids n -> Some n | _ -> None) with
        | Some n -> NumberOfAminoAcids.tryCreate n
        | None -> NumberOfAminoAcids.defaultValue |> Some


    let tryGetMaxPeptideLength (p :list<AddClmTaskArgs>) =
        match p |> List.tryPick (fun e -> match e with | MaxPeptideLength n -> Some n | _ -> None) with
        | Some n -> MaxPeptideLength.tryCreate n
        | None -> MaxPeptideLength.defaultValue |> Some


    let tryGetClmDefaultValueId (p :list<AddClmTaskArgs>) =
        p |> List.tryPick (fun e -> match e with | IndexOfDefault i -> Some i | _ -> None) |> Option.bind (fun e -> e |> ClmDefaultValueId |> Some)


    let getNumberOrRepetitions (p :list<AddClmTaskArgs>) =
        match p |> List.tryPick (fun e -> match e with | Repetitions n -> Some n | _ -> None) with
        | Some n -> n
        | None -> 1


    let getGenerateModelCode (p :list<AddClmTaskArgs>) =
        match p |> List.tryPick (fun e -> match e with | GenerateModelCode -> Some true | _ -> None) with
        | Some n -> n
        | None -> false


    let tryGetModelId (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | ModelId i -> Some i | _ -> None) |> Option.bind (fun e -> e |> ModelDataId |> Some)
    let tryGetY0 (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | Y0 i -> Some i | _ -> None)
    let tryGetTEnd (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | TEnd i -> Some i | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetCancelRunQueueId p = p |> List.tryPick (fun e -> match e with | RunQueueIdToCancel e -> e |> RunQueueId |> Some | _ -> None)


    let getMsgServiceAddress = getMsgServiceAddressImpl tryGetMsgServiceAddress
    let getMsgServicePort = getMsgServicePortImpl tryGetMsgServicePort
    let getPartitioner = getPartitionerImpl tryGetPartitioner


    let tryCancelRunQueueImpl (logger : Logger) p =
        let result =
            match tryGetCancelRunQueueId p with
            | Some q ->
                let name = contGenServiceRegistryName
                let version = versionNumberValue

                let msgAddress = getMsgServiceAddress logger version name p
                let msgPort = getMsgServicePort logger version name p
                let partitioner = getPartitioner logger version name p

                let i =
                    {
                        contGenAdmId = ContGenAdmId.newId()
                        partitionerId = partitioner

                        messagingServiceAccessInfo =
                            {
                                messagingServiceAddress = msgAddress
                                messagingServicePort = msgPort
                                messagingServiceName = messagingServiceName
                            }
                    }

                let messagingClient =
                    {
                        msgAccessInfo = i.messagingClientAccessInfo
                        messagingService = MsgResponseHandler i.messagingClientAccessInfo
                        msgClientProxy = MessagingClientProxy.create { messagingClientName = contGenServiceName.value.messagingClientName }
                    }
                    |> MessagingClient

                match messagingClient.start() with
                | Ok() ->
                    let proxy = TryCancelRunQueueProxy.create clmConnectionString messagingClient.sendMessage
                    let r1 = tryCancelRunQueue proxy q
                    let r2 = messagingClient.transmitMessages()
                    combineUnitResults  r1 r2
                | Error e -> Error e
            | None -> Ok()

        match result with
        | Ok() -> Ok()
        | Error e ->
            logger.logError e
            Error e

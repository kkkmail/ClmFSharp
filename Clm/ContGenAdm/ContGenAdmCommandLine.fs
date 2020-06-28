namespace ContGenAdm

open Argu
open ClmSys.ClmErrors
open Clm.Substances
open Clm.ModelParams
open System
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.Logging
open ClmSys.GeneralData
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ContGenData
open ContGen.ContGenServiceResponse
open ContGenServiceInfo.ServiceInfo

module AdmCommandLine =

    [<Literal>]
    let ContGenAdmAppName = "ContGenAdm.exe"


    type RunData =
        {
            y0 : double
            tEnd : double
        }


    type ContGenAdmSettings =
        {
            contGenSettings : ContGenSettings

            defaultValueId : ClmDefaultValueId
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            runData : list<RunData>
            numberOfRepetitions : int
            generateModelCode : bool
        }


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
                | RefreshInterval _ -> "refresh interval in seconds."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        ModifyRunQueueArgs =
        | [<Unique>] [<AltCommandLine("-q")>] RunQueueIdToModify of Guid
        | [<Unique>] [<AltCommandLine("-c")>] CancelOrAbort of bool
        | [<Unique>] [<AltCommandLine("-r")>] ReportResults of bool
        | [<Unique>] [<AltCommandLine("-p")>] Partitioner of Guid
        | [<Unique>] [<AltCommandLine("-address")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | RunQueueIdToModify _ -> "RunQueueId to modify."
                | CancelOrAbort _ -> "if false then requests to cancel with results, if true then requests to abort calculations."
                | ReportResults _ -> "if false then requests results without charts, if true the requests results with charts."
                | Partitioner _ -> "messaging client id of a partitioner service."
                | SvcAddress _ -> "ContGen service ip address / name."
                | SvcPort _ -> "ContGen service port."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenAdmArguments =
            | [<Unique>] [<AltCommandLine("add")>]    AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("run")>]    RunModel of ParseResults<RunModelArgs>
            | [<Unique>] [<AltCommandLine("m")>]      Monitor of ParseResults<MonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]      ModifyRunQueue of ParseResults<ModifyRunQueueArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | AddClmTask _ -> "adds task / generates a single model."
                    | RunModel _ -> "runs a given model."
                    | Monitor _ -> "starts monitor."
                    | ModifyRunQueue _ -> "tries to modify run queue."


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
    let tryGetContGenServiceAddress p = p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> ContGenServiceAddress |> Some | _ -> None)
    let tryGetContGenServicePort p = p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> ContGenServicePort |> Some | _ -> None)
    let tryGetRunQueueIdToModify p = p |> List.tryPick (fun e -> match e with | RunQueueIdToModify e -> e |> RunQueueId |> Some | _ -> None)
    let getServiceAddress (w: ContGenSettings) p = tryGetContGenServiceAddress p |> Option.defaultValue w.contGenSvcAddress
    let getServicePort (w: ContGenSettings) p = tryGetContGenServicePort p |> Option.defaultValue w.contGenSvcPort
    let getPartitionerId (w: ContGenSettings) p = tryGetPartitioner p |> Option.defaultValue w.partitionerId


    let loadSettings p =
        let w = loadContGenSettings()

        let w1 =
            {
                contGenSvcAddress = getServiceAddress w p
                contGenSvcPort = getServicePort w p
                minUsefulEe = w.minUsefulEe // geMinUsefulEe w p
                msgSvcAddress = w.msgSvcAddress // getMsgServerAddress w p
                msgSvcPort = w.msgSvcPort // getMsgServerPort w p
                partitionerId = getPartitionerId w p
                lastAllowedNodeErr = w.lastAllowedNodeErr
            }

        printfn "loadSettings: w1 = %A" w1
        w1


    let getContGenServiceAccessInfo p =
        let w = loadSettings p

        {
            contGenServiceAddress = w.contGenSvcAddress
            contGenServicePort = w.contGenSvcPort
            contGenServiceName = contGenServiceName
        }


    let getCancellationTypeOpt p =
        p |> List.tryPick (fun e -> match e with | CancelOrAbort e -> (match e with | false -> CancelWithResults None | true -> AbortCalculation None) |> Some | _ -> None)


    let getResultNotificationTypeOpt p =
        p |> List.tryPick (fun e -> match e with | ReportResults e -> (match e with | false -> RegularChartGeneration | true -> ForceChartGeneration) |> Some | _ -> None)


    let private reportResult (logger : Logger) name r =
        match r with
        | Ok() ->
            printfn "%s: Successfully scheduled." name
            Ok()
        | Error e ->
            printfn "%s: Error %A" name e
            logger.logError e
            Error e


    let tryCancelRunQueueImpl (logger : Logger) p =
        match tryGetRunQueueIdToModify p, getCancellationTypeOpt p with
        | Some q, Some c ->
            let i = getContGenServiceAccessInfo p
            let h = ContGenResponseHandler i :> IContGenService
            h.tryCancelRunQueue q c |> reportResult logger "tryCancelRunQueueImpl"
        | _ -> Ok()


    let tryRequestResultsImpl (logger : Logger) p =
        match tryGetRunQueueIdToModify p, getResultNotificationTypeOpt p with
        | Some q, Some c ->
            let i = getContGenServiceAccessInfo p
            let h = ContGenResponseHandler i :> IContGenService
            h.tryRequestResults q c  |> reportResult logger "tryRequestResultsImpl"
        | _ -> Ok()


    let tryModifyRunQueueImpl l p =
        [
            tryRequestResultsImpl
            tryCancelRunQueueImpl
        ]
        |> List.map (fun e -> e l p)
        |> foldUnitResults

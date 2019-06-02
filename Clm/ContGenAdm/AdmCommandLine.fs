namespace ContGenAdm
open Argu
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open ClmSys.Retry
open Clm.ModelParams
open System

module AdmCommandLine =

    let x = 0


    [<Literal>]
    let ContGenAdmAppName = "ContGenAdm.exe"


    type
        [<CliPrefix(CliPrefix.Dash)>]
        AddClmTaskArgs =
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
        ConfigureServiceArgs =
            | [<Unique>] [<AltCommandLine("-c")>] NumberOfCores of int
            | [<Unique>] Start
            | [<Unique>] Stop
            | [<Unique>] [<AltCommandLine("-s")>] ShutDown of bool

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | NumberOfCores _ -> "number of logical cores to use."
                    | Start -> "starts generating models."
                    | Stop -> "stops generating models."
                    | ShutDown _ -> "shut down (pass true to wait for completion of all running processes)."

            member this.configParam =
                match this with
                | NumberOfCores n -> ContGenConfigParam.SetRunLimit n
                | Start -> SetToCanGenerate
                | Stop -> SetToIdle
                | ShutDown b -> RequestShutDown b


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenAdmArguments =
            | [<Unique>] [<AltCommandLine("add")>]    AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("run")>]    RunModel of ParseResults<RunModelArgs>
            | [<Unique>] [<AltCommandLine("m")>]      Monitor of ParseResults<MonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]      ConfigureService of ParseResults<ConfigureServiceArgs>
            | [<Unique>] [<AltCommandLine("server")>] ServerAddress of string
            | [<Unique>] [<AltCommandLine("port")>]   ServerPort of int

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | AddClmTask _ -> "adds task / generates a single model."
                    | RunModel _ -> "runs a given model."
                    | Monitor _ -> "starts monitor."
                    | ConfigureService _ -> "reconfigures service."
                    | ServerAddress _ -> "server address / name."
                    | ServerPort _ -> "server port."


    let tryGetServerAddress (p :list<ContGenAdmArguments>) =
         p |> List.tryPick (fun e -> match e with | ServerAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort (p :list<ContGenAdmArguments>) =
        p |> List.tryPick (fun e -> match e with | ServerPort p -> p |> ServicePort |> Some | _ -> None)


    let getServiceAccessInfo (p :list<ContGenAdmArguments>) =
        let address =
            match tryGetServerAddress p with
            | Some a -> a
            | None -> ServiceAddress.defaultValue

        let port =
            match tryGetServerPort p with
            | Some a -> a
            | None -> ServicePort.defaultValue

        {
            serviceAddress = address
            servicePort = port
        }


    let tryGetCommandLineParams i (p :list<AddClmTaskArgs>) =
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
                                    serviceAccessInfo = i
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


    let tryGetModelId (p :list<RunModelArgs>) =
        p |> List.tryPick (fun e -> match e with | ModelId i -> Some i | _ -> None) |> Option.bind (fun e -> e |> ModelDataId |> Some)


    let tryGetY0 (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | Y0 i -> Some i | _ -> None)
    let tryGetTEnd (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | TEnd i -> Some i | _ -> None)

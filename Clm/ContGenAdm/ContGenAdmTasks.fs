namespace ContGenAdm

open System.Threading
open Argu
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralData
open ClmSys.ExitErrorCodes
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open Runner
open ClmSys.Retry
open Clm.ModelParams
open System

module ContGenAdmTasks =

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
            | [<Unique>] [<AltCommandLine("server")>] ServerAddress of string
            | [<Unique>] [<AltCommandLine("port")>]   ServerPort of int
            | [<Unique>] [<AltCommandLine("add")>]    AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("run")>]    RunModel of ParseResults<RunModelArgs>
            | [<Unique>] [<AltCommandLine("m")>]      Monitor of ParseResults<MonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]      ConfigureService of ParseResults<ConfigureServiceArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | ServerAddress _ -> "server address/ name."
                    | ServerPort _ -> "server port."
                    | AddClmTask _ -> "adds task / generates a single model."
                    | RunModel _ -> "runs a given model."
                    | Monitor _ -> "starts monitor."
                    | ConfigureService _ -> "reconfigures service."

    let tryGetServerAddress (p :list<ContGenAdmArguments>) =
         p |> List.tryPick (fun e -> match e with | ServerAddress s -> s |> ServiceAddress |> Some | _ -> None)


    let tryGetServerPort (p :list<ContGenAdmArguments>) =
        p |> List.tryPick (fun e -> match e with | ServerPort p -> p |> ServicePort |> Some | _ -> None)


    let tryGetServiceAccessInfo (p :list<ContGenAdmArguments>) =
        match tryGetServerAddress p, tryGetServerPort p with
        | Some a, Some p ->
            {
                serviceAddress = a
                servicePort = p
            }
            |> Some
        | _ -> None


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


    let logError e = printfn "Error: %A" e
    let tryDbFun f = tryDbFun logError clmConnectionString f
    let tryLoadClmDefaultValue clmDefaultValueId = tryDbFun (tryLoadClmDefaultValue clmDefaultValueId) |> Option.bind id


    let addClmTask s (p :list<AddClmTaskArgs>) =
        let i = tryGetClmDefaultValueId p
        let n = tryGetNumberOfAminoAcids p
        let m = tryGetMaxPeptideLength p
        let c = tryGetCommandLineParams s p

        match i, n, m, c with
        | Some i, Some n, Some m, Some c ->
            printfn "Updating parameters. Using number of amino acids: %A, max peptide length: %A, index of default: %A." (n.length) (m.length) i
            match tryLoadClmDefaultValue i with
            | Some _ ->
                let r = getNumberOrRepetitions p

                let t =
                    {
                        clmTaskInfo =
                            {
                                clmTaskId = Guid.NewGuid() |> ClmTaskId
                                clmDefaultValueId = i
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = c
                        numberOfRepetitions = r
                        remainingRepetitions = r
                        createdOn = DateTime.Now
                    }

                let nt = addClmTask t clmConnectionString

                match getGenerateModelCode p with
                | true ->
                    printfn "Genetrating model..."
                    match c with
                    | [] ->
                        printfn "Cannot get service address and/or port."
                        ignore()
                    | h :: _ ->
                        let g = createOneTimeGenerator { ModelRunnerParam.defaultValue h.serviceAccessInfo with saveModelCode = true }
                        g nt |> ignore
                | false -> ignore()

                CompletedSuccessfully
            | None ->
                printfn "updateParameters: Cannot find data for default set index %A." i
                InvalidCommandLineArgs
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            InvalidCommandLineArgs


    let tryGetModelId (p :list<RunModelArgs>) =
        p |> List.tryPick (fun e -> match e with | ModelId i -> Some i | _ -> None) |> Option.bind (fun e -> e |> ModelDataId |> Some)


    let tryGetY0 (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | Y0 i -> Some i | _ -> None)
    let tryGetTEnd (p :list<RunModelArgs>) = p |> List.tryPick (fun e -> match e with | TEnd i -> Some i | _ -> None)


    let runModel (service : IContGenService) i (p :list<RunModelArgs>) =
        match tryGetModelId p, tryGetY0 p, tryGetTEnd p with
        | Some m, Some y, Some t ->
            let c =
                {
                    tEnd = t
                    y0 = y
                    useAbundant = false
                    serviceAccessInfo = i
                }

            service.runModel m c
            CompletedSuccessfully
        | _ ->
            printfn "Missing some command line arguments!"
            InvalidCommandLineArgs


    let monitor (service : IContGenService) (p :list<MonitorArgs>) =
        let i =
            match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            | Some i -> i * 1_000
            | None -> 30_000

        while true do
            try
                getServiceState service
            with
                | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(i)
        0


    let configureService (service : IContGenService) (p :list<ConfigureServiceArgs>) =
        try
            p
            |> List.map (fun e -> e.configParam |> service.configureService)
            |> ignore
            0
        with
            | e ->
                printfn "Exception: %A\n" e.Message
                -1


    let startGenerate (service : IContGenService) =
        try
            service.startGenerate()
            0
        with
            | e ->
                printfn "Exception: %A" e.Message
                -1


    type ContGenAdmTask =
        | AddClmTaskTask of list<AddClmTaskArgs>
        | RunModelTask of service : IContGenService * list<RunModelArgs>
        | MonitorTask of service : IContGenService * arguments : list<MonitorArgs>
        | ConfigureServiceTask of service : IContGenService * arguments : list<ConfigureServiceArgs>

        member task.run i =
            match task with
            | AddClmTaskTask p -> addClmTask i p
            | RunModelTask (s, p) -> runModel s i p
            | MonitorTask (s, p) -> monitor s p
            | ConfigureServiceTask (s, p) -> configureService s p

        static member private tryCreateUpdateParametersTask s p =
            p |> List.tryPick (fun e -> match e with | AddClmTask q -> q.GetAllResults() |> AddClmTaskTask |> Some | _ -> None)

        static member private tryCreateRunModelTask s p =
            p |> List.tryPick (fun e -> match e with | RunModel q -> (s, q.GetAllResults()) |> RunModelTask |> Some | _ -> None)

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | Monitor q -> (s, q.GetAllResults()) |> MonitorTask |> Some | _ -> None)

        static member private tryCreatConfigureServiceTask s p =
            p |> List.tryPick (fun e -> match e with | ConfigureService q -> (s, q.GetAllResults()) |> ConfigureServiceTask |> Some | _ -> None)

        static member tryCreate s p =
            [
                ContGenAdmTask.tryCreateUpdateParametersTask
                ContGenAdmTask.tryCreateRunModelTask
                ContGenAdmTask.tryCreatConfigureServiceTask
                ContGenAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)

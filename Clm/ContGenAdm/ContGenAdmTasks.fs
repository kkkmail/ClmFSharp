namespace ContGenAdm

open System.Threading
open ContGenServiceInfo.ServiceInfo
open ClmSys.ExitErrorCodes
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open Runner
open ClmSys.Retry
open Clm.ModelParams
open System
open ContGenAdm.AdmCommandLine
open ServiceProxy.Runner

module ContGenAdmTasks =

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
                        let g =
                            createOneTimeGenerator
                                {
                                    ModelRunnerParam.defaultValue h.serviceAccessInfo (LocalRunnerConfig.defaultValue |> LocalRunnerProxy |> RunnerProxy.create)
                                    with saveModelCode = true
                                }
                        g nt |> ignore
                | false -> ignore()

                CompletedSuccessfully
            | None ->
                printfn "updateParameters: Cannot find data for default set index %A." i
                InvalidCommandLineArgs
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            InvalidCommandLineArgs


    let runModel (service : IContGenService) i (p :list<RunModelArgs>) =
        match tryGetModelId p, tryGetY0 p, tryGetTEnd p with
        | Some m, Some y, Some t ->
            let c =
                {
                    taskParam =
                        {
                            tEnd = t
                            y0 = y
                            useAbundant = false
                        }

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

namespace ContGenAdm

open System.Threading
open ContGenServiceInfo.ServiceInfo
open ClmSys.ExitErrorCodes
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open ClmSys.Retry
open Clm.ModelParams
open System
open ContGenAdm.AdmCommandLine
open ClmSys.ContGenData
open ClmSys.ContGenPrimitives
open ContGen.ModelRunner
open ContGen.ModelGenerator
open ServiceProxy.ModelGeneratorProxy

module ContGenAdmTasks =

    let logError e =
        printfn "Error occurred: %A" e
        UnknownException


    let loadClmDefaultValue = loadClmDefaultValue clmConnectionString


    let addClmTask s (p :list<AddClmTaskArgs>) =
        let i = tryGetClmDefaultValueId p
        let n = tryGetNumberOfAminoAcids p
        let m = tryGetMaxPeptideLength p
        let c = tryGetCommandLineParams s p

        match i, n, m, c with
        | Some i, Some n, Some m, Some c ->
            printfn "Updating parameters. Using number of amino acids: %A, max peptide length: %A, index of default: %A." (n.length) (m.length) i
            match loadClmDefaultValue i with
            | Ok _ ->
                let r = getNumberOrRepetitions p

                let t =
                    {
                        clmTaskInfo =
                            {
                                clmTaskId = ClmTaskId.getNewId()
                                clmDefaultValueId = i
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = c
                        numberOfRepetitions = r
                        remainingRepetitions = r
                        createdOn = DateTime.Now
                    }

                match addClmTask clmConnectionString t with
                | Ok() ->
                    match getGenerateModelCode p with
                    | true ->
                        printfn "Genetrating model..."
                        let proxy = GenerateModelProxy.create clmConnectionString

                        match generateModel proxy t with
                        | Ok modelDataId ->
                            match generateModelCode proxy.generateModelCodeProxy modelDataId t with
                            | Ok _ -> CompletedSuccessfully
                            | Error e -> logError e
                        | Error e -> logError e
                    | false -> CompletedSuccessfully
                | Error e -> logError e
            | Error e ->
                printfn "updateParameters: Cannot find data for default set index %A, Error: %A" i e
                InvalidCommandLineArgs
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            InvalidCommandLineArgs


    //let runModel (service : IContGenService) i (p :list<RunModelArgs>) =
    //    match tryGetModelId p, tryGetY0 p, tryGetTEnd p with
    //    | Some m, Some y, Some t ->
    //        let c =
    //            {
    //                taskParam =
    //                    {
    //                        tEnd = t
    //                        y0 = y
    //                        useAbundant = false
    //                    }
    //
    //                serviceAccessInfo = i
    //            }
    //
    //        service.runModel m c
    //        CompletedSuccessfully
    //    | _ ->
    //        printfn "Missing some command line arguments!"
    //        InvalidCommandLineArgs


    let monitor (service : IContGenService) (p :list<MonitorArgs>) =
        let i =
            match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            | Some i -> i * 1_000
            | None -> 30_000

        let modelMonitor = ModelMonitor.create clmConnectionString

        while true do
            try
                getServiceState modelMonitor.getRunState |> ignore
            with
            | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(i)
        0


    //let configureService (service : IContGenService) (p :list<ConfigureServiceArgs>) =
    //    try
    //        p
    //        |> List.map (fun e -> e.configParam |> service.configureService)
    //        |> ignore
    //        0
    //    with
    //    | e ->
    //        printfn "Exception: %A\n" e.Message
    //        -1


    //let startGenerate (service : IContGenService) =
    //    try
    //        service.startGenerate()
    //        0
    //    with
    //    | e ->
    //        printfn "Exception: %A" e.Message
    //        -1


    type ContGenAdmTask =
        | AddClmTaskTask of list<AddClmTaskArgs>
        //| RunModelTask of service : IContGenService * list<RunModelArgs>
        | MonitorTask of service : IContGenService * arguments : list<MonitorArgs>
        //| ConfigureServiceTask of service : IContGenService * arguments : list<ConfigureServiceArgs>

        member task.run i =
            match task with
            | AddClmTaskTask p -> addClmTask i p
            //| RunModelTask (s, p) -> runModel s i p
            | MonitorTask (s, p) -> monitor s p
            //| ConfigureServiceTask (s, p) -> configureService s p

        static member private tryCreateUpdateParametersTask s p =
            p |> List.tryPick (fun e -> match e with | AddClmTask q -> q.GetAllResults() |> AddClmTaskTask |> Some | _ -> None)

        //static member private tryCreateRunModelTask s p =
        //    p |> List.tryPick (fun e -> match e with | RunModel q -> (s, q.GetAllResults()) |> RunModelTask |> Some | _ -> None)

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | Monitor q -> (s, q.GetAllResults()) |> MonitorTask |> Some | _ -> None)

        //static member private tryCreatConfigureServiceTask s p =
        //    p |> List.tryPick (fun e -> match e with | ConfigureService q -> (s, q.GetAllResults()) |> ConfigureServiceTask |> Some | _ -> None)

        static member tryCreate s p =
            [
                ContGenAdmTask.tryCreateUpdateParametersTask
                //ContGenAdmTask.tryCreateRunModelTask
                //ContGenAdmTask.tryCreatConfigureServiceTask
                ContGenAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)

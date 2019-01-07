namespace ContGen

open System.Data.SqlClient
open System.Threading
open Argu
open Clm.Substances
open Clm.Generator.DefaultValuesExt
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open AsyncRun
open Runner

module ContGenTasks =

    [<CliPrefix(CliPrefix.Dash)>]
    type RunContGenArgs =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-ql")>] MaxQueueLength of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MaxQueueLength _ -> "max queue length."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        UpdateParametersArgs =
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-n")>] NumberOfAminoAcids of int
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-m")>] MaxPeptideLength of int

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | NumberOfAminoAcids _ -> "number of amino acids."
                    | MaxPeptideLength _ -> "max peptide length."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        RequestShutDownArgs =
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-w")>] WaitForCompletion of bool

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | WaitForCompletion _ -> "wait for completion of already running tasks (this might take many days)."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenArguments =
            | [<Unique>] [<AltCommandLine("run")>]      RunContGen of ParseResults<RunContGenArgs>
            | [<Unique>] [<AltCommandLine("update")>]   UpdateParameters of ParseResults<UpdateParametersArgs>
            | [<Unique>] [<AltCommandLine("shutdown")>] RequestShutDown of ParseResults<RequestShutDownArgs>
            | [<Unique>] [<AltCommandLine("generate")>] GenerateModel

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | RunContGen _ -> "run Continuous Generation."
                    | UpdateParameters _ -> "update parameters."
                    | RequestShutDown _ -> "request shut down."
                    | GenerateModel -> "generate a single model."


    let runContGen (p :list<RunContGenArgs>) =
        let a = createRunner ModelRunnerParam.defaultValue
        a.startGenerate()

        while a.getState().shuttingDown |> not do
            Thread.Sleep(30000)
            let state = a.getState()
            printfn "a.getState() = %s" (state.ToString())
            if state.queue.Length = 0 then a.startGenerate()
        0


    let updateParameters (p :list<UpdateParametersArgs>) =
        let n =
            match p |> List.tryPick (fun e -> match e with | NumberOfAminoAcids n -> Some n | _ -> None) with
            | Some n -> NumberOfAminoAcids.tryCreate n
            | None -> NumberOfAminoAcids.defaultValue |> Some

        let m =
            match p |> List.tryPick (fun e -> match e with | MaxPeptideLength n -> Some n | _ -> None) with
            | Some n -> MaxPeptideLength.tryCreate n
            | None -> MaxPeptideLength.defaultValue |> Some

        match n, m with
        | Some n, Some m ->
            printfn "Updating parameters. Using number of amino acids: %A, max peptide length: %A." (n.length) (m.length)
            use conn = new SqlConnection(ClmConnectionString)
            openConnIfClosed conn
            saveDefaults conn n m |> ignore
            0
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length specified."
            -1


    /// TODO kk:20190107 - Implement.
    let requestShutDown (p :list<RequestShutDownArgs>) =
        printfn "requestShutDown is not implemented yet."
        -1


    /// TODO kk:20190107 - Implement.
    let generateModel () =
        printfn "generateModel is not implemented yet."
        -1


    type ContGenTask =
        | RunContGenTask of list<RunContGenArgs>
        | UpdateParametersTask of list<UpdateParametersArgs>
        | RequestShutDownTask of list<RequestShutDownArgs>
        | GenerateModelTask

        member task.run() =
            match task with
            | RunContGenTask p -> runContGen p
            | UpdateParametersTask p -> updateParameters p
            | RequestShutDownTask p -> requestShutDown p
            | GenerateModelTask -> generateModel ()

        static member private tryCreateRunContGenTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RunContGen q -> q.GetAllResults() |> RunContGenTask |> Some | _ -> None)

        static member private tryCreateUpdateParametersTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | UpdateParameters q -> q.GetAllResults() |> UpdateParametersTask |> Some | _ -> None)

        static member private tryCreateRequestShutDownTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RequestShutDown q -> q.GetAllResults() |> RequestShutDownTask |> Some | _ -> None)

        static member private tryCreateGenerateModelTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | GenerateModel -> GenerateModelTask |> Some | _ -> None)

        static member tryCreate (p : list<ContGenArguments>) =
            [
                ContGenTask.tryCreateRequestShutDownTask
                ContGenTask.tryCreateUpdateParametersTask
                ContGenTask.tryCreateGenerateModelTask
                ContGenTask.tryCreateRunContGenTask
            ]
            |> List.tryPick (fun e -> e p)


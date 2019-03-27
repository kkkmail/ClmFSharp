namespace ContGen

open System.Threading
open Argu
open ClmSys.ExitErrorCodes
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open Runner
open ClmSys.Retry

module ContGenTasks =
    open ClmDefaults
    open Clm.ModelParams
    open System

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
        AddClmTaskArgs =
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-i")>] IndexOfDefault of int64
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-n")>] NumberOfAminoAcids of int
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-m")>] MaxPeptideLength of int
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y")>] TaskY0 of decimal
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>] TaskTEnd of decimal
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>]               Repetitions of int


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


    and
        [<CliPrefix(CliPrefix.Dash)>]
        RunModelArgs =
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-i")>] ModelDataId of int
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y")>] Y0 of decimal
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>] TEnd of decimal

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | ModelDataId _ -> "id of the modelData to run."
                    | Y0 _ -> "value of total y0."
                    | TEnd _ -> "value of tEnd."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenArguments =
            | [<Unique>] [<AltCommandLine("run")>]      RunContGen of ParseResults<RunContGenArgs>
            | [<Unique>] [<AltCommandLine("add")>]   AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("generate")>] GenerateModel
            | [<Unique>] [<AltCommandLine("rm")>]       RunModel of ParseResults<RunModelArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | RunContGen _ -> "runs Continuous Generation."
                    | AddClmTask _ -> "adds task."
                    | GenerateModel -> "generates a single model."
                    | RunModel _ -> "runs a given model."


    let runContGen (p :list<RunContGenArgs>) =
        let a = createRunner ModelRunnerParam.defaultValue
        a.start()
        a.startGenerate()

        while a.getState().isShuttingDown |> not do
            Thread.Sleep(30000)
            let state = a.getState()
            printfn "a.getState() = %s" (state.ToString())
            if state.queue.Length = 0 then a.startGenerate()
        CompletedSuccessfully


    let tryGetCommandLineParams (p :list<AddClmTaskArgs>) =
        let t = p |> List.tryPick (fun e -> match e with | TaskTEnd i -> Some i | _ -> None)
        let y = p |> List.tryPick (fun e -> match e with | TaskY0 i -> Some i | _ -> None)

        match t, y with
        | Some tEnd, Some y0 ->
            {
                tEnd = tEnd
                y0 = y0
                useAbundant = false
            }
            |> Some
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


    let logError e = printfn "Error: %A" e
    let tryDbFun f = tryDbFun logError clmConnectionString f

    let tryLoadClmDefaultValue clmDefaultValueId =
        tryDbFun (tryLoadClmDefaultValue clmDefaultValueId)
        |> Option.bind id


    let addClmTask (p :list<AddClmTaskArgs>) =
        let i = tryGetClmDefaultValueId p
        let n = tryGetNumberOfAminoAcids p
        let m = tryGetMaxPeptideLength p
        let c = tryGetCommandLineParams p

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
                                clmTaskId = ClmTaskId -1L
                                clmDefaultValueId = i
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = [ c ]
                        numberOfRepetitions = r
                        remainingRepetitions = r
                        createdOn = DateTime.Now
                    }

                addClmTask t clmConnectionString |> ignore
                CompletedSuccessfully
            | None ->
                printfn "updateParameters: Cannot find data for default set index %A." i
                InvalidCommandLineArgs
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            InvalidCommandLineArgs


    let generateModel () =
        printfn "Genetrating and compiling model..."
        let g = createOneTimeGenerator { ModelRunnerParam.defaultValue with saveModelCode = true }
        g() |> ignore
        CompletedSuccessfully


    /// TODO kk:20190107 - Implement.
    let runModel (p :list<RunModelArgs>) =
        printfn "runModel is not implemented yet."
        NotImplemented


    type ContGenTask =
        | RunContGenTask of list<RunContGenArgs>
        | AddClmTaskTask of list<AddClmTaskArgs>
        | GenerateModelTask
        | RunModelTask of list<RunModelArgs>

        member task.run() =
            match task with
            | RunContGenTask p -> runContGen p
            | AddClmTaskTask p -> addClmTask p
            | GenerateModelTask -> generateModel ()
            | RunModelTask p -> runModel p

        static member private tryCreateRunContGenTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RunContGen q -> q.GetAllResults() |> RunContGenTask |> Some | _ -> None)

        static member private tryCreateUpdateParametersTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | AddClmTask q -> q.GetAllResults() |> AddClmTaskTask |> Some | _ -> None)

        static member private tryCreateGenerateModelTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | GenerateModel -> GenerateModelTask |> Some | _ -> None)

        static member private tryCreateRunModelTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RunModel q -> q.GetAllResults() |> RunModelTask |> Some | _ -> None)

        static member tryCreate (p : list<ContGenArguments>) =
            [
                ContGenTask.tryCreateUpdateParametersTask
                ContGenTask.tryCreateGenerateModelTask
                ContGenTask.tryCreateRunModelTask
                ContGenTask.tryCreateRunContGenTask
            ]
            |> List.tryPick (fun e -> e p)

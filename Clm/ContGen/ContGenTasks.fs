namespace ContGen

open Argu
open ClmSys.GeneralData
open ClmSys.ExitErrorCodes
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open ContGen.AsyncRun
open Runner
open ClmSys.Retry
open Clm.ModelParams
open System

module ContGenTasks =

    [<Literal>]
    let ContGenAppName = "ContGen.exe"


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
            | [<Mandatory>] [<Unique>] [<AltCommandLine("-m")>] ModelId of int64
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
        [<CliPrefix(CliPrefix.None)>]
        ContGenArguments =
            | [<Unique>] [<AltCommandLine("add")>]   AddClmTask of ParseResults<AddClmTaskArgs>
            | [<Unique>] [<AltCommandLine("run")>]       RunModel of ParseResults<RunModelArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | AddClmTask _ -> "adds task / generate a single model."
                    | RunModel _ -> "runs a given model."


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


    let logError e = printfn "Error: %A" e
    let tryDbFun f = tryDbFun logError clmConnectionString f
    let tryLoadClmDefaultValue clmDefaultValueId = tryDbFun (tryLoadClmDefaultValue clmDefaultValueId) |> Option.bind id


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
                        commandLineParams = c
                        numberOfRepetitions = r
                        remainingRepetitions = r
                        createdOn = DateTime.Now
                    }

                let nt = addClmTask t clmConnectionString

                match getGenerateModelCode p with
                | true ->
                    printfn "Genetrating model..."
                    let g = createOneTimeGenerator { ModelRunnerParam.defaultValue with saveModelCode = true }
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


    let runModel (p :list<RunModelArgs>) =
        match tryGetModelId p, tryGetY0 p, tryGetTEnd p with
        | Some m, Some y, Some t ->
            let p =
                {
                    notifyOnStarted = fun _ -> ()
                    calledBackModelId = m
                    runQueueId = RunQueueId 0L
                }

            let c =
                {
                    tEnd = t
                    y0 = y
                    useAbundant = false
                }

            runModel ModelRunnerParam.defaultValue.exeName c p |> ignore
            CompletedSuccessfully
        | _ ->
            printfn "Missing some command line arguments!"
            InvalidCommandLineArgs


    type ContGenTask =
        | AddClmTaskTask of list<AddClmTaskArgs>
        | RunModelTask of list<RunModelArgs>

        member task.run() =
            match task with
            | AddClmTaskTask p -> addClmTask p
            | RunModelTask p -> runModel p

        static member private tryCreateUpdateParametersTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | AddClmTask q -> q.GetAllResults() |> AddClmTaskTask |> Some | _ -> None)

        static member private tryCreateRunModelTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RunModel q -> q.GetAllResults() |> RunModelTask |> Some | _ -> None)

        static member tryCreate (p : list<ContGenArguments>) =
            [
                ContGenTask.tryCreateUpdateParametersTask
                ContGenTask.tryCreateRunModelTask
            ]
            |> List.tryPick (fun e -> e p)

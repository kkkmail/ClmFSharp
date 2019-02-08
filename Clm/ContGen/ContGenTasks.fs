﻿namespace ContGen

open System.Data.SqlClient
open System.Threading
open Argu
open ClmSys.ExitErrorCodes
open Clm.Substances
open ClmDefaults.DefaultValuesExt
open ClmDefaults.AllDefaults
open DbData.Configuration
open DbData.DatabaseTypes
open ContGen
open AsyncRun
open Runner

module ContGenTasks =
    open ClmDefaults

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
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-i")>] IndexOfDefault of int

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | NumberOfAminoAcids _ -> "number of amino acids."
                    | MaxPeptideLength _ -> "max peptide length."
                    | IndexOfDefault _ -> "0-based index of default value in an array of defaults."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        RunModelArgs =
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-i")>] ModelDataId of int
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y")>] Y0 of double
            | [<Mandatory>] [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>] TEnd of double

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
            | [<Unique>] [<AltCommandLine("update")>]   UpdateParameters of ParseResults<UpdateParametersArgs>
            | [<Unique>] [<AltCommandLine("generate")>] GenerateModel
            | [<Unique>] [<AltCommandLine("rm")>]       RunModel of ParseResults<RunModelArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | RunContGen _ -> "runs Continuous Generation."
                    | UpdateParameters _ -> "updates parameters."
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


    let updateParameters (p :list<UpdateParametersArgs>) =
        let n =
            match p |> List.tryPick (fun e -> match e with | NumberOfAminoAcids n -> Some n | _ -> None) with
            | Some n -> NumberOfAminoAcids.tryCreate n
            | None -> NumberOfAminoAcids.defaultValue |> Some

        let m =
            match p |> List.tryPick (fun e -> match e with | MaxPeptideLength n -> Some n | _ -> None) with
            | Some n -> MaxPeptideLength.tryCreate n
            | None -> MaxPeptideLength.defaultValue |> Some

        let i =
            match p |> List.tryPick (fun e -> match e with | IndexOfDefault i -> Some i | _ -> None) with
            | Some i ->
                if i >= 0 && i < AllDefaults.defaultValues.Length then Some i
                else None
            | None -> Some 0

        match i, n, m with
        | Some i, Some n, Some m ->
            printfn "Updating parameters. Using number of amino acids: %A, max peptide length: %A, index of default: %A." (n.length) (m.length) i
            saveDefaults clmConnectionString (AllDefaults.getDefaultValues i) n m |> ignore
            CompletedSuccessfully
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
        | UpdateParametersTask of list<UpdateParametersArgs>
        | GenerateModelTask
        | RunModelTask of list<RunModelArgs>

        member task.run() =
            match task with
            | RunContGenTask p -> runContGen p
            | UpdateParametersTask p -> updateParameters p
            | GenerateModelTask -> generateModel ()
            | RunModelTask p -> runModel p

        static member private tryCreateRunContGenTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | RunContGen q -> q.GetAllResults() |> RunContGenTask |> Some | _ -> None)

        static member private tryCreateUpdateParametersTask (p : list<ContGenArguments>) =
            p |> List.tryPick (fun e -> match e with | UpdateParameters q -> q.GetAllResults() |> UpdateParametersTask |> Some | _ -> None)

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

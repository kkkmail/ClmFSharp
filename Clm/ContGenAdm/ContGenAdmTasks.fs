namespace ContGenAdm

open System.Threading
open Argu
open ContGenServiceInfo.ServiceInfo
open ContGenAdm.ContGenServiceResponse

module ContGenAdmTasks =

    [<CliPrefix(CliPrefix.Dash)>]
    type MonitorArgs =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-r")>] RefreshInterval of int

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | RefreshInterval _ -> "refresh inteval in seconds."


    and
        [<CliPrefix(CliPrefix.Dash)>]
        ConfigureServiceArgs =
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-cores")>] NumberOfCores of int
            | [<Unique>] Start
            | [<Unique>] Stop

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | NumberOfCores _ -> "number of logical cores to use."
                    | Start -> "starts generating models."
                    | Stop -> "stops generating models."

            member this.configParam =
                match this with
                | NumberOfCores n -> ContGenConfigParam.SetRunLimit n
                | Start -> SetToCanGenerate
                | Stop -> SetToIdle


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenAdmArguments =
            | [<Unique>] [<AltCommandLine("m")>]      Monitor of ParseResults<MonitorArgs>
            | [<Unique>] [<AltCommandLine("c")>]       ConfigureService of ParseResults<ConfigureServiceArgs>

            // For debugging...
            | [<Unique>] [<AltCommandLine("s")>]    StartGenerate

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | Monitor _ -> "starts monitor."
                    | ConfigureService _ -> "reconfigures service."
                    | StartGenerate -> "starts continuos generation."


    let monitor (service : IContGenService) (p :list<MonitorArgs>) =
        while true do
            try
                printfn "Getting state..."
                let state = service.getState()
                printfn "...state = %A\n\n" state
            with
                | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(5_000)
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
        | MonitorTask of service : IContGenService * arguments : list<MonitorArgs>
        | ConfigureServiceTask of service : IContGenService * arguments : list<ConfigureServiceArgs>
        | StartGenerateTask of service : IContGenService

        member task.run() =
            match task with
            | MonitorTask (s, p) -> monitor s p
            | ConfigureServiceTask (s, p) -> configureService s p
            | StartGenerateTask s -> startGenerate s

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | Monitor q -> (s, q.GetAllResults()) |> MonitorTask |> Some | _ -> None)

        static member private tryCreatConfigureServiceTask s p =
            p |> List.tryPick (fun e -> match e with | ConfigureService q -> (s, q.GetAllResults()) |> ConfigureServiceTask |> Some | _ -> None)

        static member private tryCreateStartGenerateTask s p =
            p |> List.tryPick (fun e -> match e with | StartGenerate -> StartGenerateTask s |> Some | _ -> None)

        static member tryCreate s p =
            [
                ContGenAdmTask.tryCreatConfigureServiceTask
                ContGenAdmTask.tryCreateStartGenerateTask
                ContGenAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)

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
            | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-c")>] NumberOfCores of int
            | [<Unique>] [<AltCommandLine("-start")>] Start
            | [<Unique>] [<AltCommandLine("-stop")>] Stop

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


        //with
        //    interface IArgParserTemplate with
        //        member this.Usage =
        //            match this with
        //            | WaitForCompletion _ -> "wait for completion of already running tasks (this might take many days)."


    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenAdmArguments =
            | [<Unique>] [<AltCommandLine("run")>]      Monitor of ParseResults<MonitorArgs>
            //| [<Unique>] [<AltCommandLine("start")>]    StartGenerate
            //| [<Unique>] [<AltCommandLine("stop")>]     StopGenerate
            | [<Unique>] [<AltCommandLine("rm")>]       ConfigureService of ParseResults<ConfigureServiceArgs>

        with
            interface IArgParserTemplate with
                member this.Usage =
                    match this with
                    | Monitor _ -> "starts monitor."
                    //| StartGenerate -> "starts continuos generation."
                    //| StopGenerate -> "stops continuos generation."
                    | ConfigureService _ -> "reconfigures service."


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


    let startGenerate (service : IContGenService) =
        try
            service.startGenerate()
            0
        with
            | e ->
                printfn "Exception: %A" e.Message
                -1


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


    type ContGenAdmTask =
        | MonitorTask of service : IContGenService * arguments : list<MonitorArgs>
        | StartGenerateTask of service : IContGenService
        | ConfigureServiceTask of service : IContGenService * arguments : list<ConfigureServiceArgs>

        member task.run() =
            match task with
            | MonitorTask (s, p) -> monitor s p
            | StartGenerateTask s -> startGenerate s
            | ConfigureServiceTask (s, p) -> configureService s p

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | Monitor q -> (s, q.GetAllResults()) |> MonitorTask |> Some | _ -> None)

        static member private tryCreateStartGenerateTask s p =
            p |> List.tryPick (fun e -> match e with | StartGenerate -> StartGenerateTask s |> Some | _ -> None)

        static member private tryCreatConfigureServiceTask s p =
            p |> List.tryPick (fun e -> match e with | ConfigureService q -> (s, q.GetAllResults()) |> ConfigureServiceTask |> Some | _ -> None)

        static member tryCreate s p =
            [
                ContGenAdmTask.tryCreateStartGenerateTask
                ContGenAdmTask.tryCreatConfigureServiceTask
                ContGenAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)

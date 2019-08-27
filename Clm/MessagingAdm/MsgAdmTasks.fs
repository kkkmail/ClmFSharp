namespace MessagingAdm

open System.Threading
open MessagingServiceInfo.ServiceInfo
open MessagingAdm.AdmCommandLine

module MsgAdmTasks =

    let monitor (service : IMessagingService) p =
        let i =
            match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            | Some i -> i * 1_000
            | None -> 30_000

        while true do
            try
                let state = service.getState()
                ignore()
            with
                | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(i)
        0


    let configureService (service : IMessagingService) (p :list<ConfigureMsgServiceArgs>) =
        try
            p
            |> List.map (fun e -> e.configParam |> service.configureService)
            |> ignore
            0
        with
            | e ->
                printfn "Exception: %A\n" e.Message
                -1


    type MsgAdmTask =
        | MonitorTask of service : IMessagingService * arguments : list<MsgMonitorArgs>
        | ConfigureServiceTask of service : IMessagingService * arguments : list<ConfigureMsgServiceArgs>

        member task.run () =
            match task with
            | MonitorTask (s, p) -> monitor s p
            | ConfigureServiceTask (s, p) -> configureService s p

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | MonitorMsgService q -> (s, q.GetAllResults()) |> MonitorTask |> Some | _ -> None)

        static member private tryCreatConfigureServiceTask s p =
            p |> List.tryPick (fun e -> match e with | ConfigureMsgService q -> (s, q.GetAllResults()) |> ConfigureServiceTask |> Some | _ -> None)

        static member tryCreate s p =
            [
                MsgAdmTask.tryCreatConfigureServiceTask
                MsgAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)

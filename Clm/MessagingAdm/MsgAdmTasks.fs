﻿namespace MessagingAdm

open System.Threading
open MessagingServiceInfo.ServiceInfo
open MessagingAdm.AdmCommandLine
open System

module MsgAdmTasks =

    let monitorService (service : IMessagingService) =
        let i = 30_000

        while true do
            try
                printfn "Getting messaging service state..."
                let state = service.getState()
                printfn "State at %A is: %A\n" DateTime.Now state
            with
                | e -> printfn "Exception: %A\n" e.Message

            Thread.Sleep(i)

        ignore()


    let stopService (service : IMessagingService) = service.configureService (MsgWorkState ShuttingDown)
    let startService (service : IMessagingService) = service.configureService (MsgWorkState CanTransmitMessages)


    type MsgAdmTask =
        | MonitorMsgServiceTask of IMessagingService
        | StartMsgServiceTask of IMessagingService
        | StopMsgServiceTask of IMessagingService

        member task.run () =
            match task with
            | MonitorMsgServiceTask s -> monitorService s
            | StartMsgServiceTask s -> startService s
            | StopMsgServiceTask s -> stopService s

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | MonitorMsgService -> s |> MonitorMsgServiceTask |> Some | _ -> None)

        static member private tryCreatStopServiceTask s p =
            p |> List.tryPick (fun e -> match e with | StopMsgService -> s |> StopMsgServiceTask |> Some | _ -> None)

        static member private tryCreatStartServiceTask s p =
            p |> List.tryPick (fun e -> match e with | StartMsgService -> s |> StartMsgServiceTask |> Some | _ -> None)

        static member createTask s p =
            let tt =
                [
                    MsgAdmTask.tryCreatStopServiceTask
                    MsgAdmTask.tryCreatStartServiceTask
                    MsgAdmTask.tryCreateMonitorTask
                ]
                |> List.tryPick (fun e -> e s p)

            match tt with
            | Some t -> t
            | None -> MonitorMsgServiceTask s

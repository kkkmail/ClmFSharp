﻿namespace ClmSys

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open Argu

module ServiceInstaller =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    [<CliPrefix(CliPrefix.None)>]
    type SvcArguments<'A when 'A :> IArgParserTemplate> =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start of ParseResults<'A>
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<'A>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install service."
                | Uninstall -> "uninstall service."
                | Start _ -> "start service."
                | Stop -> "stop service."
                | Run _ -> "run service from command line without installing."


    /// https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let private getInstaller<'T> () =
        let installer = new AssemblyInstaller(typedefof<'T>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let private installService<'T> serviceName =
        try
            printfn "Attempting to install service %s ..." serviceName
            let i = getInstaller<'T> ()
            let d = new System.Collections.Hashtable()
            i.Install(d)
            i.Commit(d)
            printfn "... services installed successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to install services!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let private uninstallService<'T> serviceName =
        try
            printfn "Attempting to uninstall service %s ..." serviceName
            let i = getInstaller<'T> ()
            let d = new System.Collections.Hashtable()
            i.Uninstall(d)
            printfn "... services uninstalled successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to uninstall services!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let private startService s serviceName timeoutMilliseconds =
        try
            printfn "Attempting to start service %s ..." serviceName
            let service = new ServiceController(serviceName)
            let timeout = TimeSpan.FromMilliseconds (timeoutMilliseconds)

            service.Start ()
            service.WaitForStatus(ServiceControllerStatus.Running, timeout)
            printfn "... service %s started successfully.\n" serviceName
            true
        with
            | e ->
                printfn "FAILED to start service %s!" serviceName
                printfn "    Error message : %s\n" (e.Message)
                false


    let private stopService serviceName timeoutMilliseconds =
        try
            printfn "Attempting to stop service %s ..." serviceName
            let service = new ServiceController(serviceName)
            let timeout = TimeSpan.FromMilliseconds (timeoutMilliseconds)

            service.Stop ()
            service.WaitForStatus(ServiceControllerStatus.Stopped, timeout)
            printfn "... service %s stopped successfully.\n" serviceName
            true
        with
            | e ->
                printfn "FAILED to stop service %s!" serviceName
                printfn "    Error message : %s\n" (e.Message)
                false


    let private runService r =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        r logger
        waitHandle.WaitOne() |> ignore
        true


    type ServiceTask<'T, 'S, 'R, 'A when 'A :> IArgParserTemplate> =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask of 'S
        | StopServiceTask
        | RunServiceTask of 'R

        member task.run serviceName =
            match task with
            | InstallServiceTask -> installService<'T> serviceName
            | UninstallServiceTask ->
                match stopService serviceName ServiceTmeOut with
                | true -> printfn "Successfully stopped service %s." serviceName
                | false -> printfn "Failed to stop service %s! Proceeding with uninstall anyway." serviceName

                uninstallService<'T> serviceName
            | StartServiceTask s -> startService s serviceName ServiceTmeOut
            | StopServiceTask -> stopService serviceName ServiceTmeOut
            | RunServiceTask r -> runService r

        static member private tryCreateInstallServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'S, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'S, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask s (p : list<SvcArguments<'A>>) :ServiceTask<'T, 'S, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Start p -> s p |> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'S, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask r (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'S, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Start p -> r p |> RunServiceTask |> Some | _ -> None)

        static member tryCreate s r p : ServiceTask<'T, 'S, 'R, 'A> option =
            [
                ServiceTask.tryCreateUninstallServiceTask
                ServiceTask.tryCreateInstallServiceTask
                ServiceTask.tryCreateStopServiceTask
                ServiceTask.tryCreateStartServiceTask s
                ServiceTask.tryCreateRunServiceTask r
            ]
            |> List.tryPick (fun e -> e p)

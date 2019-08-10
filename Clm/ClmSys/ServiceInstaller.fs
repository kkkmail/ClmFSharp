namespace ClmSys

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open Argu
open ClmSys.Logging

module ServiceInstaller =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    type ServiceName =
        | ServiceName of string

        member this.value = let (ServiceName v) = this in v


    type ServiceInfo<'R> =
        {
            serviceName : ServiceName
            runService : Logger -> 'R -> unit
            timeoutMilliseconds : int option
            logger : Logger
        }

        member this.timeout =
            match this.timeoutMilliseconds with
            | Some t -> float t
            | None -> ServiceTmeOut
            |> TimeSpan.FromMilliseconds


    [<CliPrefix(CliPrefix.None)>]
    type SvcArguments<'A when 'A :> IArgParserTemplate> =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<'A>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save

        static member fromArgu c a : list<SvcArguments<'A>> = a |> List.map (fun e -> c e)


    /// https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let private getInstaller<'T> () =
        let installer = new AssemblyInstaller(typedefof<'T>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let private installService<'T> (l : Logger) (ServiceName serviceName) =
        try
            l.logInfo (sprintf "Attempting to install service %s ..." serviceName)
            let i = getInstaller<'T> ()
            let d = new System.Collections.Hashtable()
            i.Install(d)
            i.Commit(d)
            l.logInfo "... services installed successfully.\n"
            true
        with
            | e ->
                l.logErr "FAILED to install services!"
                l.logErr (sprintf "    Error message : %s\n" e.Message)
                false


    let private uninstallService<'T>  (l : Logger) (ServiceName serviceName) =
        try
            l.logInfo (sprintf "Attempting to uninstall service %s ..." serviceName)
            let i = getInstaller<'T> ()
            let d = new System.Collections.Hashtable()
            i.Uninstall(d)
            l.logInfo "... services uninstalled successfully.\n"
            true
        with
            | e -> 
                l.logErr "FAILED to uninstall services!"
                l.logErr (sprintf "    Error message : %s\n" (e.Message))
                false


    let private startService (i : ServiceInfo<'R>) =
        try
            i.logger.logInfo (sprintf "Attempting to start service %s ..." i.serviceName.value)
            let service = new ServiceController(i.serviceName.value)
            service.Start ()
            service.WaitForStatus(ServiceControllerStatus.Running, i.timeout)
            i.logger.logInfo (sprintf "... service %s started successfully.\n" i.serviceName.value)
            true
        with
            | e ->
                i.logger.logErr (sprintf "FAILED to start service %s!" i.serviceName.value)
                i.logger.logErr (sprintf "    Error message : %s\n" (e.Message))
                false


    let private stopService (i : ServiceInfo<'R>) =
        try
            i.logger.logInfo (sprintf "Attempting to stop service %s ..." i.serviceName.value)
            let service = new ServiceController(i.serviceName.value)
            service.Stop ()
            service.WaitForStatus(ServiceControllerStatus.Stopped, i.timeout)
            i.logger.logInfo (sprintf "... service %s stopped successfully.\n" i.serviceName.value)
            true
        with
            | e ->
                i.logger.logErr (sprintf "FAILED to stop service %s!" i.serviceName.value)
                i.logger.logErr (sprintf "    Error message : %s\n" (e.Message))
                false


    let private runService (i : ServiceInfo<'R>) r =
        i.logger.logInfo "Starting..."
        let waitHandle = new ManualResetEvent(false)
        i.runService i.logger r
        waitHandle.WaitOne() |> ignore
        true


    type ServiceTask<'T, 'R, 'A when 'A :> IArgParserTemplate> =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask
        | StopServiceTask
        | RunServiceTask of 'R

        member task.run (i : ServiceInfo<'R>) =
            match task with
            | InstallServiceTask -> installService<'T> i.logger i.serviceName
            | UninstallServiceTask ->
                match stopService i with
                | true -> i.logger.logInfo (sprintf "Successfully stopped service %s." i.serviceName.value)
                | false -> i.logger.logInfo (sprintf "Failed to stop service %s! Proceeding with uninstall anyway." i.serviceName.value)

                uninstallService<'T> i.logger i.serviceName
            | StartServiceTask -> startService i
            | StopServiceTask -> stopService i
            | RunServiceTask r -> runService i r

        static member private tryCreateInstallServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask (p : list<SvcArguments<'A>>) :ServiceTask<'T, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Start -> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask r (p : list<SvcArguments<'A>>) : ServiceTask<'T, 'R, 'A> option =
            p |> List.tryPick (fun e -> match e with | Run p -> r p |> RunServiceTask |> Some | _ -> None)

        static member tryCreate r p : ServiceTask<'T, 'R, 'A> option =
            [
                ServiceTask.tryCreateUninstallServiceTask
                ServiceTask.tryCreateInstallServiceTask
                ServiceTask.tryCreateStopServiceTask
                ServiceTask.tryCreateStartServiceTask
                ServiceTask.tryCreateRunServiceTask r
            ]
            |> List.tryPick (fun e -> e p)

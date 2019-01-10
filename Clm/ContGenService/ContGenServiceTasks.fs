namespace ContGenService

open System
open System.Configuration.Install
open System.ServiceProcess
open ServiceInfo.ServiceConfiguration
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenService.ContGenServiceInfo

module ContGenServiceTasks =

    [<Literal>]
    let ServiceTmeOut = 10000.0


    // https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller () =
        let installer = new AssemblyInstaller(typedefof<ContGenWindowsService>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let installServices () =
        try
            printfn "Attempting to install services %s, %s ..." ContGenServiceName ProgressNotifierServiceName
            let i = getInstaller ()
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


    let uninstallServices () =
        try
            printfn "Attempting to uninstall services %s, %s ..." ContGenServiceName ProgressNotifierServiceName
            let i = getInstaller ()
            let d = new System.Collections.Hashtable()
            i.Uninstall(d)
            printfn "... services uninstalled successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to uninstall services!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let startService serviceName timeoutMilliseconds =
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

    let startServices timeoutMilliseconds =
        (startService ProgressNotifierServiceName timeoutMilliseconds) &&
        (startService ContGenServiceName timeoutMilliseconds)


    let stopService serviceName timeoutMilliseconds =
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


    let stopServices timeoutMilliseconds =
        (stopService ProgressNotifierServiceName timeoutMilliseconds) &&
        (stopService ContGenServiceName timeoutMilliseconds)


    type ContGenServiceTask =
        | InstallServicesTask
        | UninstallServicesTask
        | StartServicesTask
        | StopServicesTask

        member task.run() =
            match task with
            | InstallServicesTask ->
                if installServices () then startServices ServiceTmeOut |> ignore
            | UninstallServicesTask ->
                stopServices ServiceTmeOut |> ignore
                uninstallServices () |> ignore
            | StartServicesTask -> startServices ServiceTmeOut |> ignore
            | StopServicesTask -> stopServices ServiceTmeOut |> ignore

        static member private tryCreateInstallServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServicesTask |> Some | _ -> None)

        static member private tryCreateUninstallServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServicesTask |> Some | _ -> None)

        static member private tryCreateStartServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start -> StartServicesTask |> Some | _ -> None)

        static member private tryCreateStopServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServicesTask |> Some | _ -> None)

        static member tryCreate (p : list<SvcArguments>) =
            [
                ContGenServiceTask.tryCreateUninstallServicesTask
                ContGenServiceTask.tryCreateInstallServicesTask
                ContGenServiceTask.tryCreateStopServicesTask
                ContGenServiceTask.tryCreateStartServicesTask
            ]
            |> List.tryPick (fun e -> e p)

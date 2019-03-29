namespace ContGenService

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open ContGenAdm.ContGenServiceResponse

module ContGenServiceTasks =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    // https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller () =
        let installer = new AssemblyInstaller(typedefof<ContGenWindowsService>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let installServices () =
        try
            printfn "Attempting to install service %s ..." ContGenServiceName
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
            printfn "Attempting to uninstall service %s ..." ContGenServiceName
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
        (stopService ContGenServiceName timeoutMilliseconds)


    let runService () =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)

        let logger e = printfn "Error: %A" e
        startServiceRun logger
        let service = (new ContGenResponseHandler()).contGenService

        let eventHandler _ =
            printfn "Getting state..."
            let state = service.getState()
            printfn "...state =\n%s\n\n" (state.ToString())
            if state.queue.Length = 0 then service.startGenerate()

        let timer = new System.Timers.Timer(30_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()
        eventHandler 0

        waitHandle.WaitOne() |> ignore

        0


    type ContGenServiceTask =
        | InstallServicesTask
        | UninstallServicesTask
        | StartServicesTask
        | StopServicesTask
        | RunServicesTask

        member task.run() =
            match task with
            | InstallServicesTask ->
                if installServices () then startServices ServiceTmeOut |> ignore
            | UninstallServicesTask ->
                stopServices ServiceTmeOut |> ignore
                uninstallServices () |> ignore
            | StartServicesTask -> startServices ServiceTmeOut |> ignore
            | StopServicesTask -> stopServices ServiceTmeOut |> ignore
            | RunServicesTask -> runService () |> ignore

        static member private tryCreateInstallServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServicesTask |> Some | _ -> None)

        static member private tryCreateUninstallServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServicesTask |> Some | _ -> None)

        static member private tryCreateStartServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start -> StartServicesTask |> Some | _ -> None)

        static member private tryCreateStopServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServicesTask |> Some | _ -> None)

        static member private tryCreateRunServicesTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Run -> RunServicesTask |> Some | _ -> None)

        static member tryCreate (p : list<SvcArguments>) =
            [
                ContGenServiceTask.tryCreateUninstallServicesTask
                ContGenServiceTask.tryCreateInstallServicesTask
                ContGenServiceTask.tryCreateStopServicesTask
                ContGenServiceTask.tryCreateStartServicesTask
                ContGenServiceTask.tryCreateRunServicesTask
            ]
            |> List.tryPick (fun e -> e p)

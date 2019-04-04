namespace ContGenService

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open ContGenAdm.ContGenServiceResponse
open ClmSys.ExitErrorCodes
open Argu


module ContGenServiceTasks =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    type ContGenServiceRunParam =
        | NumberOfCores of int

        // TODO kk:20190404 - Implement and propagate the parameter all the way through to the service.
        static member fromParseResults (p : ParseResults<RunArgs>) : list<ContGenServiceRunParam> =
            []


    // https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller () =
        let installer = new AssemblyInstaller(typedefof<ContGenWindowsService>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let installService () =
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


    let uninstallService () =
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


    let startContGenService timeoutMilliseconds (p : list<ContGenServiceRunParam>) =
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


    let stopContGenService timeoutMilliseconds =
        (stopService ContGenServiceName timeoutMilliseconds)


    let runService (p : list<ContGenServiceRunParam>)=
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        startServiceRun logger
        let service = (new ContGenResponseHandler()).contGenService
        let eventHandler _ = getServiceState service

        let timer = new System.Timers.Timer(30_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()

        eventHandler 0
        waitHandle.WaitOne() |> ignore
        CompletedSuccessfully


    type ContGenServiceTask =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask of list<ContGenServiceRunParam>
        | StopServiceTask
        | RunServiceTask of list<ContGenServiceRunParam>

        member task.run() =
            match task with
            | InstallServiceTask ->
                if installService() then startContGenService ServiceTmeOut |> ignore
            | UninstallServiceTask ->
                stopContGenService ServiceTmeOut |> ignore
                uninstallService() |> ignore
            | StartServiceTask p -> startContGenService ServiceTmeOut p |> ignore
            | StopServiceTask -> stopContGenService ServiceTmeOut |> ignore
            | RunServiceTask p -> runService p |> ignore

        static member private tryCreateInstallServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start p -> ContGenServiceRunParam.fromParseResults p |> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Run p -> ContGenServiceRunParam.fromParseResults p |> RunServiceTask |> Some | _ -> None)

        static member tryCreate (p : list<SvcArguments>) =
            [
                ContGenServiceTask.tryCreateUninstallServiceTask
                ContGenServiceTask.tryCreateInstallServiceTask
                ContGenServiceTask.tryCreateStopServiceTask
                ContGenServiceTask.tryCreateStartServiceTask
                ContGenServiceTask.tryCreateRunServiceTask
            ]
            |> List.tryPick (fun e -> e p)

namespace ContGenService

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open Argu

open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open ContGenAdm.ContGenServiceResponse
open ClmSys.ExitErrorCodes
open ClmSys.GeneralData

module ContGenServiceTasks =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    type ContGenConfigParam
        with
        static member fromParseResults (p : ParseResults<RunArgs>) : list<ContGenConfigParam> =
            [
                p.TryGetResult NumberOfCores |> Option.bind (fun c -> SetRunLimit c |> Some)
                p.TryGetResult RunIdle |> Option.bind (fun _ -> Some SetToIdle)
            ]
            |> List.choose id


    /// https://stackoverflow.com/questions/31081879/writing-a-service-in-f
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

    /// TODO kk:20190520 - Propagate p into service.
    let startContGenService timeoutMilliseconds (p : list<ContGenConfigParam>) =
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


    let runService i (p : list<ContGenConfigParam>) =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        startServiceRun i logger
        let service = (new ContGenResponseHandler(i)).contGenService
        p |> List.map (fun e -> service.configureService e) |> ignore
        service.loadQueue()
        let eventHandler _ = getServiceState service

        let timer = new System.Timers.Timer(30_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()

        waitHandle.WaitOne() |> ignore
        CompletedSuccessfully


    /// TODO kk:20190601 - Propagage address / port into the service installation call.
    type ContGenServiceTask =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask of list<ContGenConfigParam>
        | StopServiceTask
        | RunServiceTask of list<ContGenConfigParam> * ServiceAccessInfo

        member task.run() =
            match task with
            | InstallServiceTask ->
                if installService() then startContGenService ServiceTmeOut |> ignore
            | UninstallServiceTask ->
                stopContGenService ServiceTmeOut |> ignore
                uninstallService() |> ignore
            | StartServiceTask p -> startContGenService ServiceTmeOut p |> ignore
            | StopServiceTask -> stopContGenService ServiceTmeOut |> ignore
            | RunServiceTask (p, i) -> runService i p |> ignore

        static member private tryCreateInstallServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start p -> ContGenConfigParam.fromParseResults p |> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask (p : list<SvcArguments>) =
            p |> List.tryPick (fun e ->
                                match e with
                                | Run p ->
                                    match tryGetServiceAccessInfo (p.GetAllResults()) with
                                    | Some i ->
                                        RunServiceTask(ContGenConfigParam.fromParseResults p, i) |> Some
                                    | None ->
                                        printfn "Cannot obtain service address and/or port from command line parameters."
                                        None
                                | _ -> None)

        static member tryCreate (p : list<SvcArguments>) =
            [
                ContGenServiceTask.tryCreateUninstallServiceTask
                ContGenServiceTask.tryCreateInstallServiceTask
                ContGenServiceTask.tryCreateStopServiceTask
                ContGenServiceTask.tryCreateStartServiceTask
                ContGenServiceTask.tryCreateRunServiceTask
            ]
            |> List.tryPick (fun e -> e p)

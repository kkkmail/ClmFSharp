﻿namespace MessagingService

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open Argu

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.MessagingData
open MessagingService.SvcCommandLine
open MessagingService.WindowsService
open MessagingServiceInfo.ServiceInfo
//open ContGenAdm.ContGenServiceResponse

module ServiceTasks =

    [<Literal>]
    let ServiceTmeOut = 10_000.0


    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
                //p.TryGetResult NumberOfCores |> Option.bind (fun c -> SetRunLimit c |> Some)
                //p.TryGetResult RunIdle |> Option.bind (fun _ -> Some SetToIdle)
                //p.TryGetResult MinimumUsefulEe |> Option.bind (fun ee -> ee |> SetMinUsefulEe |> Some)
            ]
            |> List.choose id


    /// https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller () =
        let installer = new AssemblyInstaller(typedefof<MessagingWindowsService>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let installService () =
        try
            printfn "Attempting to install service %s ..." MessagingServiceName
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
            printfn "Attempting to uninstall service %s ..." MessagingServiceName
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
    let startContGenService timeoutMilliseconds (p : list<MessagingConfigParam>) =
        (startService MessagingServiceName timeoutMilliseconds)


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
        (stopService MessagingServiceName timeoutMilliseconds)


    let runService i (p : list<MessagingConfigParam>) =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        startServiceRun i logger
        waitHandle.WaitOne() |> ignore
        true


    /// TODO kk:20190601 - Propagage address / port into the service installation call.
    type MessagingServiceTask =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask of list<MessagingConfigParam>
        | StopServiceTask
        | RunServiceTask of list<MessagingConfigParam> * MessagingServiceAccessInfo

        member task.run() =
            match task with
            | InstallServiceTask -> installService()
            | UninstallServiceTask ->
                match stopContGenService ServiceTmeOut with
                | true -> printfn "Successfully stopped service."
                | false -> printfn "Failed to stop service! Proceeding with uninstall anyway."

                uninstallService()
            | StartServiceTask p -> startContGenService ServiceTmeOut p
            | StopServiceTask -> stopContGenService ServiceTmeOut
            | RunServiceTask (p, i) -> runService i p

        static member private tryCreateInstallServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start p -> MessagingConfigParam.fromParseResults p |> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e ->
                                match e with
                                | Run p ->
                                    let i = getServiceAccessInfo (p.GetAllResults())
                                    RunServiceTask(MessagingConfigParam.fromParseResults p, i) |> Some
                                | _ -> None)

        static member tryCreate (p : list<MsgSvcArguments>) =
            [
                MessagingServiceTask.tryCreateUninstallServiceTask
                MessagingServiceTask.tryCreateInstallServiceTask
                MessagingServiceTask.tryCreateStopServiceTask
                MessagingServiceTask.tryCreateStartServiceTask
                MessagingServiceTask.tryCreateRunServiceTask
            ]
            |> List.tryPick (fun e -> e p)
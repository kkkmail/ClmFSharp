namespace ContGenService

open System
open System.Configuration
open System.Configuration.Install
open System.Collections.Generic
open System.Linq
open System.ServiceProcess
open System.Text

open ServiceInfo.ServiceConfiguration
open ContGenService.ServiceImplementation
open ContGenService.WindowsService
open ServiceInfo.ServiceConfiguration
open ContGenService.ContGenServiceInfo

module Program =

    // https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller serviceName =
        let installer =
            match serviceName with
            | ProgressNotifierServiceName -> 
                //new AssemblyInstaller(typedefof<ProgressNotifierWindowsService>.Assembly, null);
                new AssemblyInstaller(typedefof<ProgressNotifierWindowsService>.Assembly, [| "/name=" + serviceName |]);
            | ContGenServiceName -> 
                new AssemblyInstaller(typedefof<ContGenWindowsService>.Assembly, [| "/name=" + serviceName |]);
            | _ -> failwith "Invalid name specified."
        installer.UseNewContext <- true
        installer


    let installService serviceName =
        try
            printfn "Attempting to install service %s ..." serviceName
            let i = getInstaller serviceName
            let d = new System.Collections.Hashtable()
            i.Install(d)
            i.Commit(d)
            printfn "... service installed successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to install service!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let uninstallService serviceName =
        try
            printfn "Attempting to uninstall service %s ..." serviceName
            let i = getInstaller serviceName
            let d = new System.Collections.Hashtable()
            i.Uninstall(d)
            printfn "... service uninstalled successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to uninstall service!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let startService serviceName timeoutMilliseconds =
        try
            printfn "Attempting to start service %s ..." serviceName
            let service : ServiceController = new ServiceController(serviceName)
            let timeout = TimeSpan.FromMilliseconds (timeoutMilliseconds)

            service.Start ()
            service.WaitForStatus(ServiceControllerStatus.Running, timeout)
            printfn "... service started successfully.\n"
            true
        with
            | e ->
                printfn "FAILED to start service!"
                printfn "    Error message : %s\n" (e.Message)
                false

    let stopService serviceName timeoutMilliseconds =
        try
            printfn "Attempting to stop service %s ..." serviceName
            let service : ServiceController = new ServiceController(serviceName)
            let timeout = TimeSpan.FromMilliseconds (timeoutMilliseconds)

            service.Stop ()
            service.WaitForStatus(ServiceControllerStatus.Stopped, timeout)
            printfn "... service stopped successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to stop service!"
                printfn "    Error message : %s\n" (e.Message)
                false


    [<EntryPoint>]
    let main (args : string[]) : int = 
        let timeOut = 10000.0

        let usage () : unit = 
            let msg : string = 
                (if args.Length > 1 then ("Unrecognized param(s): " + (sprintf "%A" args) + "\n") else "") +
                "Correct params are:\n" + 
                "    -i[nstall]\n" + 
                "        to install and start the service.\n" + 
                "    -u[ninstall]\n" + 
                "        to try stopping and then uninstall the service.\n" + 
                "    -start\n" + 
                "        to try starting the service.\n" + 
                "    -stop\n" + 
                "        to try stopping the service."

            printfn "%s" msg

        match (args |> Seq.length) with
        | 1 -> 
            match (args.[0].ToLower()) with
            |"-install"   | "-i" -> 
                if installService ProgressNotifierServiceName then startService ProgressNotifierServiceName timeOut |> ignore
                //if installService ContGenServiceName then startService ContGenServiceName timeOut |> ignore
            |"-uninstall" | "-u" -> 
                stopService ProgressNotifierServiceName timeOut |> ignore // Service may be already stopped
                stopService ContGenServiceName timeOut |> ignore // Service may be already stopped

                uninstallService ProgressNotifierServiceName |> ignore
                //uninstallService ContGenServiceName |> ignore
            |"-start" ->
                startService ProgressNotifierServiceName timeOut |> ignore
                startService ContGenServiceName timeOut |> ignore
            |"-stop" -> 
                stopService ProgressNotifierServiceName timeOut |> ignore
                stopService ContGenServiceName timeOut |> ignore
            |_-> usage ()
        | _ -> 
            ServiceBase.Run
                [|
                    new ProgressNotifierWindowsService() :> ServiceBase
                    new ContGenWindowsService() :> ServiceBase
                |]

        0

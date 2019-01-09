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

module Program =

    // https://stackoverflow.com/questions/31081879/writing-a-service-in-f
    let getInstaller () : AssemblyInstaller =
        let installer : AssemblyInstaller = new AssemblyInstaller(typedefof<ContGenWindowsService>.Assembly, null);
        installer.UseNewContext <- true
        installer


    let installService () : bool =
        try
            printfn "Attempting to install service %s ..." ServiceName
            let i : AssemblyInstaller = getInstaller()
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


    let uninstallService () : bool =
        try
            printfn "Attempting to uninstall service %s ..." ServiceName
            let i = getInstaller()
            let d = new System.Collections.Hashtable()
            i.Uninstall(d)
            printfn "... service uninstalled successfully.\n"
            true
        with
            | e -> 
                printfn "FAILED to uninstall service!"
                printfn "    Error message : %s\n" (e.Message)
                false


    let startService (timeoutMilliseconds : float) : bool = 
        try
            printfn "Attempting to start service %s ..." ServiceName
            let service : ServiceController = new ServiceController(ServiceName)
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

    let stopService (timeoutMilliseconds : float) : bool = 
        try
            printfn "Attempting to stop service %s ..." ServiceName
            let service : ServiceController = new ServiceController(ServiceName)
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
                if installService () then startService timeOut |> ignore
            |"-uninstall" | "-u" -> 
                stopService timeOut |> ignore // Service may be already stopped
                uninstallService () |> ignore
            |"-start" -> 
                startService timeOut |> ignore
            |"-stop" -> 
                stopService timeOut |> ignore
            |_-> usage ()
        | _ -> ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]

        0

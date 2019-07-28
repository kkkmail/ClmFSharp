namespace ContGenServMessagingServerice

open System.Configuration.Install
open System.ComponentModel
open System.ServiceProcess

open ContGenServiceInfo.ServiceInfo
open ContGenService.WindowsService

[<RunInstaller(true)>]
type ContGenServiceInstaller() =
    inherit Installer()

    do
        // Specify properties of the hosting process.
        new ServiceProcessInstaller(Account = ServiceAccount.LocalSystem)
        |> base.Installers.Add |> ignore

        // Specify properties of the service running inside the process.
        new ServiceInstaller
          ( DisplayName = ContGenServiceName,
            ServiceName = ContGenServiceName,
            StartType = ServiceStartMode.Automatic )
        |> base.Installers.Add |> ignore


// Run the services when the process starts.
module Main =
    ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]

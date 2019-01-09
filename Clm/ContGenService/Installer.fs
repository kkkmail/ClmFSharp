namespace ContGenService

open System
open System.Configuration
open System.Configuration.Install
open System.ComponentModel
open System.Linq
open System.ServiceProcess

open ServiceInfo.ServiceConfiguration
open ContGenService.ServiceImplementation
open ContGenService.WindowsService

[<RunInstaller(true)>]
type ContGenServiceInstaller() =
    inherit Installer()
    do 
        // Specify properties of the hosting process
        new ServiceProcessInstaller
          (Account = ServiceAccount.LocalSystem)
        |> base.Installers.Add |> ignore

        // Specify properties of the service running inside the process
        new ServiceInstaller
          ( DisplayName = ServiceName,
            ServiceName = ServiceName,
            StartType = ServiceStartMode.Automatic )
        |> base.Installers.Add |> ignore

// Run the service when the process starts
module Main =
    ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]

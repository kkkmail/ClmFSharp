namespace WorkerNodeService

open Argu
open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.WindowsService
open ClmSys.WorkerNodePrimitives

module ServiceTasks =

    let serviceInfo =
        {
            serviceName = workerNodeServiceName.value
            runService = startWrkNodeWcfServiceRun
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<WorkerNodeServiceRunArgs>) =
        match getServiceAccessInfo (p.GetAllResults()) with
        | Ok r -> r
        | Error e -> failwith (sprintf "Critical error: %A" e)
        
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = ServiceTask<WorkerNodeWindowsService, WorkerNodeServiceInfo, WorkerNodeServiceRunArgs>

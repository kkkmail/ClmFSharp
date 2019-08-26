namespace WorkerNodeService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.WindowsService
open WorkerNodeServiceInfo.ServiceInfo
open ProgressNotifierClient.ServiceResponse
open ClmSys.TimerEvents

module ServiceTasks =

    let runService l i =
        printfn "runService: Trying to create WorkerNodeResponseHandler..."
        startServiceRun l i
        let service = (new WorkerNodeResponseHandler(i)).workerNodeService

        try
            printfn "runService: Calling: service.ping()..."
            service.ping()
        with
        | ex -> printfn "Exception occurred: %A" ex

        let h = new EventHandler(EventHandlerInfo.defaultValue service.ping)
        do h.start()


    let serviceInfo =
        {
            serviceName = ServiceName WorkerNodeServiceName
            runService = runService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = ServiceTask<WorkerNodeWindowsService, WorkerNodeServiceAccessInfo, WorkerNodeServiceRunArgs>
